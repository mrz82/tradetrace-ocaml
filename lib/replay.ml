open Types

(* Deterministic event replay engine.

   The same event log always produces the same state regardless of input order:
   events are sorted by (seq, kind, id) before folding.  This makes replay safe
   against out-of-order message delivery, which is routine in practice whenever
   feeds reconnect or messages are redelivered.

   Every validation failure is non-fatal — it lands in state.errors and replay
   continues.  The desk needs to see the complete set of problems in a batch,
   not just the first one that tripped a guard. *)

(* Per-order fill summary, computed from resolved (corrected) execution quantities *)
type fill_status = {
  ordered_qty    : qty;
  cum_filled_qty : qty;
  open_qty       : qty;
  exec_ids       : string list;  (* exec IDs in ascending seq order *)
}

type replay_state = {
  orders          : order          StringMap.t;
  executions      : execution      StringMap.t;
  (* correction list is in descending seq order (newest first).
     Prepend on every new correction so List.hd is always the effective one
     in O(1) — the common case is "what is the current corrected price?",
     not "give me the full history."  The full chain is still there for audit. *)
  corrections     : correction list StringMap.t;
  correction_ids  : unit StringMap.t;  (* global dedup set for correction IDs *)
  order_fills     : string list StringMap.t;  (* order_id -> exec_ids, ascending seq *)
  cancellations   : cancellation   StringMap.t;
  broker_confirms : broker_confirm StringMap.t;
  errors          : string list;
}

let empty_state = {
  orders          = StringMap.empty;
  executions      = StringMap.empty;
  corrections     = StringMap.empty;
  correction_ids  = StringMap.empty;
  order_fills     = StringMap.empty;
  cancellations   = StringMap.empty;
  broker_confirms = StringMap.empty;
  errors          = [];
}

let add_error msg s = { s with errors = msg :: s.errors }

(* Return the execution with the latest effective correction applied.
   The head of the corrections list has the highest seq (most recently prepended). *)
let resolved_execution state exec_id =
  match StringMap.find_opt exec_id state.executions with
  | None -> None
  | Some exec ->
    match StringMap.find_opt exec_id state.corrections with
    | None | Some [] -> Some exec
    | Some (latest :: _) ->
      Some { exec with qty   = latest.corrected_qty;
                       price = latest.corrected_price }

(* Cumulative fill for an order using resolved quantities.
   Because corrections are processed in seq order before later executions,
   a corrected-down fill creates room that a subsequent fill can legitimately use. *)
let cum_fill_resolved state order_id =
  match StringMap.find_opt order_id state.order_fills with
  | None -> 0
  | Some ids ->
    List.fold_left (fun acc exec_id ->
      match resolved_execution state exec_id with
      | None   -> acc
      | Some e -> acc + e.qty
    ) 0 ids

let apply_event state event =
  match event with
  | OrderEvent o ->
    if StringMap.mem o.order_id state.orders then
      add_error (Printf.sprintf "duplicate order %s" o.order_id) state
    else if o.qty <= 0 then
      add_error (Printf.sprintf "order %s has non-positive qty %d"
                   o.order_id o.qty) state
    else
      (match o.limit_price with
       | Some p when p <= 0 ->
         add_error (Printf.sprintf "order %s has non-positive limit price %d"
                      o.order_id p) state
       | _ ->
         { state with orders = StringMap.add o.order_id o state.orders })

  | ExecutionEvent e ->
    if StringMap.mem e.exec_id state.executions then
      add_error (Printf.sprintf "duplicate execution %s" e.exec_id) state
    else if e.qty <= 0 then
      add_error (Printf.sprintf "execution %s has non-positive qty %d"
                   e.exec_id e.qty) state
    else if e.price <= 0 then
      add_error (Printf.sprintf "execution %s has non-positive price %d"
                   e.exec_id e.price) state
    else
      (match StringMap.find_opt e.order_id state.orders with
       | None ->
         add_error (Printf.sprintf "execution %s references unknown order %s"
                      e.exec_id e.order_id) state
       | Some _ when StringMap.mem e.order_id state.cancellations ->
         add_error (Printf.sprintf "execution %s on cancelled order %s"
                      e.exec_id e.order_id) state
       | Some order when e.symbol <> order.symbol ->
         add_error (Printf.sprintf
           "execution %s symbol %s does not match order %s symbol %s"
           e.exec_id e.symbol e.order_id order.symbol) state
       | Some order when e.side <> order.side ->
         add_error (Printf.sprintf
           "execution %s side %s does not match order %s side %s"
           e.exec_id (pp_side e.side) e.order_id (pp_side order.side)) state
       | Some order ->
         (* Limit price: reject the fill outright, don't just flag it.
            A buy above the limit or sell below it shouldn't be possible —
            either the order data is stale or the fill report has a typo.
            Either way the desk needs to investigate, not carry a silent
            bad trade into the position blotter. *)
         let violates_limit =
           match order.limit_price with
           | None -> false
           | Some lim ->
             (match e.side with
              | Buy  -> e.price > lim
              | Sell -> e.price < lim)
         in
         if violates_limit then
           add_error (Printf.sprintf
             "execution %s price %s violates %s limit %s on order %s"
             e.exec_id (pp_price e.price) (pp_side e.side)
             (pp_price (Option.get order.limit_price)) e.order_id) state
         else
           let cum = cum_fill_resolved state e.order_id in
           if cum + e.qty > order.qty then
             add_error (Printf.sprintf
               "overfill on order %s: cumulative %d + new %d exceeds ordered %d (exec %s rejected)"
               e.order_id cum e.qty order.qty e.exec_id) state
           else
             let prev = match StringMap.find_opt e.order_id state.order_fills with
                        | None -> [] | Some ids -> ids in
             { state with
               executions  = StringMap.add e.exec_id e state.executions;
               order_fills = StringMap.add e.order_id (prev @ [e.exec_id]) state.order_fills })

  | ConfirmEvent c ->
    if StringMap.mem c.exec_id state.broker_confirms then
      add_error (Printf.sprintf "duplicate broker confirm for exec %s" c.exec_id) state
    else if c.qty <= 0 then
      add_error (Printf.sprintf
        "broker confirm for exec %s has non-positive qty %d" c.exec_id c.qty) state
    else if c.price <= 0 then
      add_error (Printf.sprintf
        "broker confirm for exec %s has non-positive price %d" c.exec_id c.price) state
    else
      { state with broker_confirms = StringMap.add c.exec_id c state.broker_confirms }

  | CancelEvent c ->
    if not (StringMap.mem c.order_id state.orders) then
      add_error (Printf.sprintf "cancel %s references unknown order %s"
                   c.cancel_id c.order_id) state
    else
      { state with cancellations = StringMap.add c.order_id c state.cancellations }

  | CorrectionEvent c ->
    if StringMap.mem c.correction_id state.correction_ids then
      add_error (Printf.sprintf "duplicate correction id %s" c.correction_id) state
    else if c.corrected_qty <= 0 then
      (* Zero corrected_qty is not allowed.  It might seem natural as "void this
         fill," but a correction says "the fill happened, the data was wrong."
         A void/bust says "the fill did not happen."  Conflating them makes audit
         trails ambiguous — exactly what you don't want during a P&L dispute. *)
      add_error (Printf.sprintf
        "correction %s has non-positive corrected_qty %d \
         (must be > 0; to void a fill, use a reversal)"
        c.correction_id c.corrected_qty) state
    else if c.corrected_price <= 0 then
      add_error (Printf.sprintf
        "correction %s has non-positive corrected_price %d"
        c.correction_id c.corrected_price) state
    else if not (StringMap.mem c.original_exec_id state.executions) then
      add_error (Printf.sprintf "correction %s references unknown execution %s"
                   c.correction_id c.original_exec_id) state
    else
      let exec      = StringMap.find c.original_exec_id state.executions in
      let order_opt = StringMap.find_opt exec.order_id state.orders in
      (* Check that corrected_qty would not push cumulative fill past order qty.
         Replace the current resolved qty of the target exec with corrected_qty,
         then verify the resulting cumulative fill still fits. *)
      let would_overfill = match order_opt with
        | None -> false
        | Some order ->
          let cur_qty = match resolved_execution state c.original_exec_id with
                        | None -> 0 | Some e -> e.qty in
          let other_cum = cum_fill_resolved state exec.order_id - cur_qty in
          other_cum + c.corrected_qty > order.qty
      in
      (* Corrected price must also respect the original order's limit. *)
      let violates_limit = match order_opt with
        | None -> false
        | Some order ->
          (match order.limit_price with
           | None -> false
           | Some lim ->
             (match exec.side with
              | Buy  -> c.corrected_price > lim
              | Sell -> c.corrected_price < lim))
      in
      if would_overfill then
        add_error (Printf.sprintf
          "correction %s would overfill order %s: corrected qty %d rejected"
          c.correction_id exec.order_id c.corrected_qty) state
      else if violates_limit then
        add_error (Printf.sprintf
          "correction %s corrected price %s violates %s limit on order %s"
          c.correction_id (pp_price c.corrected_price) (pp_side exec.side) exec.order_id) state
      else
        let prev = match StringMap.find_opt c.original_exec_id state.corrections with
                   | None -> [] | Some cs -> cs in
        { state with
          (* prepend so List.hd is always the latest correction *)
          corrections    = StringMap.add c.original_exec_id (c :: prev) state.corrections;
          correction_ids = StringMap.add c.correction_id () state.correction_ids }

(* stable_sort preserves the relative order of equal-seq events as a secondary
   guarantee; compare_events already provides a fully deterministic total order
   via the (seq, kind_rank, event_id) compound key. *)
let replay events =
  let sorted = List.stable_sort compare_events events in
  List.fold_left apply_event empty_state sorted

(* Replay only events that had arrived by seq_cutoff.
   Enables bitemporal queries: "what did the system know at sequence N?" *)
let replay_until seq_cutoff events =
  List.filter (fun e -> event_seq e <= seq_cutoff) events |> replay

(* Fill summary for one order using resolved (corrected) quantities.
   open_qty can be negative if corrections pushed cumulative fill above ordered_qty. *)
let order_fill_status state order_id =
  match StringMap.find_opt order_id state.orders with
  | None -> None
  | Some order ->
    let exec_ids = match StringMap.find_opt order_id state.order_fills with
                   | None -> [] | Some ids -> ids in
    let cum_filled_qty = List.fold_left (fun acc exec_id ->
      match resolved_execution state exec_id with
      | None   -> acc
      | Some e -> acc + e.qty
    ) 0 exec_ids in
    Some {
      ordered_qty    = order.qty;
      cum_filled_qty;
      open_qty       = order.qty - cum_filled_qty;
      exec_ids;
    }
