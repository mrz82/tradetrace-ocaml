open Types

(* P&L attribution for the trading desk.
   Two views:
     calculate         — unrealized P&L per open symbol against mark prices
     correction_impact — before/after cost basis delta when a correction arrives

   This answers the core operations question every TDOE asks at end of day:
   "what did that price correction actually do to our P&L?" *)

(* Per-symbol P&L breakdown for open (non-flat) positions *)
type position_pnl = {
  symbol           : symbol;
  net_qty          : int;    (* positive = long, negative = short *)
  avg_entry_cents  : int;    (* weighted average entry price, rounded toward zero *)
  unrealized_cents : int;    (* 0 when symbol absent from mark_prices *)
}

type summary = {
  by_symbol              : position_pnl StringMap.t;
  total_unrealized_cents : int;
}

(* How much a specific correction moves cost basis.
   before/after are the cash-flow contribution of the execution (negative = cost for buys,
   positive = proceeds for sells) in the state immediately before and after this correction.
   delta_cents is a cost-basis change, NOT realized profit — the position may still be open.
   delta_cents > 0 means the correction improved the books (cost fell or proceeds rose). *)
type correction_impact = {
  correction_id : string;
  exec_id       : string;
  symbol        : symbol;
  before_cents  : int;   (* cash flow sign: negative = cost (buy), positive = proceeds (sell) *)
  after_cents   : int;
  delta_cents   : int;   (* cost basis change — not realized P&L *)
}

(* Build P&L summary from resolved (post-correction) executions.
   Symbols with net_qty = 0 (fully closed positions) are excluded from by_symbol. *)
let calculate (state : Replay.replay_state) ~(mark_prices : int StringMap.t) : summary =
  (* First pass: accumulate (net_qty, net_cost) per symbol.
     Sign convention: Buy → positive qty and positive cost; Sell → negative both.
     This keeps the unrealized formula uniform: net_qty * mark - net_cost. *)
  let acc =
    StringMap.fold (fun exec_id _ acc ->
      match Replay.resolved_execution state exec_id with
      | None -> acc
      | Some e ->
        let sign = match e.side with Buy -> 1 | Sell -> -1 in
        let (prev_qty, prev_cost) =
          match StringMap.find_opt e.symbol acc with
          | None        -> (0, 0)
          | Some (q, c) -> (q, c)
        in
        StringMap.add e.symbol
          (prev_qty + sign * e.qty,
           prev_cost + sign * e.qty * e.price) acc
    ) state.executions StringMap.empty
  in
  let by_symbol =
    StringMap.fold (fun sym (net_qty, net_cost) m ->
      if net_qty = 0 then m
      else
        (* avg_entry uses integer division; off by at most $0.01 on display only.
           The unrealized calculation itself is exact: net_qty * mark - net_cost. *)
        let avg_entry_cents = net_cost / net_qty in
        let unrealized_cents =
          match StringMap.find_opt sym mark_prices with
          | None      -> 0
          | Some mark -> net_qty * mark - net_cost
        in
        StringMap.add sym { symbol = sym; net_qty; avg_entry_cents; unrealized_cents } m
    ) acc StringMap.empty
  in
  let total_unrealized_cents =
    StringMap.fold (fun _ p acc -> acc + p.unrealized_cents) by_symbol 0
  in
  { by_symbol; total_unrealized_cents }

(* Impact of a specific correction on cost basis.
   Returns None if the correction_id is unknown or its execution cannot be found.
   Only accepted corrections appear in state.correction_ids, so rejected corrections
   correctly return None — they had no effect on the books. *)
let correction_impact (state : Replay.replay_state) ~(correction_id : string) : correction_impact option =
  if not (StringMap.mem correction_id state.correction_ids) then None
  else
    (* corrections map is keyed by exec_id, not correction_id, so we search linearly.
       We carry the full chain so we can determine the predecessor state below. *)
    let found =
      StringMap.fold (fun exec_id cs found ->
        match found with
        | Some _ -> found
        | None   ->
          match List.find_opt (fun (c : Types.correction) -> c.correction_id = correction_id) cs with
          | None      -> None
          | Some corr -> Some (exec_id, corr, cs)
      ) state.corrections None
    in
    match found with
    | None -> None
    | Some (exec_id, corr, chain) ->
      match StringMap.find_opt exec_id state.executions with
      | None      -> None
      | Some orig ->
        (* Determine what was in effect immediately before this correction arrived.
           'chain' is in descending seq order (newest first).  The predecessor is
           the next element in the list (the correction that arrived just before ours),
           or the original execution if ours is the first correction ever applied. *)
        let rec find_before_state (cs : Types.correction list) =
          match cs with
          | [] | [_]                   -> (orig.qty, orig.price)
          | c :: ((prev :: _) as rest) ->
            if c.correction_id = correction_id
            then (prev.corrected_qty, prev.corrected_price)
            else find_before_state rest
        in
        let (before_qty, before_price) = find_before_state chain in
        let sign = match orig.side with Buy -> -1 | Sell -> 1 in
        let before_cents = sign * before_qty          * before_price in
        let after_cents  = sign * corr.corrected_qty  * corr.corrected_price in
        Some {
          correction_id;
          exec_id;
          symbol      = orig.symbol;
          before_cents;
          after_cents;
          delta_cents = after_cents - before_cents;
        }
