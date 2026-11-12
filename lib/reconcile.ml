open Types

(* Reconciliation: compare our internal execution records against what the
   broker reported.  One status per execution, not per order — a single order
   can have multiple partial fills, and the broker confirms each one separately.
   Results are sorted by exec_id so the output is stable across runs. *)

type status =
  | Matched
  | PriceMismatch         of { internal : price;  broker : price  }
  | QtyMismatch           of { internal : qty;    broker : qty    }
  | BothMismatch          of { internal_price : price; broker_price : price;
                                internal_qty   : qty;   broker_qty   : qty   }
  (* Broker confirm references the right exec_id but reports a different symbol or side.
     These are checked before qty/price because a wrong symbol or side indicates
     a fundamentally misrouted confirmation, not just a data discrepancy. *)
  | ConfirmSymbolMismatch of { internal : symbol; broker : symbol }
  | ConfirmSideMismatch   of { internal : side;   broker : side   }
  | Unconfirmed   (* we have an execution; broker sent nothing *)
  | Unexpected    (* broker sent a confirm; we have no matching execution *)

type item = {
  exec_id : string;
  status  : status;
}

(* Reconcile all executions (post-correction) against broker confirms *)
let run (state : Replay.replay_state) =
  let from_executions =
    StringMap.fold (fun exec_id _ acc ->
      match Replay.resolved_execution state exec_id with
      | None -> acc
      | Some exec ->
        let status =
          match StringMap.find_opt exec_id state.broker_confirms with
          | None -> Unconfirmed
          | Some conf ->
            (* Symbol and side are checked before qty/price.
               A confirm with the right exec_id but wrong symbol or side is
               likely a routing error — the broker associated the wrong fill
               with this ID.  That's categorically different from a price
               discrepancy and needs a different kind of investigation. *)
            if exec.symbol <> conf.symbol then
              ConfirmSymbolMismatch { internal = exec.symbol; broker = conf.symbol }
            else if exec.side <> conf.side then
              ConfirmSideMismatch { internal = exec.side; broker = conf.side }
            else
              let p_ok = exec.price = conf.price in
              let q_ok = exec.qty   = conf.qty   in
              (match p_ok, q_ok with
               | true,  true  -> Matched
               | true,  false -> QtyMismatch   { internal = exec.qty;   broker = conf.qty }
               | false, true  -> PriceMismatch { internal = exec.price; broker = conf.price }
               | false, false -> BothMismatch  { internal_price = exec.price; broker_price = conf.price;
                                                  internal_qty   = exec.qty;   broker_qty   = conf.qty })
        in
        { exec_id; status } :: acc
    ) state.executions []
  in
  let unexpected =
    StringMap.fold (fun exec_id _ acc ->
      if StringMap.mem exec_id state.executions then acc
      else { exec_id; status = Unexpected } :: acc
    ) state.broker_confirms []
  in
  List.sort (fun a b -> String.compare a.exec_id b.exec_id)
    (from_executions @ unexpected)

let pp_status = function
  | Matched           -> "MATCHED"
  | Unconfirmed       -> "UNCONFIRMED"
  | Unexpected        -> "UNEXPECTED"
  | PriceMismatch m   ->
    Printf.sprintf "PRICE_MISMATCH (internal=%s  broker=%s)"
      (pp_price m.internal) (pp_price m.broker)
  | QtyMismatch m     ->
    Printf.sprintf "QTY_MISMATCH (internal=%d  broker=%d)"
      m.internal m.broker
  | BothMismatch m    ->
    Printf.sprintf "BOTH_MISMATCH (internal=%s@%d  broker=%s@%d)"
      (pp_price m.internal_price) m.internal_qty
      (pp_price m.broker_price)   m.broker_qty
  | ConfirmSymbolMismatch m ->
    Printf.sprintf "CONFIRM_SYMBOL_MISMATCH (internal=%s  broker=%s)"
      m.internal m.broker
  | ConfirmSideMismatch m ->
    Printf.sprintf "CONFIRM_SIDE_MISMATCH (internal=%s  broker=%s)"
      (pp_side m.internal) (pp_side m.broker)
