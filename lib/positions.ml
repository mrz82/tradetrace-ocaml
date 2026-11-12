open Types

(* Portfolio positions derived from resolved (post-correction) executions.
   Two views are exposed:
     calculate        — full replay, all corrections applied
     calculate_at t   — same but filtered to trades with trade_time ≤ t
   Compose with Replay.replay_until for the full bitemporal matrix:
     replay_until seq_cutoff |> calculate_at trade_cutoff *)

type position = {
  symbol  : symbol;
  net_qty : int;    (* positive = long, negative = short *)
}

type portfolio = {
  positions  : position StringMap.t;
  cash_cents : int;   (* net cash in cents; buys spend, sells earn *)
}

let empty = { positions = StringMap.empty; cash_cents = 0 }

let apply_fill pf sym side qty price =
  (* Buys add to position and drain cash; sells reduce position and add cash.
     Remove a symbol from the map when net_qty reaches zero — a flat position
     is not the same as an absent one for display purposes, but it simplifies
     any downstream code that iterates the positions map. *)
  let sign = match side with Buy -> 1 | Sell -> -1 in
  let prev = match StringMap.find_opt sym pf.positions with
             | None   -> 0
             | Some p -> p.net_qty in
  let nqty = prev + sign * qty in
  let positions =
    if nqty = 0 then StringMap.remove sym pf.positions
    else StringMap.add sym { symbol = sym; net_qty = nqty } pf.positions
  in
  let cash_delta = match side with
    | Buy  -> -(qty * price)
    | Sell ->   qty * price
  in
  { positions; cash_cents = pf.cash_cents + cash_delta }

(* Current portfolio: all resolved executions *)
let calculate (state : Replay.replay_state) =
  StringMap.fold (fun exec_id _ pf ->
    match Replay.resolved_execution state exec_id with
    | None   -> pf
    | Some e -> apply_fill pf e.symbol e.side e.qty e.price
  ) state.executions empty

(* Bitemporal query: positions including only trades with trade_time <= cutoff.
   Compose with Replay.replay_until for full bitemporal access:
     replay_until seq_cutoff events |> calculate_at trade_cutoff
   The trade_time field is from the original execution and is not affected by corrections. *)
let calculate_at trade_cutoff (state : Replay.replay_state) =
  StringMap.fold (fun exec_id _ pf ->
    match Replay.resolved_execution state exec_id with
    | None   -> pf
    | Some e ->
      if e.trade_time <= trade_cutoff
      then apply_fill pf e.symbol e.side e.qty e.price
      else pf
  ) state.executions empty
