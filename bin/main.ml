open Tradetrace
open Types

(* ------------------------------------------------------------------ *)
(* Synthetic scenario
   ORD-001  AAPL  Buy  100  filled correctly                 → MATCHED
   ORD-002  MSFT  Buy   50  broker price differs             → PRICE_MISMATCH
   ORD-003  GOOG  Sell  10  no broker confirm yet            → UNCONFIRMED
   ORD-004  AMZN  Buy  100  partial fill; price correction   → MATCHED after correction
   (none)   TSLA  Sell   5  broker surprise confirm          → UNEXPECTED

   Three timestamps on each event:
     trade_time   — when the exchange executed the trade
     arrival_time — when our system received the message
     seq          — monotonic ordering key assigned by the message bus

   The correction on EX-004 (seq=20) arrives before the broker confirms
   (seq=30+), and after the execution (seq=13) — out-of-order relative to
   trade_time, but deterministic because replay sorts by seq first.
*)

let events = [
  (* Orders *)
  OrderEvent { order_id = "ORD-001"; symbol = "AAPL"; side = Buy;
               qty = 100; limit_price = Some 18000; seq = 1 };
  OrderEvent { order_id = "ORD-002"; symbol = "MSFT"; side = Buy;
               qty = 50;  limit_price = Some 42000; seq = 2 };
  OrderEvent { order_id = "ORD-003"; symbol = "GOOG"; side = Sell;
               qty = 10;  limit_price = None;        seq = 3 };
  OrderEvent { order_id = "ORD-004"; symbol = "AMZN"; side = Buy;
               qty = 100; limit_price = Some 19000;  seq = 4 };

  (* Internal execution reports *)
  ExecutionEvent { exec_id = "EX-001"; order_id = "ORD-001"; symbol = "AAPL";
                   side = Buy;  qty = 100; price = 17985;
                   trade_time = 1000; arrival_time = 1001; seq = 10 };
  ExecutionEvent { exec_id = "EX-002"; order_id = "ORD-002"; symbol = "MSFT";
                   side = Buy;  qty = 50;  price = 41950;
                   trade_time = 1002; arrival_time = 1003; seq = 11 };
  ExecutionEvent { exec_id = "EX-003"; order_id = "ORD-003"; symbol = "GOOG";
                   side = Sell; qty = 10;  price = 175000;
                   trade_time = 1004; arrival_time = 1005; seq = 12 };
  (* Partial fill on ORD-004 (100 ordered, 25 filled); price slightly misentered *)
  ExecutionEvent { exec_id = "EX-004"; order_id = "ORD-004"; symbol = "AMZN";
                   side = Buy;  qty = 25;  price = 18960;
                   trade_time = 1006; arrival_time = 1007; seq = 13 };

  (* Price correction on EX-004: arrives before broker confirms, seq=20 < seq=30+ *)
  CorrectionEvent { correction_id = "CORR-001"; original_exec_id = "EX-004";
                    corrected_qty = 25; corrected_price = 18920;
                    reason = "Trader entered price as 189.60; actual fill was 189.20";
                    arrival_time = 1010; seq = 20 };

  (* Broker confirms *)
  ConfirmEvent { broker_exec_id = "BRK-001"; exec_id = "EX-001"; symbol = "AAPL";
                 side = Buy;  qty = 100; price = 17985;
                 trade_time = 1000; arrival_time = 1050; seq = 30 };
  (* Broker reports $421.00 — we recorded $419.50 — PRICE_MISMATCH *)
  ConfirmEvent { broker_exec_id = "BRK-002"; exec_id = "EX-002"; symbol = "MSFT";
                 side = Buy;  qty = 50;  price = 42100;
                 trade_time = 1002; arrival_time = 1051; seq = 31 };
  (* No confirm for EX-003 (GOOG) — left UNCONFIRMED *)
  (* Broker confirms corrected price=18920 for AMZN partial fill *)
  ConfirmEvent { broker_exec_id = "BRK-004"; exec_id = "EX-004"; symbol = "AMZN";
                 side = Buy;  qty = 25;  price = 18920;
                 trade_time = 1006; arrival_time = 1052; seq = 32 };
  (* Broker sends a confirm we have no record of — UNEXPECTED *)
  ConfirmEvent { broker_exec_id = "BRK-999"; exec_id = "EX-999"; symbol = "TSLA";
                 side = Sell; qty = 5;   price = 24000;
                 trade_time = 1008; arrival_time = 1053; seq = 33 };
]

let pp_portfolio pf =
  StringMap.iter (fun sym pos ->
    Printf.printf "  %-6s  %+d\n" sym pos.Positions.net_qty
  ) pf.Positions.positions;
  let c = pf.Positions.cash_cents in
  Printf.printf "  Cash    %s%d.%02d\n"
    (if c < 0 then "-$" else "+$") (abs c / 100) (abs c mod 100)

let () =
  let state = Replay.replay events in

  Printf.printf "=== Replay errors (%d) ===\n" (List.length state.errors);
  List.iter (fun e -> Printf.printf "  ERROR: %s\n" e) (List.rev state.errors);

  Printf.printf "\n=== Reconciliation ===\n";
  let results = Reconcile.run state in
  List.iter (fun r ->
    Printf.printf "  %-8s  %s\n" r.Reconcile.exec_id
      (Reconcile.pp_status r.Reconcile.status)
  ) results;

  Printf.printf "\n=== Current positions (full replay) ===\n";
  pp_portfolio (Positions.calculate state);

  Printf.printf "\n=== Mismatch spotlight: EX-002 (MSFT) ===\n";
  (match StringMap.find_opt "EX-002" state.executions with
   | None -> ()
   | Some ex ->
     Printf.printf "  Internal record : %s  qty=%d  price=%s\n"
       ex.exec_id ex.qty (pp_price ex.price));
  (match StringMap.find_opt "EX-002" state.broker_confirms with
   | None -> ()
   | Some bc ->
     Printf.printf "  Broker confirm  : %s  qty=%d  price=%s\n"
       bc.broker_exec_id bc.qty (pp_price bc.price));
  Printf.printf "  Difference      : %s\n" (pp_price (42100 - 41950));

  Printf.printf "\n=== Order fill status ===\n";
  List.iter (fun oid ->
    match Replay.order_fill_status state oid with
    | None -> ()
    | Some fs ->
      Printf.printf "  %-8s  filled=%d/%d  open=%d  execs=[%s]\n"
        oid fs.cum_filled_qty fs.ordered_qty fs.open_qty
        (String.concat "," fs.exec_ids)
  ) ["ORD-001"; "ORD-002"; "ORD-003"; "ORD-004"];

  Printf.printf "\n=== Bitemporal positions ===\n";

  (* 1. Snapshot at seq=15: before the correction (seq=20) arrives *)
  let state_pre_corr = Replay.replay_until 15 events in
  Printf.printf "\n  [seq ≤ 15, all trade_times] — EX-004 at original price $189.60:\n";
  pp_portfolio (Positions.calculate state_pre_corr);

  (* 2. Full replay with correction applied *)
  Printf.printf "\n  [all seqs, all trade_times] — EX-004 at corrected price $189.20:\n";
  pp_portfolio (Positions.calculate state);

  (* 3. Full replay but filter to trades that settled before trade_time=1003 *)
  Printf.printf "\n  [all seqs, trade_time ≤ 1003] — only AAPL and MSFT:\n";
  pp_portfolio (Positions.calculate_at 1003 state);

  (* 4. Corrected historical at trade_time=1006 using seq=15 knowledge *)
  let pf_naive = Positions.calculate_at 1006 state_pre_corr in
  let pf_corr  = Positions.calculate_at 1006 state in
  Printf.printf "\n  [seq ≤ 15, trade_time ≤ 1006] — AMZN cash before correction:\n";
  Printf.printf "    Portfolio cash: %s\n"
    (let c = pf_naive.cash_cents in
     Printf.sprintf "%s$%d.%02d" (if c<0 then "-" else "+") (abs c/100) (abs c mod 100));
  Printf.printf "\n  [all seqs, trade_time ≤ 1006] — AMZN cash with hindsight correction:\n";
  Printf.printf "    Portfolio cash: %s\n"
    (let c = pf_corr.cash_cents in
     Printf.sprintf "%s$%d.%02d" (if c<0 then "-" else "+") (abs c/100) (abs c mod 100))
