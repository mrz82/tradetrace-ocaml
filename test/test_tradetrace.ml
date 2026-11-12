open Tradetrace
open Types

(* ------------------------------------------------------------------ *)
(* Minimal test runner *)

let failures = ref []

let check name condition =
  if condition then
    Printf.printf "[PASS] %s\n%!" name
  else begin
    Printf.printf "[FAIL] %s\n%!" name;
    failures := name :: !failures
  end

(* ------------------------------------------------------------------ *)
(* Test-event helpers *)

(* trade_time = seq * 10 so that trade_time ordering matches seq ordering *)
let mk_order order_id symbol side qty seq =
  OrderEvent { order_id; symbol; side; qty; limit_price = None; seq }

let mk_exec exec_id order_id symbol side qty price seq =
  ExecutionEvent { exec_id; order_id; symbol; side; qty; price;
                   trade_time = seq * 10; arrival_time = seq * 10 + 1; seq }

let mk_confirm exec_id qty price seq =
  ConfirmEvent { broker_exec_id = "B-" ^ exec_id; exec_id; symbol = "X";
                 side = Buy; qty; price; trade_time = seq * 10;
                 arrival_time = seq * 10 + 2; seq }

let mk_cancel cancel_id order_id seq =
  CancelEvent { cancel_id; order_id; reason = "test cancel";
                arrival_time = seq * 10; seq }

let mk_correction correction_id original_exec_id qty price seq =
  CorrectionEvent { correction_id; original_exec_id;
                    corrected_qty = qty; corrected_price = price;
                    reason = "test correction"; arrival_time = seq * 10; seq }

let recon_status exec_id state =
  match List.find_opt (fun r -> r.Reconcile.exec_id = exec_id)
          (Reconcile.run state) with
  | Some r -> Some r.Reconcile.status
  | None   -> None

(* ------------------------------------------------------------------ *)

let () = Printf.printf "=== Reconciliation tests ===\n%!"

let () =
  let state = Replay.replay [
    mk_order   "O1" "X" Buy 10 1;
    mk_exec    "E1" "O1" "X" Buy 10 5000 2;
    mk_confirm "E1" 10 5000 3;
  ] in
  check "matched: price and qty agree"
    (recon_status "E1" state = Some Reconcile.Matched)

let () =
  let state = Replay.replay [
    mk_order   "O1" "X" Buy 10 1;
    mk_exec    "E1" "O1" "X" Buy 10 5000 2;
    mk_confirm "E1" 10 5100 3;
  ] in
  check "price mismatch detected"
    (match recon_status "E1" state with
     | Some (Reconcile.PriceMismatch { internal = 5000; broker = 5100 }) -> true
     | _ -> false)

let () =
  let state = Replay.replay [
    mk_order   "O1" "X" Buy 10 1;
    mk_exec    "E1" "O1" "X" Buy 10 5000 2;
    mk_confirm "E1" 7 5000 3;
  ] in
  check "qty mismatch detected"
    (match recon_status "E1" state with
     | Some (Reconcile.QtyMismatch { internal = 10; broker = 7 }) -> true
     | _ -> false)

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 10 1;
    mk_exec  "E1" "O1" "X" Buy 10 5000 2;
    (* no broker confirm *)
  ] in
  check "unconfirmed execution detected"
    (recon_status "E1" state = Some Reconcile.Unconfirmed)

let () =
  let state = Replay.replay [
    mk_confirm "E1" 10 5000 1;  (* no internal execution *)
  ] in
  check "unexpected broker confirm detected"
    (recon_status "E1" state = Some Reconcile.Unexpected)

(* Correction applied before reconciliation:
   order qty=100 avoids overfill; exec initially wrong price, corrected,
   broker confirms at corrected price. *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 25 5100 2;  (* exec at wrong price *)
    mk_correction "C1" "E1" 25 5000 3;           (* corrected price *)
    mk_confirm    "E1" 25 5000 4;                 (* broker confirms corrected values *)
  ] in
  check "correction applied before reconciliation — matched at corrected values"
    (recon_status "E1" state = Some Reconcile.Matched)

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== Replay validation tests ===\n%!"

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 10 1;
    mk_order "O1" "X" Buy 10 2;
  ] in
  check "duplicate order produces an error"
    (List.length state.errors = 1)

let () =
  let state = Replay.replay [
    mk_order  "O1" "X" Buy 10 1;
    mk_cancel "K1" "O1" 2;
    mk_exec   "E1" "O1" "X" Buy 10 5000 3;
  ] in
  check "execution on cancelled order produces an error"
    (List.length state.errors = 1 &&
     not (StringMap.mem "E1" state.executions))

let () =
  let state = Replay.replay [
    mk_exec "E1" "GHOST" "X" Buy 10 5000 1;
  ] in
  check "execution on unknown order produces an error"
    (List.length state.errors = 1 &&
     not (StringMap.mem "E1" state.executions))

let () =
  let ordered = [
    mk_order   "O1" "X" Buy 10 1;
    mk_exec    "E1" "O1" "X" Buy 10 5000 2;
    mk_confirm "E1" 10 5000 3;
  ] in
  let s1 = Replay.replay ordered in
  let s2 = Replay.replay (List.rev ordered) in
  check "replay is deterministic regardless of input order"
    (StringMap.cardinal s1.executions = StringMap.cardinal s2.executions &&
     StringMap.mem "E1" s1.executions && StringMap.mem "E1" s2.executions &&
     List.length s1.errors = List.length s2.errors)

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== Position calculation tests ===\n%!"

let () =
  let state = Replay.replay [
    mk_order "O1" "AAPL" Buy 100 1;
    mk_exec  "E1" "O1" "AAPL" Buy 100 17985 2;
  ] in
  let pf = Positions.calculate state in
  check "buy increases long position"
    (match StringMap.find_opt "AAPL" pf.positions with
     | Some p -> p.net_qty = 100
     | None   -> false);
  check "buy reduces cash"
    (pf.cash_cents = -(100 * 17985))

let () =
  let state = Replay.replay [
    mk_order "O1" "AAPL" Sell 50 1;
    mk_exec  "E1" "O1" "AAPL" Sell 50 18100 2;
  ] in
  let pf = Positions.calculate state in
  check "sell creates short position"
    (match StringMap.find_opt "AAPL" pf.positions with
     | Some p -> p.net_qty = -50
     | None   -> false);
  check "sell increases cash"
    (pf.cash_cents = 50 * 18100)

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy  100 1;
    mk_order "O2" "X" Sell 100 2;
    mk_exec  "E1" "O1" "X" Buy  100 5000 3;
    mk_exec  "E2" "O2" "X" Sell 100 5000 4;
  ] in
  let pf = Positions.calculate state in
  check "opposite fills at equal price net to zero position"
    (not (StringMap.mem "X" pf.positions));
  check "opposite fills at equal price net to zero cash"
    (pf.cash_cents = 0)

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 200 1;
    mk_exec  "E1" "O1" "X" Buy 100 5000 2;
    mk_exec  "E2" "O1" "X" Buy 100 5050 3;
  ] in
  let pf = Positions.calculate state in
  check "partial fills accumulate position"
    (match StringMap.find_opt "X" pf.positions with
     | Some p -> p.net_qty = 200
     | None   -> false);
  check "partial fills accumulate cash correctly"
    (pf.cash_cents = -(100 * 5000 + 100 * 5050))

(* Correction changes position and cash.
   Order qty=100 so exec qty=25 is not an overfill; correction reduces qty to 20. *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 25 5000 2;
    mk_correction "C1" "E1" 20 5000 3;
  ] in
  let pf = Positions.calculate state in
  check "position uses corrected qty not original"
    (match StringMap.find_opt "X" pf.positions with
     | Some p -> p.net_qty = 20
     | None   -> false);
  check "cash uses corrected qty not original"
    (pf.cash_cents = -(20 * 5000))

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== Partial-fill / overfill tests ===\n%!"

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    mk_exec  "E1" "O1" "X" Buy 60 5000 2;
    mk_exec  "E2" "O1" "X" Buy 60 5000 3;  (* 60+60=120 > 100 *)
  ] in
  check "overfill is rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "E2" state.executions));
  check "non-overfilling first fill is kept"
    (StringMap.mem "E1" state.executions)

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    mk_exec  "E1" "O1" "X" Buy 60 5000 2;
    mk_exec  "E2" "O1" "X" Buy 40 5000 3;  (* 60+40=100: exact *)
  ] in
  check "exact fill is accepted"
    (StringMap.mem "E2" state.executions &&
     List.length state.errors = 0)

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    mk_exec  "E1" "O1" "X" Buy 100 5000 2;
    mk_exec  "E2" "O1" "X" Buy 1   5000 3;  (* 100+1=101 > 100 *)
  ] in
  check "one-share overfill is rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "E2" state.executions))

(* A correction that reduces a prior fill opens capacity for a later fill *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 90 5000 2;   (* fills 90/100 *)
    mk_correction "C1" "E1" 50 5000 3;            (* corrected: only 50 filled *)
    mk_exec       "E2" "O1" "X" Buy 50 5000 4;   (* 50+50=100: now fits *)
  ] in
  check "correction reducing prior fill allows subsequent fill to pass"
    (StringMap.mem "E2" state.executions &&
     List.length state.errors = 0)

(* Without the correction the same second fill would be an overfill *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    mk_exec  "E1" "O1" "X" Buy 90 5000 2;
    (* no correction *)
    mk_exec  "E2" "O1" "X" Buy 50 5000 3;   (* 90+50=140 > 100 *)
  ] in
  check "without correction the same fill would be an overfill"
    (List.length state.errors = 1 &&
     not (StringMap.mem "E2" state.executions))

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== Correction history tests ===\n%!"

let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_correction "C1" "E1" 50 5100 3;   (* first correction *)
    mk_correction "C2" "E1" 50 5200 4;   (* second correction *)
  ] in
  check "latest correction applied — price is 5200"
    (match Replay.resolved_execution state "E1" with
     | Some e -> e.price = 5200
     | None   -> false);
  check "both corrections stored in history"
    (match StringMap.find_opt "E1" state.corrections with
     | Some corrs -> List.length corrs = 2
     | None       -> false)

let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_correction "C1" "E1" 50 5100 3;
    mk_correction "C1" "E1" 50 5200 4;   (* duplicate correction_id *)
  ] in
  check "duplicate correction id rejected"
    (List.length state.errors = 1);
  check "first correction still applied after duplicate rejection"
    (match Replay.resolved_execution state "E1" with
     | Some e -> e.price = 5100
     | None   -> false)

let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    (* correction references exec that does not exist *)
    mk_correction "C1" "GHOST" 50 5100 3;
  ] in
  check "correction on unknown exec produces an error"
    (List.length state.errors = 1)

(* Three sequential corrections: only the last (highest seq) applies *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_correction "C1" "E1" 50 5100 3;
    mk_correction "C2" "E1" 50 5200 4;
    mk_correction "C3" "E1" 50 5300 5;
  ] in
  check "three corrections: latest (5300) wins"
    (match Replay.resolved_execution state "E1" with
     | Some e -> e.price = 5300
     | None   -> false);
  check "three corrections: all three stored"
    (match StringMap.find_opt "E1" state.corrections with
     | Some corrs -> List.length corrs = 3
     | None       -> false)

(* Correction ordering is by seq, not by correction_id string sort *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_correction "C2" "E1" 50 5200 3;   (* "C2" arrives first (seq=3) *)
    mk_correction "C1" "E1" 50 5100 4;   (* "C1" arrives second (seq=4) *)
  ] in
  check "correction ordering is by seq not by ID — latest seq (C1 at seq=4) wins"
    (match Replay.resolved_execution state "E1" with
     | Some e -> e.price = 5100
     | None   -> false)

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== Order fill status tests ===\n%!"

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 200 1;
    mk_exec  "E1" "O1" "X" Buy 80 5000 2;
    mk_exec  "E2" "O1" "X" Buy 70 5000 3;
  ] in
  check "fill status: partial fill"
    (match Replay.order_fill_status state "O1" with
     | Some fs -> fs.ordered_qty = 200 &&
                  fs.cum_filled_qty = 150 &&
                  fs.open_qty = 50
     | None -> false)

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    mk_exec  "E1" "O1" "X" Buy 100 5000 2;
  ] in
  check "fill status: fully filled"
    (match Replay.order_fill_status state "O1" with
     | Some fs -> fs.cum_filled_qty = 100 && fs.open_qty = 0
     | None -> false)

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    (* no executions *)
  ] in
  check "fill status: unfilled order"
    (match Replay.order_fill_status state "O1" with
     | Some fs -> fs.cum_filled_qty = 0 && fs.open_qty = 100
     | None -> false)

let () =
  check "fill status: unknown order returns None"
    (Replay.order_fill_status Replay.empty_state "GHOST" = None)

(* Fill status uses corrected quantities *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 60 5000 2;
    mk_correction "C1" "E1" 50 5000 3;   (* corrected down to 50 *)
  ] in
  check "fill status uses corrected qty"
    (match Replay.order_fill_status state "O1" with
     | Some fs -> fs.cum_filled_qty = 50 && fs.open_qty = 50
     | None -> false)

(* Exec IDs are reported in seq order *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 200 1;
    mk_exec  "E1" "O1" "X" Buy 100 5000 2;
    mk_exec  "E2" "O1" "X" Buy 100 5000 3;
  ] in
  check "fill status exec_ids in seq order"
    (match Replay.order_fill_status state "O1" with
     | Some fs -> fs.exec_ids = ["E1"; "E2"]
     | None -> false)

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== replay_until tests ===\n%!"

let () =
  let events = [
    mk_order "O1" "X" Buy 100 1;
    mk_exec  "E1" "O1" "X" Buy 50 5000 2;
    mk_exec  "E2" "O1" "X" Buy 50 5000 10;   (* arrives later *)
  ] in
  let state = Replay.replay_until 5 events in
  check "replay_until: event after cutoff not included"
    (StringMap.mem "E1" state.executions &&
     not (StringMap.mem "E2" state.executions))

let () =
  let events = [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_correction "C1" "E1" 50 5100 10;   (* correction seq=10 *)
  ] in
  let s_before = Replay.replay_until 5 events in   (* before correction *)
  let s_after  = Replay.replay events in            (* full replay *)
  check "replay_until: correction not yet applied at cutoff before it"
    (match Replay.resolved_execution s_before "E1" with
     | Some e -> e.price = 5000
     | None   -> false);
  check "replay_until: correction applied in full replay"
    (match Replay.resolved_execution s_after "E1" with
     | Some e -> e.price = 5100
     | None   -> false)

let () =
  let events = [
    mk_order "O1" "X" Buy 10 1;
    mk_exec  "E1" "O1" "X" Buy 10 5000 2;
  ] in
  let s1 = Replay.replay events in
  let s2 = Replay.replay_until 100 events in   (* cutoff beyond all events *)
  check "replay_until with cutoff beyond all events equals full replay"
    (StringMap.cardinal s1.executions = StringMap.cardinal s2.executions &&
     List.length s1.errors = List.length s2.errors)

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== Bitemporal position tests ===\n%!"

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;   (* trade_time = 10 *)
    mk_order "O2" "Y" Buy 100 2;   (* trade_time = 20 *)
    mk_exec  "E1" "O1" "X" Buy 100 5000 3;   (* trade_time = 30 *)
    mk_exec  "E2" "O2" "Y" Buy 100 5000 4;   (* trade_time = 40 *)
  ] in
  let pf35 = Positions.calculate_at 35 state in
  check "calculate_at: trade after cutoff excluded"
    (StringMap.mem "X" pf35.positions &&
     not (StringMap.mem "Y" pf35.positions));
  check "calculate_at: cash only from trade before cutoff"
    (pf35.cash_cents = -(100 * 5000))

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    mk_exec  "E1" "O1" "X" Buy 100 5000 2;   (* trade_time = 20 *)
  ] in
  check "calculate_at exact boundary: trade_time = cutoff is included"
    (let pf = Positions.calculate_at 20 state in
     match StringMap.find_opt "X" pf.positions with
     | Some p -> p.net_qty = 100
     | None   -> false)

(* Bitemporal: seq cutoff excludes correction; trade_time cutoff filters trades *)
let () =
  let events = [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;    (* trade_time=20, price=5000 *)
    mk_exec       "E2" "O1" "X" Buy 50 5000 3;    (* trade_time=30, price=5000 *)
    mk_correction "C1" "E1" 50 5100 10;            (* correction at seq=10 *)
  ] in
  (* Naive: seq ≤ 5 and trade_time ≤ 25 (before E2 and before correction) *)
  let pf_naive = Positions.calculate_at 25 (Replay.replay_until 5 events) in
  (* Corrected historical: all seqs, trade_time ≤ 25 *)
  let pf_corr  = Positions.calculate_at 25 (Replay.replay events) in
  check "naive historical: original price used (cash = -50*5000)"
    (pf_naive.cash_cents = -(50 * 5000));
  check "corrected historical: corrected price used (cash = -50*5100)"
    (pf_corr.cash_cents = -(50 * 5100))

(* Full bitemporal: different (seq, trade_time) pairs give consistent results *)
let () =
  let events = [
    mk_order "O1" "X" Buy 200 1;
    mk_exec  "E1" "O1" "X" Buy 100 5000 2;   (* trade_time=20 *)
    mk_exec  "E2" "O1" "X" Buy 100 5000 5;   (* trade_time=50 *)
  ] in
  let pf_at_30 = Positions.calculate_at 30 (Replay.replay events) in
  let pf_all   = Positions.calculate    (Replay.replay events) in
  check "calculate_at before second trade: only first fill counted"
    (match StringMap.find_opt "X" pf_at_30.positions with
     | Some p -> p.net_qty = 100
     | None   -> false);
  check "calculate (no cutoff): both fills counted"
    (match StringMap.find_opt "X" pf_all.positions with
     | Some p -> p.net_qty = 200
     | None   -> false)

(* replay_until + calculate_at compose correctly *)
let () =
  let events = [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 100 5000 2;   (* trade_time=20 *)
    mk_correction "C1" "E1" 100 5500 10;           (* price correction at seq=10 *)
  ] in
  let pf_before = Positions.calculate (Replay.replay_until 5 events) in
  let pf_after  = Positions.calculate (Replay.replay events) in
  check "compose: before correction seq — original cash"
    (pf_before.cash_cents = -(100 * 5000));
  check "compose: after correction seq — corrected cash"
    (pf_after.cash_cents = -(100 * 5500))

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== Execution and correction validation tests ===\n%!"

(* Symbol mismatch between execution and parent order *)
let () =
  let state = Replay.replay [
    mk_order "O1" "AAPL" Buy 100 1;
    ExecutionEvent { exec_id = "E1"; order_id = "O1"; symbol = "MSFT";
                     side = Buy; qty = 50; price = 5000;
                     trade_time = 20; arrival_time = 21; seq = 2 };
  ] in
  check "execution symbol mismatch is rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "E1" state.executions))

(* Side mismatch between execution and parent order *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    mk_exec  "E1" "O1" "X" Sell 50 5000 2;  (* Buy order, Sell exec *)
  ] in
  check "execution side mismatch is rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "E1" state.executions))

(* Side mismatch in opposite direction *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Sell 100 1;
    mk_exec  "E1" "O1" "X" Buy 50 5000 2;  (* Sell order, Buy exec *)
  ] in
  check "execution Buy-on-Sell-order is rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "E1" state.executions))

(* Matching symbol and side: accepted regardless of other values *)
let () =
  let state = Replay.replay [
    mk_order "O1" "AAPL" Sell 100 1;
    mk_exec  "E1" "O1" "AAPL" Sell 50 5000 2;
  ] in
  check "matching symbol and side: execution accepted"
    (StringMap.mem "E1" state.executions &&
     List.length state.errors = 0)

(* Symbol mismatch does not affect subsequent valid executions on the same order *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    ExecutionEvent { exec_id = "E1"; order_id = "O1"; symbol = "Y";
                     side = Buy; qty = 50; price = 5000;
                     trade_time = 20; arrival_time = 21; seq = 2 };
    mk_exec "E2" "O1" "X" Buy 50 5000 3;  (* correct symbol *)
  ] in
  check "symbol mismatch error does not block later valid exec on same order"
    (not (StringMap.mem "E1" state.executions) &&
     StringMap.mem "E2" state.executions &&
     List.length state.errors = 1)

(* Correction raising qty to exactly the order boundary is accepted *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_exec       "E2" "O1" "X" Buy 30 5000 3;  (* total 80 *)
    mk_correction "C1" "E1" 70 5000 4;           (* 70+30=100: exactly fills *)
  ] in
  check "correction to exactly-full order boundary is accepted"
    (List.length state.errors = 0 &&
     match Replay.resolved_execution state "E1" with
     | Some e -> e.qty = 70
     | None   -> false)

(* Correction raising qty one above the order boundary is rejected *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_exec       "E2" "O1" "X" Buy 30 5000 3;  (* total 80 *)
    mk_correction "C1" "E1" 71 5000 4;           (* 71+30=101 > 100 *)
  ] in
  check "correction one-above-boundary is rejected"
    (List.length state.errors = 1 &&
     match Replay.resolved_execution state "E1" with
     | Some e -> e.qty = 50  (* original qty unchanged *)
     | None   -> false)

(* Correction on a fully-filled order is rejected *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_exec       "E2" "O1" "X" Buy 50 5000 3;  (* exactly full: 100/100 *)
    mk_correction "C1" "E1" 51 5000 4;           (* 51+50=101 > 100 *)
  ] in
  check "correction rejected when order is exactly full"
    (List.length state.errors = 1 &&
     match Replay.resolved_execution state "E1" with
     | Some e -> e.qty = 50
     | None   -> false)

(* Correction on the middle fill: overfill check accounts for all surrounding fills *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 30 5000 2;
    mk_exec       "E2" "O1" "X" Buy 30 5000 3;
    mk_exec       "E3" "O1" "X" Buy 30 5000 4;  (* total 90 *)
    mk_correction "C1" "E2" 70 5000 5;           (* 30+70+30=130 > 100 *)
  ] in
  check "correction on middle fill rejected: sum of surrounding fills included in check"
    (List.length state.errors = 1 &&
     match Replay.resolved_execution state "E2" with
     | Some e -> e.qty = 30  (* unchanged *)
     | None   -> false)

(* Valid correction on middle fill is accepted *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 30 5000 2;
    mk_exec       "E2" "O1" "X" Buy 30 5000 3;
    mk_exec       "E3" "O1" "X" Buy 30 5000 4;  (* total 90 *)
    mk_correction "C1" "E2" 40 5000 5;           (* 30+40+30=100: exactly fills *)
  ] in
  check "valid correction on middle fill is accepted"
    (List.length state.errors = 0 &&
     match Replay.resolved_execution state "E2" with
     | Some e -> e.qty = 40
     | None   -> false)

(* Second correction also checked: even with a first correction in the chain *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_exec       "E2" "O1" "X" Buy 30 5000 3;  (* total 80 *)
    mk_correction "C1" "E1" 70 5000 4;           (* 70+30=100: valid, exactly fills *)
    mk_correction "C2" "E1" 71 5000 5;           (* 71+30=101 > 100: rejected *)
  ] in
  check "second correction rejected if it would overfill even after first correction"
    (List.length state.errors = 1 &&
     match Replay.resolved_execution state "E1" with
     | Some e -> e.qty = 70  (* C1 applied; C2 rejected *)
     | None   -> false)

(* Structural guarantee: corrections cannot change symbol *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "AAPL" Buy 100 1;
    mk_exec       "E1" "O1" "AAPL" Buy 50 5000 2;
    mk_correction "C1" "E1" 60 5100 3;
    mk_correction "C2" "E1" 70 5200 4;
  ] in
  check "corrections cannot change symbol of original execution"
    (match Replay.resolved_execution state "E1" with
     | Some e -> e.symbol = "AAPL"
     | None   -> false)

(* Structural guarantee: corrections cannot change side *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_correction "C1" "E1" 60 5100 3;
  ] in
  check "corrections cannot change side of original execution"
    (match Replay.resolved_execution state "E1" with
     | Some e -> e.side = Buy
     | None   -> false)

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== Input validation — quantities and prices ===\n%!"

let () =
  let state = Replay.replay [
    OrderEvent { order_id = "BAD-O"; symbol = "X"; side = Buy;
                 qty = 0; limit_price = None; seq = 1 };
  ] in
  check "order with zero qty rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "BAD-O" state.orders))

let () =
  let state = Replay.replay [
    OrderEvent { order_id = "BAD-O"; symbol = "X"; side = Buy;
                 qty = -10; limit_price = None; seq = 1 };
  ] in
  check "order with negative qty rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "BAD-O" state.orders))

let () =
  let state = Replay.replay [
    OrderEvent { order_id = "BAD-O"; symbol = "X"; side = Buy;
                 qty = 100; limit_price = Some 0; seq = 1 };
  ] in
  check "order with zero limit price rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "BAD-O" state.orders))

let () =
  let state = Replay.replay [
    OrderEvent { order_id = "BAD-O"; symbol = "X"; side = Buy;
                 qty = 100; limit_price = Some (-500); seq = 1 };
  ] in
  check "order with negative limit price rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "BAD-O" state.orders))

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    ExecutionEvent { exec_id = "BAD-E"; order_id = "O1"; symbol = "X";
                     side = Buy; qty = 0; price = 5000;
                     trade_time = 20; arrival_time = 21; seq = 2 };
  ] in
  check "execution with zero qty rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "BAD-E" state.executions))

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    ExecutionEvent { exec_id = "BAD-E"; order_id = "O1"; symbol = "X";
                     side = Buy; qty = -50; price = 5000;
                     trade_time = 20; arrival_time = 21; seq = 2 };
  ] in
  check "execution with negative qty rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "BAD-E" state.executions))

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    ExecutionEvent { exec_id = "BAD-E"; order_id = "O1"; symbol = "X";
                     side = Buy; qty = 50; price = 0;
                     trade_time = 20; arrival_time = 21; seq = 2 };
  ] in
  check "execution with zero price rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "BAD-E" state.executions))

let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    ExecutionEvent { exec_id = "BAD-E"; order_id = "O1"; symbol = "X";
                     side = Buy; qty = 50; price = -5000;
                     trade_time = 20; arrival_time = 21; seq = 2 };
  ] in
  check "execution with negative price rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "BAD-E" state.executions))

let () =
  let state = Replay.replay [
    ConfirmEvent { broker_exec_id = "B-BAD"; exec_id = "BAD-C"; symbol = "X";
                   side = Buy; qty = 0; price = 5000;
                   trade_time = 10; arrival_time = 11; seq = 1 };
  ] in
  check "broker confirm with zero qty rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "BAD-C" state.broker_confirms))

let () =
  let state = Replay.replay [
    ConfirmEvent { broker_exec_id = "B-BAD"; exec_id = "BAD-C"; symbol = "X";
                   side = Buy; qty = 100; price = 0;
                   trade_time = 10; arrival_time = 11; seq = 1 };
  ] in
  check "broker confirm with zero price rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "BAD-C" state.broker_confirms))

(* Correction zero-qty policy: zero is not permitted.
   A fill cannot be voided by correcting to zero; use a reversal event instead. *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_correction "BAD-C" "E1" 0 5000 3;
  ] in
  check "correction with zero corrected_qty rejected (void fill must use reversal)"
    (List.length state.errors = 1 &&
     match Replay.resolved_execution state "E1" with
     | Some e -> e.qty = 50   (* original unchanged *)
     | None   -> false)

let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_correction "BAD-C" "E1" (-10) 5000 3;
  ] in
  check "correction with negative corrected_qty rejected"
    (List.length state.errors = 1 &&
     match Replay.resolved_execution state "E1" with
     | Some e -> e.qty = 50
     | None   -> false)

let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_correction "BAD-C" "E1" 50 0 3;
  ] in
  check "correction with zero corrected_price rejected"
    (List.length state.errors = 1 &&
     match Replay.resolved_execution state "E1" with
     | Some e -> e.price = 5000   (* original price unchanged *)
     | None   -> false)

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== Confirm symbol/side reconciliation tests ===\n%!"

(* Helper: confirm with explicit symbol and side, used only for reconciliation tests *)
let mk_confirm_sym exec_id symbol side qty price seq =
  ConfirmEvent { broker_exec_id = "B-" ^ exec_id; exec_id; symbol; side;
                 qty; price; trade_time = seq * 10; arrival_time = seq * 10 + 2; seq }

let () =
  let state = Replay.replay [
    mk_order        "O1" "X" Buy 100 1;
    mk_exec         "E1" "O1" "X" Buy 50 5000 2;
    mk_confirm_sym  "E1" "WRONG" Buy 50 5000 3;   (* broker reports wrong symbol *)
  ] in
  check "confirm symbol mismatch is reported as ConfirmSymbolMismatch"
    (match recon_status "E1" state with
     | Some (Reconcile.ConfirmSymbolMismatch { internal = "X"; broker = "WRONG" }) -> true
     | _ -> false)

let () =
  let state = Replay.replay [
    mk_order        "O1" "X" Buy 100 1;
    mk_exec         "E1" "O1" "X" Buy 50 5000 2;
    mk_confirm_sym  "E1" "X" Sell 50 5000 3;   (* broker reports wrong side *)
  ] in
  check "confirm side mismatch Buy-vs-Sell is reported as ConfirmSideMismatch"
    (match recon_status "E1" state with
     | Some (Reconcile.ConfirmSideMismatch { internal = Buy; broker = Sell }) -> true
     | _ -> false)

let () =
  let state = Replay.replay [
    mk_order        "O1" "X" Sell 100 1;
    mk_exec         "E1" "O1" "X" Sell 50 5000 2;
    mk_confirm_sym  "E1" "X" Buy 50 5000 3;   (* broker says Buy; we sold *)
  ] in
  check "confirm side mismatch Sell-vs-Buy is reported as ConfirmSideMismatch"
    (match recon_status "E1" state with
     | Some (Reconcile.ConfirmSideMismatch { internal = Sell; broker = Buy }) -> true
     | _ -> false)

let () =
  let state = Replay.replay [
    mk_order        "O1" "X" Buy 100 1;
    mk_exec         "E1" "O1" "X" Buy 50 5000 2;
    mk_confirm_sym  "E1" "X" Buy 50 5000 3;   (* symbol and side correct *)
  ] in
  check "confirm with matching symbol and side proceeds to qty/price check"
    (recon_status "E1" state = Some Reconcile.Matched)

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== Seq tiebreaker and permutation tests ===\n%!"

(* When an order and an execution share the same seq value, the compound
   compare_events tiebreaker (kind rank: Order=0, Execution=1) must ensure
   the order is always processed first regardless of input list order. *)
let () =
  let events_fwd = [
    mk_order "O1" "X" Buy 10 5;             (* seq=5, kind=0 *)
    mk_exec  "E1" "O1" "X" Buy 10 5000 5;  (* seq=5, kind=1 *)
  ] in
  let events_rev = List.rev events_fwd in
  let s1 = Replay.replay events_fwd in
  let s2 = Replay.replay events_rev in
  check "same-seq order then execution (forward): both accepted with zero errors"
    (StringMap.mem "E1" s1.executions && List.length s1.errors = 0);
  check "same-seq order then execution (reversed): same result via tiebreaker"
    (StringMap.mem "E1" s2.executions && List.length s2.errors = 0)

(* Five-event stream: a specific shuffle must produce the identical final state *)
let () =
  let events = [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 40 5000 2;
    mk_exec       "E2" "O1" "X" Buy 40 5000 3;
    mk_correction "C1" "E1" 40 5100 4;
    mk_correction "C2" "E2" 40 5200 5;
  ] in
  let shuffled = [
    mk_correction "C1" "E1" 40 5100 4;
    mk_exec       "E2" "O1" "X" Buy 40 5000 3;
    mk_order      "O1" "X" Buy 100 1;
    mk_correction "C2" "E2" 40 5200 5;
    mk_exec       "E1" "O1" "X" Buy 40 5000 2;
  ] in
  let s1 = Replay.replay events in
  let s2 = Replay.replay shuffled in
  check "five-event permutation: same set of executions accepted"
    (StringMap.cardinal s1.executions = StringMap.cardinal s2.executions &&
     StringMap.mem "E1" s2.executions && StringMap.mem "E2" s2.executions);
  check "five-event permutation: same resolved prices after shuffle"
    (let price s id =
       match Replay.resolved_execution s id with Some e -> e.price | None -> 0
     in
     price s1 "E1" = 5100 && price s2 "E1" = 5100 &&
     price s1 "E2" = 5200 && price s2 "E2" = 5200)

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== Limit price validation tests ===\n%!"

(* Helper: order with a limit price *)
let mk_order_lim order_id symbol side qty limit seq =
  OrderEvent { order_id; symbol; side; qty; limit_price = Some limit; seq }

(* Buy limit: execution price must be ≤ limit *)
let () =
  let state = Replay.replay [
    mk_order_lim "O1" "X" Buy 100 18000 1;      (* limit $180.00 *)
    mk_exec      "E1" "O1" "X" Buy 50 18001 2;  (* $180.01 > limit: rejected *)
  ] in
  check "buy exec above limit is rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "E1" state.executions))

let () =
  let state = Replay.replay [
    mk_order_lim "O1" "X" Buy 100 18000 1;
    mk_exec      "E1" "O1" "X" Buy 50 18000 2;  (* at limit: accepted *)
  ] in
  check "buy exec at limit is accepted"
    (StringMap.mem "E1" state.executions && List.length state.errors = 0)

let () =
  let state = Replay.replay [
    mk_order_lim "O1" "X" Buy 100 18000 1;
    mk_exec      "E1" "O1" "X" Buy 50 17999 2;  (* below limit: accepted *)
  ] in
  check "buy exec below limit is accepted"
    (StringMap.mem "E1" state.executions && List.length state.errors = 0)

(* Sell limit: execution price must be ≥ limit *)
let () =
  let state = Replay.replay [
    mk_order_lim "O1" "X" Sell 100 17500 1;     (* limit $175.00 *)
    mk_exec      "E1" "O1" "X" Sell 50 17499 2; (* $174.99 < limit: rejected *)
  ] in
  check "sell exec below limit is rejected"
    (List.length state.errors = 1 &&
     not (StringMap.mem "E1" state.executions))

let () =
  let state = Replay.replay [
    mk_order_lim "O1" "X" Sell 100 17500 1;
    mk_exec      "E1" "O1" "X" Sell 50 17500 2; (* at limit: accepted *)
  ] in
  check "sell exec at limit is accepted"
    (StringMap.mem "E1" state.executions && List.length state.errors = 0)

let () =
  let state = Replay.replay [
    mk_order_lim "O1" "X" Sell 100 17500 1;
    mk_exec      "E1" "O1" "X" Sell 50 17501 2; (* above limit: accepted *)
  ] in
  check "sell exec above limit is accepted"
    (StringMap.mem "E1" state.executions && List.length state.errors = 0)

(* Market order (limit_price = None): any execution price is accepted *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;                 (* no limit *)
    mk_exec  "E1" "O1" "X" Buy 50 99999 2;       (* very high price: accepted *)
  ] in
  check "market order accepts any execution price"
    (StringMap.mem "E1" state.executions && List.length state.errors = 0)

(* Corrections must also respect the original order's limit price *)
let () =
  let state = Replay.replay [
    mk_order_lim "O1" "X" Buy 100 18000 1;
    mk_exec      "E1" "O1" "X" Buy 50 17900 2;  (* valid: $179.00 ≤ $180.00 *)
    mk_correction "C1" "E1" 50 18001 3;          (* $180.01 > limit: rejected *)
  ] in
  check "correction raising buy exec price above limit is rejected"
    (List.length state.errors = 1 &&
     match Replay.resolved_execution state "E1" with
     | Some e -> e.price = 17900   (* original price preserved *)
     | None   -> false)

let () =
  let state = Replay.replay [
    mk_order_lim "O1" "X" Sell 100 17500 1;
    mk_exec      "E1" "O1" "X" Sell 50 17600 2; (* valid: $176.00 ≥ $175.00 *)
    mk_correction "C1" "E1" 50 17499 3;          (* $174.99 < limit: rejected *)
  ] in
  check "correction lowering sell exec price below limit is rejected"
    (List.length state.errors = 1 &&
     match Replay.resolved_execution state "E1" with
     | Some e -> e.price = 17600   (* original price preserved *)
     | None   -> false)

(* ------------------------------------------------------------------ *)

let () = Printf.printf "\n=== P&L tests ===\n%!"

(* Unrealized P&L: long position gains when mark exceeds entry *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    mk_exec  "E1" "O1" "X" Buy 100 10000 2;
  ] in
  let marks = StringMap.singleton "X" 11000 in
  let pnl = Pnl.calculate state ~mark_prices:marks in
  check "unrealized P&L long: mark above entry = gain"
    (match StringMap.find_opt "X" pnl.Pnl.by_symbol with
     | Some p -> p.Pnl.unrealized_cents = 100 * (11000 - 10000)
     | None   -> false)

(* Unrealized P&L: short position gains when mark falls below entry *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Sell 100 1;
    mk_exec  "E1" "O1" "X" Sell 100 20000 2;
  ] in
  let marks = StringMap.singleton "X" 19000 in
  let pnl = Pnl.calculate state ~mark_prices:marks in
  check "unrealized P&L short: mark below entry = gain"
    (match StringMap.find_opt "X" pnl.Pnl.by_symbol with
     (* net_qty=-100, net_cost=-2000000; unrealized = -100*19000 - (-2000000) = +100000 *)
     | Some p -> p.Pnl.unrealized_cents = 100000
     | None   -> false)

(* No mark price supplied: unrealized is zero, position still present *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    mk_exec  "E1" "O1" "X" Buy 100 10000 2;
  ] in
  let pnl = Pnl.calculate state ~mark_prices:StringMap.empty in
  check "no mark supplied: unrealized = 0"
    (match StringMap.find_opt "X" pnl.Pnl.by_symbol with
     | Some p -> p.Pnl.unrealized_cents = 0
     | None   -> false)

(* Flat position (buy then sell at same price) is excluded from by_symbol *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy  100 1;
    mk_order "O2" "X" Sell 100 2;
    mk_exec  "E1" "O1" "X" Buy  100 10000 3;
    mk_exec  "E2" "O2" "X" Sell 100 10000 4;
  ] in
  let pnl = Pnl.calculate state ~mark_prices:(StringMap.singleton "X" 12000) in
  check "flat position excluded from by_symbol"
    (not (StringMap.mem "X" pnl.Pnl.by_symbol))

(* avg_entry_cents is the weighted average across all fills *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 200 1;
    mk_exec  "E1" "O1" "X" Buy 100 10000 2;
    mk_exec  "E2" "O1" "X" Buy 100 11000 3;
  ] in
  let pnl = Pnl.calculate state ~mark_prices:StringMap.empty in
  check "avg_entry_cents: weighted average of two fills"
    (match StringMap.find_opt "X" pnl.Pnl.by_symbol with
     | Some p -> p.Pnl.avg_entry_cents = 10500
     | None   -> false)

(* total_unrealized_cents sums across all symbols *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    mk_order "O2" "Y" Buy 100 2;
    mk_exec  "E1" "O1" "X" Buy 100 10000 3;
    mk_exec  "E2" "O2" "Y" Buy 100 20000 4;
  ] in
  let marks =
    List.fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty
      [("X", 11000); ("Y", 21000)]
  in
  let pnl = Pnl.calculate state ~mark_prices:marks in
  check "total_unrealized_cents sums all symbols"
    (pnl.Pnl.total_unrealized_cents =
       100 * (11000 - 10000) + 100 * (21000 - 20000))

(* Unrealized P&L uses corrected price, not the original *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 100 10000 2;
    mk_correction "C1" "E1" 100 9500 3;
  ] in
  let marks = StringMap.singleton "X" 10000 in
  let pnl = Pnl.calculate state ~mark_prices:marks in
  check "unrealized P&L uses corrected price: avg=9500 so unrealized=+$500"
    (match StringMap.find_opt "X" pnl.Pnl.by_symbol with
     | Some p -> p.Pnl.unrealized_cents = 100 * (10000 - 9500)
     | None   -> false)

(* correction_impact: buy exec price corrected down — cost falls, delta positive *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5100 2;
    mk_correction "C1" "E1" 50 5000 3;
  ] in
  check "correction_impact buy: price down = positive delta"
    (match Pnl.correction_impact state ~correction_id:"C1" with
     | Some ci ->
       ci.Pnl.exec_id      = "E1"           &&
       ci.Pnl.symbol       = "X"            &&
       ci.Pnl.before_cents = -(50 * 5100)   &&
       ci.Pnl.after_cents  = -(50 * 5000)   &&
       ci.Pnl.delta_cents  = 50 * (5100 - 5000)
     | None -> false)

(* correction_impact: sell exec price corrected down — proceeds fall, delta negative *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Sell 100 1;
    mk_exec       "E1" "O1" "X" Sell 50 5000 2;
    mk_correction "C1" "E1" 50 4900 3;
  ] in
  check "correction_impact sell: price down = negative delta"
    (match Pnl.correction_impact state ~correction_id:"C1" with
     | Some ci ->
       ci.Pnl.before_cents = 50 * 5000                 &&
       ci.Pnl.after_cents  = 50 * 4900                 &&
       ci.Pnl.delta_cents  = 50 * (4900 - 5000)
     | None -> false)

(* correction_impact: unknown correction_id returns None *)
let () =
  let state = Replay.replay [
    mk_order "O1" "X" Buy 100 1;
    mk_exec  "E1" "O1" "X" Buy 50 5000 2;
  ] in
  check "correction_impact: unknown correction_id returns None"
    (Pnl.correction_impact state ~correction_id:"GHOST" = None)

(* Regression: first correction in a two-correction chain — before = original execution *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_correction "C1" "E1" 50 4800 3;
    mk_correction "C2" "E1" 50 4700 4;
  ] in
  check "multi-correction: first correction before = original execution price"
    (match Pnl.correction_impact state ~correction_id:"C1" with
     | Some ci ->
       ci.Pnl.before_cents = -(50 * 5000) &&
       ci.Pnl.after_cents  = -(50 * 4800) &&
       ci.Pnl.delta_cents  = 50 * (5000 - 4800)
     | None -> false)

(* Regression: second correction in a chain — before = first correction's price, not original *)
let () =
  let state = Replay.replay [
    mk_order      "O1" "X" Buy 100 1;
    mk_exec       "E1" "O1" "X" Buy 50 5000 2;
    mk_correction "C1" "E1" 50 4800 3;
    mk_correction "C2" "E1" 50 4700 4;
  ] in
  check "multi-correction: second correction before = first correction's price"
    (match Pnl.correction_impact state ~correction_id:"C2" with
     | Some ci ->
       (* before must be C1's after-state: -(50 * 4800), NOT -(50 * 5000) *)
       ci.Pnl.before_cents = -(50 * 4800) &&
       ci.Pnl.after_cents  = -(50 * 4700) &&
       ci.Pnl.delta_cents  = 50 * (4800 - 4700)
     | None -> false)

(* ------------------------------------------------------------------ *)

let () =
  Printf.printf "\n";
  match !failures with
  | [] -> Printf.printf "All tests passed.\n%!"
  | fs ->
    Printf.printf "FAILED: %s\n%!" (String.concat ", " (List.rev fs));
    exit 1
