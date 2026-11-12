(* Domain types for a single-venue equity trading desk.
   The event lifecycle:  Order → Execution(s) → Correction(s) → Confirm
   Any order can also be Cancelled.  Replay folds these in seq order to build
   state we can then query at any past point in time. *)

(* Prices are integers in cents: $179.85 = 17985.
   Float accumulates rounding error that shows up as phantom P&L at end of
   day, especially on large block trades.  Fixed-point sidesteps that. *)
type price  = int
type qty    = int
type symbol = string

type side = Buy | Sell

module StringMap = Map.Make (String)

type order = {
  order_id    : string;
  symbol      : symbol;
  side        : side;
  qty         : qty;
  limit_price : price option;  (* None = market order *)
  seq         : int;
}

(* An execution report says "your order filled."  Three timestamps travel
   with it because they mean different things:
     trade_time   — when the exchange says the fill happened (what settlement uses)
     arrival_time — when our feed handler received the message (useful for latency)
     seq          — message-bus sequence number; the ONLY key used for replay ordering
   Keeping them separate is what makes bitemporal queries possible: you can ask
   "what did we know at seq N about trades that happened before time T?" *)
type execution = {
  exec_id      : string;
  order_id     : string;
  symbol       : symbol;
  side         : side;
  qty          : qty;
  price        : price;
  trade_time   : int;
  arrival_time : int;
  seq          : int;
}

type broker_confirm = {
  broker_exec_id : string;
  exec_id        : string;
  symbol         : symbol;
  side           : side;
  qty            : qty;
  price          : price;
  trade_time     : int;
  arrival_time   : int;
  seq            : int;
}

type cancellation = {
  cancel_id    : string;
  order_id     : string;
  reason       : string;
  arrival_time : int;
  seq          : int;
}

type correction = {
  correction_id    : string;
  original_exec_id : string;
  corrected_qty    : qty;
  corrected_price  : price;
  (* Mandatory: most markets require a stated reason for trade corrections *)
  reason           : string;
  arrival_time     : int;
  seq              : int;
}

type event =
  | OrderEvent      of order
  | ExecutionEvent  of execution
  | ConfirmEvent    of broker_confirm
  | CancelEvent     of cancellation
  | CorrectionEvent of correction

let event_seq = function
  | OrderEvent      e -> e.seq
  | ExecutionEvent  e -> e.seq
  | ConfirmEvent    e -> e.seq
  | CancelEvent     e -> e.seq
  | CorrectionEvent e -> e.seq

(* Semantic rank used as a tiebreaker when two events share a seq value.
   The ordering matches the dependency chain: an order must exist before
   an execution references it; an execution must exist before a correction
   targets it.  Confirm and cancel do not depend on other events in the
   same batch, so they are ranked last by convention. *)
let event_kind_rank = function
  | OrderEvent _      -> 0
  | ExecutionEvent _  -> 1
  | CorrectionEvent _ -> 2
  | ConfirmEvent _    -> 3
  | CancelEvent _     -> 4

(* Canonical identity string for each event type, used as the final
   tiebreaker so that sort output is fully deterministic. *)
let event_id = function
  | OrderEvent      e -> e.order_id
  | ExecutionEvent  e -> e.exec_id
  | ConfirmEvent    e -> e.broker_exec_id
  | CancelEvent     e -> e.cancel_id
  | CorrectionEvent e -> e.correction_id

(* Primary key: seq.  Tiebreaker 1: semantic kind rank.
   Tiebreaker 2: event ID string.
   Together these make the sort output fully deterministic even when
   seq values collide, and they preserve dependency order within a batch. *)
let compare_events a b =
  let c = Int.compare (event_seq a) (event_seq b) in
  if c <> 0 then c
  else
    let c2 = Int.compare (event_kind_rank a) (event_kind_rank b) in
    if c2 <> 0 then c2
    else String.compare (event_id a) (event_id b)

let pp_side  = function Buy -> "Buy" | Sell -> "Sell"
let pp_price p = Printf.sprintf "$%d.%02d" (p / 100) (abs (p mod 100))
