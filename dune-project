(lang dune 3.0)

(name tradetrace)

(generate_opam_files true)

(package
 (name tradetrace)
 (synopsis "Trading-desk reconciliation engine with deterministic event replay and bitemporal position queries")
 (description
  "Reconciles internal execution reports against broker confirmations. \
   Tracks partial fills, correction chains, and positions. \
   Supports bitemporal queries over an immutable event log. \
   Written in OCaml.")
 (depends ocaml))
