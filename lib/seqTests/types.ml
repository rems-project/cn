module CF = Cerb_frontend
module C = CF.Ctype
module SymSet = Set.Make (Sym)

type call = SymSet.elt option * bool * C.ctype * SymSet.elt * (C.ctype * string) list

type context = call list

type test_stats =
  { successes : int;
    failures : int;
    skipped : int;
    discarded : int;
    distrib : (string * int) list
  }

let empty_stats =
  { successes = 0; failures = 0; discarded = 0; skipped = 0; distrib = [] }


let no_qs : C.qualifiers = { const = false; restrict = false; volatile = false }
