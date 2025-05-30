val compile
  :  string ->
  ?ctx:GenContext.Make(GenTerms).t ->
  (Sym.t * Definition.Predicate.t) list ->
  Test.t list ->
  GenContext.Make(GenTerms).t
