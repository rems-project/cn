val compile
  :  string ->
  ?ctx:GenContext.MakeOptional(GenTerms).t ->
  (Sym.t * Definition.Predicate.t) list ->
  Test.t list ->
  GenContext.Make(GenTerms).t
