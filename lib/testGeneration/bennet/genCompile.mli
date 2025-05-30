val compile
  :  string ->
  ?ctx:GenDefinitions.Make(GenTerms).context ->
  (Sym.t * Definition.Predicate.t) list ->
  Test.t list ->
  GenDefinitions.Make(GenTerms).context
