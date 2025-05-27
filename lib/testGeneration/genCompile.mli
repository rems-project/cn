val compile
  :  string ->
  ?ctx:GenDefinitions.context ->
  (Sym.t * Definition.Predicate.t) list ->
  Test.t list ->
  GenDefinitions.context
