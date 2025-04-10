val compile
  :  ?ctx:GenDefinitions.context ->
  (Sym.t * Definition.Predicate.t) list ->
  Fulminate.Extract.instrumentation list ->
  GenDefinitions.context
