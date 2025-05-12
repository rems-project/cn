val compile
  :  string ->
  ?ctx:GenDefinitions.context ->
  (Sym.t * Definition.Predicate.t) list ->
  (bool * Fulminate.Extract.instrumentation) list ->
  GenDefinitions.context
