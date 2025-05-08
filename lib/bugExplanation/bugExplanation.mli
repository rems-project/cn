module CF = Cerb_frontend
module A = CF.AilSyntax

val synthesize_replicators
  :  string ->
  CF.GenTypes.genTypeCategory A.sigma ->
  unit Mucore.file ->
  (bool * Fulminate.Extract.instrumentation) list ->
  (A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition) list

val synthesize_shape_analyzers
  :  string ->
  CF.GenTypes.genTypeCategory A.sigma ->
  unit Mucore.file ->
  (bool * Fulminate.Extract.instrumentation) list ->
  (A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition) list
