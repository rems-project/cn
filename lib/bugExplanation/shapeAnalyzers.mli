module CF = Cerb_frontend
module A = CF.AilSyntax

val synthesize
  :  string ->
  CF.GenTypes.genTypeCategory A.sigma ->
  unit Mucore.file ->
  (bool * Fulminate.Extract.instrumentation) list ->
  (A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition) list
