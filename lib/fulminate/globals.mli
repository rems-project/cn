module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype

val getter
  :  string ->
  Sym.t ->
  Sctypes.ctype ->
  A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition

val setter
  :  string ->
  C.union_tag ->
  Sctypes.ctype ->
  A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition

val accessor
  :  string ->
  C.union_tag ->
  Sctypes.ctype ->
  (A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition) list

val accessors
  :  string ->
  unit Mucore.file ->
  A.sigma_declaration list * CF.GenTypes.genTypeCategory A.sigma_function_definition list

val accessors_prototypes : string -> unit Mucore.file -> string

val accessors_str : string -> unit Mucore.file -> string
