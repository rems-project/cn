module CB = Cerb_backend
module BT = BaseTypes
module A = Cn_to_ail.A
module IT = IndexTerms
module LRT = LogicalReturnTypes
module LAT = LogicalArgumentTypes
module AT = ArgumentTypes

type executable_spec =
  { pre_post : (A.ail_identifier * (string list * string list)) list;
    in_stmt : (Cerb_location.t * string list) list;
    returns :
      (Cerb_location.t
      * (Cerb_frontend.GenTypes.genTypeCategory A.expression option * string list))
        list
  }

val generate_c_assume_pres_internal
  :  Extract.instrumentation list ->
  Cerb_frontend.GenTypes.genTypeCategory A.sigma ->
  unit Mucore.file ->
  (Cn_to_ail.A.sigma_declaration
  * Cerb_frontend.GenTypes.genTypeCategory Cn_to_ail.A.sigma_function_definition)
    list

val generate_c_specs
  :  bool ->
  bool ->
  bool ->
  Extract.instrumentation list ->
  Cerb_frontend.GenTypes.genTypeCategory Cn_to_ail.A.sigma ->
  unit Mucore.file ->
  executable_spec

val generate_c_records
  :  (Sym.t
     * (Cerb_location.t * Cerb_frontend.Annot.attributes * Cn_to_ail.C.tag_definition))
       list ->
  string

val generate_c_datatypes
  :  Cerb_frontend.GenTypes.genTypeCategory Cn_to_ail.A.sigma ->
  (Cerb_location.t * string) list

val generate_c_struct_strs
  :  (A.ail_identifier
     * (Cerb_location.t * Cerb_frontend.Annot.attributes * Cn_to_ail.C.tag_definition))
       list ->
  string

val generate_cn_versions_of_structs : Cn_to_ail.A.sigma_tag_definition list -> string

val generate_c_functions
  :  Cerb_frontend.GenTypes.genTypeCategory A.sigma ->
  (A.ail_identifier * Definition.Function.t) list ->
  string * string * Cerb_location.t list

val generate_c_predicates
  :  Cerb_frontend.GenTypes.genTypeCategory Cn_to_ail.A.sigma ->
  (Sym.t * Definition.Predicate.t) list ->
  string * string * Cerb_location.t list

val generate_ownership_functions : bool -> Cn_to_ail.C.ctype list -> string * string

val generate_conversion_and_equality_functions
  :  Cerb_frontend.GenTypes.genTypeCategory Cn_to_ail.A.sigma ->
  string * string

val has_main : Cerb_frontend.GenTypes.genTypeCategory Cn_to_ail.A.sigma -> bool

val generate_ownership_global_assignments
  :  Cerb_frontend.GenTypes.genTypeCategory Cn_to_ail.A.sigma ->
  unit Mucore.file ->
  (Sym.t * (string list * string list)) list
