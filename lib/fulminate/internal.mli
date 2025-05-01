open Cerb_frontend

type executable_spec =
  { pre_post : (AilSyntax.ail_identifier * (string list * string list)) list;
    in_stmt : (Cerb_location.t * string list) list;
    returns :
      (Cerb_location.t
      * (GenTypes.genTypeCategory AilSyntax.expression option * string list))
        list
  }

val generate_c_assume_pres_internal
  :  string ->
  (bool * Extract.instrumentation) list ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  unit Mucore.file ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val generate_c_specs
  :  bool ->
  bool ->
  bool ->
  string ->
  Extract.instrumentation list ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  unit Mucore.file ->
  executable_spec

val generate_c_records
  :  (Sym.t * (Cerb_location.t * Annot.attributes * Ctype.tag_definition)) list ->
  string

val generate_c_datatypes
  :  GenTypes.genTypeCategory AilSyntax.sigma ->
  (Cerb_location.t * string) list

val generate_c_struct_strs
  :  (AilSyntax.ail_identifier
     * (Cerb_location.t * Annot.attributes * Ctype.tag_definition))
       list ->
  string

val generate_c_struct_decl_strs
  :  (AilSyntax.ail_identifier
     * (Cerb_location.t * Cerb_frontend.Annot.attributes * Ctype.tag_definition))
       list ->
  string list

val generate_cn_versions_of_structs : AilSyntax.sigma_tag_definition list -> string

val generate_c_functions
  :  string ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  (AilSyntax.ail_identifier * Definition.Function.t) list ->
  string * string * Cerb_location.t list

val generate_c_predicates
  :  string ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  (Sym.t * Definition.Predicate.t) list ->
  string * string * Cerb_location.t list

val generate_ownership_functions : bool -> Ctype.ctype list -> string * string

val generate_conversion_and_equality_functions
  :  string ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  string * string

val has_main : GenTypes.genTypeCategory AilSyntax.sigma -> bool

val generate_ownership_global_assignments
  :  GenTypes.genTypeCategory AilSyntax.sigma ->
  unit Mucore.file ->
  (Sym.t * (string list * string list)) list
