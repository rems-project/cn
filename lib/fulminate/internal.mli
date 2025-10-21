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

val generate_ghost_enum : _ Mucore.file -> string

val generate_ghost_call_site_glob : unit -> string list

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
  _ Mucore.file ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  string * string * Cerb_location.t list

val generate_c_predicates
  :  string ->
  _ Mucore.file ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  string * string * Cerb_location.t list

val generate_c_lemmas
  :  string ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  unit Mucore.file ->
  string * string

val generate_ownership_functions : bool -> Ctype.ctype list -> string * string

val generate_conversion_and_equality_functions
  :  string ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  string * string

val has_main : GenTypes.genTypeCategory AilSyntax.sigma -> bool

val generate_global_assignments
  :  ?exec_c_locs_mode:bool ->
  ?experimental_ownership_stack_mode:bool ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  unit Mucore.file ->
  (Sym.t * (string list * string list)) list

val generate_fn_call_ghost_args_injs
  :  string ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  unit Mucore.file ->
  (Cerb_location.t * string list) list

val generate_tag_definition_injs
  :  AilSyntax.sigma_tag_definition list ->
  (Cerb_location.t * string list) list
