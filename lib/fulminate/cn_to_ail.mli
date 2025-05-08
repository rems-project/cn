open Cerb_frontend

val ownership_ctypes : Ctype.ctype list ref

type spec_mode =
  | Pre
  | Post
  | Loop
  | Statement

module MembersKey : sig
  type t = (Id.t * BaseTypes.t) list

  val compare : t -> t -> int
end

module RecordMap : module type of Map.Make (MembersKey)

val records : Sym.t RecordMap.t ref

val augment_record_map : ?cn_sym:Sym.t -> BaseTypes.t -> unit

val lookup_records_map_opt : BaseTypes.t -> Sym.t option

val bt_to_ail_ctype : ?pred_sym:Sym.t option -> BaseTypes.t -> Ctype.ctype

val wrap_with_convert_from
  :  ?sct:Sctypes.t ->
  GenTypes.genTypeCategory AilSyntax.expression_ ->
  BaseTypes.t ->
  GenTypes.genTypeCategory AilSyntax.expression_

val wrap_with_convert_to
  :  ?sct:Sctypes.t ->
  GenTypes.genTypeCategory AilSyntax.expression_ ->
  BaseTypes.t ->
  GenTypes.genTypeCategory AilSyntax.expression_

val wrap_with_convert_from_cn_bool
  :  GenTypes.genTypeCategory AilSyntax.expression ->
  GenTypes.genTypeCategory AilSyntax.expression

type ail_bindings_and_statements =
  AilSyntax.bindings * GenTypes.genTypeCategory AilSyntax.statement_ list

type ail_executable_spec =
  { pre : ail_bindings_and_statements;
    post : ail_bindings_and_statements;
    in_stmt : (Locations.t * ail_bindings_and_statements) list;
    loops :
      ((Locations.t * ail_bindings_and_statements)
      * (Locations.t * ail_bindings_and_statements))
        list
  }

val generate_get_or_put_ownership_function
  :  without_ownership_checking:bool ->
  Ctype.ctype ->
  AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition

val generate_assume_ownership_function
  :  without_ownership_checking:bool ->
  Ctype.ctype ->
  AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition

val generate_datatype_equality_function
  :  string ->
  AilSyntax.sigma_cn_datatype ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val generate_datatype_map_get
  :  Cerb_frontend.Symbol.sym Cerb_frontend.Cn.cn_datatype ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val generate_datatype_default_function
  :  Cerb_frontend.Symbol.sym Cerb_frontend.Cn.cn_datatype ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val generate_struct_conversion_to_function
  :  AilSyntax.sigma_tag_definition ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val generate_struct_conversion_from_function
  :  AilSyntax.sigma_tag_definition ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val generate_struct_equality_function
  :  ?is_record:bool ->
  AilSyntax.sigma_tag_definition ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val generate_struct_map_get
  :  AilSyntax.sigma_tag_definition ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val generate_struct_default_function
  :  ?is_record:bool ->
  AilSyntax.sigma_tag_definition ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val generate_record_tag : Sym.t -> BaseTypes.t -> Sym.t option

val generate_record_opt : Sym.t -> BaseTypes.t -> AilSyntax.sigma_tag_definition option

val generate_record_equality_function
  :  Sym.t * BaseTypes.member_types ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val generate_record_default_function
  :  'a ->
  Sym.t * BaseTypes.member_types ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val generate_record_map_get
  :  Sym.t * 'a ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val cn_to_ail_expr_toplevel
  :  string ->
  AilSyntax.sigma_cn_datatype list ->
  (Ctype.union_tag * Ctype.ctype) list ->
  Sym.t option ->
  spec_mode option ->
  IndexTerms.t ->
  AilSyntax.bindings
  * GenTypes.genTypeCategory AilSyntax.statement_ list
  * GenTypes.genTypeCategory AilSyntax.expression

val cn_to_ail_logical_constraint
  :  string ->
  AilSyntax.sigma_cn_datatype list ->
  (Ctype.union_tag * Ctype.ctype) list ->
  spec_mode option ->
  LogicalConstraints.t ->
  AilSyntax.bindings
  * GenTypes.genTypeCategory AilSyntax.statement_ list
  * GenTypes.genTypeCategory AilSyntax.expression

val cn_to_ail_struct
  :  AilSyntax.sigma_tag_definition ->
  AilSyntax.sigma_tag_definition list

val cn_to_ail_datatype
  :  ?first:bool ->
  AilSyntax.sigma_cn_datatype ->
  Locations.t * AilSyntax.sigma_tag_definition list

val cn_to_ail_records
  :  (MembersKey.t * AilSyntax.ail_identifier) list ->
  AilSyntax.sigma_tag_definition list

val cn_to_ail_function
  :  string ->
  Sym.t * Definition.Function.t ->
  AilSyntax.sigma_cn_datatype list ->
  AilSyntax.sigma_cn_function list ->
  ((Locations.t * AilSyntax.sigma_declaration)
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition option)
  * AilSyntax.sigma_tag_definition option

val cn_to_ail_predicates
  :  (Sym.t * Definition.Predicate.t) list ->
  string ->
  AilSyntax.sigma_cn_datatype list ->
  (Sym.t * Ctype.ctype) list ->
  AilSyntax.sigma_cn_predicate list ->
  ((Locations.t * AilSyntax.sigma_declaration)
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list
  * AilSyntax.sigma_tag_definition option list

val cn_to_ail_pre_post
  :  without_ownership_checking:bool ->
  with_loop_leak_checks:bool ->
  string ->
  AilSyntax.sigma_cn_datatype list ->
  (Sym.t * Definition.Predicate.t) list ->
  (Sym.t * Ctype.ctype) list ->
  Ctype.ctype ->
  Extract.fn_args_and_body option ->
  ail_executable_spec

val cn_to_ail_assume_predicates
  :  string ->
  (Sym.t * Definition.Predicate.t) list ->
  AilSyntax.sigma_cn_datatype list ->
  (Sym.t * Ctype.ctype) list ->
  (Sym.t * Definition.Predicate.t) list ->
  (AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition)
    list

val cn_to_ail_assume_pre
  :  string ->
  AilSyntax.sigma_cn_datatype list ->
  Ctype.union_tag ->
  (Ctype.union_tag * (BaseTypes.t * Ctype.ctype)) list ->
  (Ctype.union_tag * Ctype.ctype) list ->
  (Ctype.union_tag * Definition.Predicate.t) list ->
  'a LogicalArgumentTypes.t ->
  AilSyntax.sigma_declaration
  * GenTypes.genTypeCategory AilSyntax.sigma_function_definition
