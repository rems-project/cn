open Cerb_frontend

type ail_bindings_and_statements =
  AilSyntax.bindings * GenTypes.genTypeCategory AilSyntax.statement_ list

type return_kind =
  | ReturnVoid
  | ReturnExpr of GenTypes.genTypeCategory AilSyntax.expression

type injection_kind =
  | ReturnInj of return_kind
  | NonReturnInj

type ownership_injection =
  { loc : Cerb_location.t;
    bs_and_ss : ail_bindings_and_statements;
    injection_kind : injection_kind
  }

val cn_stack_depth_incr_sym : Sym.t

val cn_stack_depth_decr_sym : Sym.t

val cn_postcondition_leak_check_sym : Sym.t

val cn_loop_put_back_ownership_sym : Sym.t

val cn_loop_leak_check_sym : Sym.t

val generate_c_local_ownership_entry
  :  is_register:bool ->
  AilSyntax.ail_identifier * Ctype.ctype ->
  GenTypes.genTypeCategory AilSyntax.statement_ option

val generate_c_local_ownership_exit
  :  is_register:bool ->
  AilSyntax.ail_identifier * Ctype.ctype ->
  GenTypes.genTypeCategory AilSyntax.statement_ option

val get_c_fn_local_ownership_checking_injs
  :  contains_user_spec:bool ->
  Config.config ->
  Ctype.union_tag ->
  GenTypes.genTypeCategory AilSyntax.sigma ->
  (((Ctype.union_tag
    * ((Cerb_location.t * AilSyntax.storageDuration * bool)
      * 'a option
      * Ctype.qualifiers
      * Ctype.ctype))
      list
   * GenTypes.genTypeCategory AilSyntax.statement_ list)
  * ('b list * GenTypes.genTypeCategory AilSyntax.statement_ list))
    option
  * (ownership_injection list * (Cerb_location.t * string list) list)
