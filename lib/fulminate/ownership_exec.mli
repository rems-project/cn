module CF = Cerb_frontend
module A = Executable_spec_utils.A
module C = Executable_spec_utils.C

type ownership_mode =
  | Pre
  | Post
  | Loop

val ownership_mode_to_enum_str : ownership_mode -> string

val cn_stack_depth_incr_sym : Sym.t

val cn_stack_depth_decr_sym : Sym.t

val cn_postcondition_leak_check_sym : Sym.t

val cn_loop_put_back_ownership_sym : Sym.t

val cn_loop_leak_check_and_put_back_ownership_sym : Sym.t

val get_ownership_global_init_stats
  :  unit ->
  Cerb_frontend.GenTypes.genTypeCategory A.statement_ list

val generate_c_local_ownership_entry_fcall
  :  A.ail_identifier * Executable_spec_utils.C.ctype ->
  Cerb_frontend.GenTypes.genTypeCategory Executable_spec_utils.A.expression

val generate_c_local_ownership_exit
  :  A.ail_identifier * Executable_spec_utils.C.ctype ->
  Cerb_frontend.GenTypes.genTypeCategory A.statement_

val get_c_fn_local_ownership_checking_injs
  :  C.union_tag ->
  CF.GenTypes.genTypeCategory A.sigma ->
  (((C.union_tag
    * ((Cerb_location.t * A.storageDuration * bool) * 'a option * C.qualifiers * C.ctype))
      list
   * CF.GenTypes.genTypeCategory A.statement_ list)
  * ('b list * CF.GenTypes.genTypeCategory A.statement_ list))
    option
  * (Cerb_location.t
    * CF.GenTypes.genTypeCategory A.expression option option
    * (C.union_tag
      * ((Cerb_location.t * A.storageDuration * bool)
        * 'c option
        * C.qualifiers
        * C.ctype))
        list
    * CF.GenTypes.genTypeCategory A.statement_ list)
      list
