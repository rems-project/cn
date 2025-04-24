type env

type 'a cerb_frontend_result =
  ('a, Locations.t * Cerb_frontend.Errors.cause) Cerb_frontend.Exception.exceptM

val init
  :  (Sym.t, Mucore.tag_definition) Pmap.map ->
  (Locations.t -> Sym.t -> unit Cerb_frontend.AilSyntax.expression cerb_frontend_result) ->
  (Locations.t -> Sym.t -> Cerb_frontend.Ctype.ctype cerb_frontend_result) ->
  env

val exec_spec_hack_syms : BaseTypes.Surface.t Hashtbl.Make(Sym).t

val add_computational : Sym.t -> BaseTypes.Surface.t -> env -> env

val add_renamed_computational : Sym.t -> Sym.t -> BaseTypes.Surface.t -> env -> env

val add_logical : Sym.t -> BaseTypes.Surface.t -> env -> env

val base_type : env -> Sym.t Cerb_frontend.Cn.cn_base_type -> BaseTypes.Surface.t

val add_predicates
  :  env ->
  (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_predicate list ->
  env

type message =
  | Builtins of Builtins.message
  | Global of Global.message
  | WellTyped of WellTyped.message
  | Cannot_convert_enum_const of Cerb_frontend.AilSyntax.constant
  | Cannot_convert_enum_expr of unit Cerb_frontend.AilSyntax.expression
  | Cerb_frontend of Locations.t * Cerb_frontend.Errors.cause
  | Illtyped_binary_it of
      { left : IndexTerms.Surface.t;
        right : IndexTerms.Surface.t;
        binop : Cerb_frontend.Cn.cn_binop
      }
  | First_iarg_missing
  | First_iarg_not_pointer of
      { pname : Request.name;
        found_bty : BaseTypes.t
      }
  | Datatype_repeated_member of Id.t
  | No_pointee_ctype of IndexTerms.Surface.t
  | Each_quantifier_not_numeric of Sym.t * BaseTypes.Surface.t
  | Generic of Pp.document [@deprecated "Temporary, for refactor, to be deleted."]

type err =
  { loc : Locations.t;
    msg : message
  }

module Or_Error : sig
  type 'a t = ('a, err) Result.t
end

val add_user_defined_functions
  :  env ->
  (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_function list ->
  env Or_Error.t

val add_datatypes : env -> Sym.t Cerb_frontend.Cn.cn_datatype list -> env Or_Error.t

module C_vars : sig
  type state =
    | Value of Sym.t * BaseTypes.Surface.t
    | Points_to of IndexTerms.Surface.t

  type name

  type named_scopes

  type env

  val init : env

  val start : name

  val get_old_scopes : env -> named_scopes

  (** This is only called with [start] so I'm not really sure about then
     intention of the API here. *)
  val push_scope : env -> name -> env

  val add : (Sym.t * state) list -> env -> env

  val add_pointee_values
    :  (IndexTerms.Surface.t * IndexTerms.Surface.t) list ->
    env ->
    env
end

val expr
  :  Sym.Set.t ->
  env ->
  C_vars.env ->
  (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_expr ->
  IndexTerms.Surface.t Or_Error.t

val let_resource
  :  env ->
  C_vars.env ->
  Sym.t * (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_resource ->
  ((Request.t * BaseTypes.Surface.t)
  * (LogicalConstraints.t * (Locations.t * string option)) list
  * (IndexTerms.Surface.t * IndexTerms.Surface.t) list)
    Or_Error.t

val assrt
  :  env ->
  C_vars.env ->
  Locations.t * (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_assertion ->
  LogicalConstraints.t Or_Error.t

val function_
  :  env ->
  (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_function ->
  (Sym.t * Definition.Function.t) Or_Error.t

val ownership
  :  Locations.t * (Sym.t * Cerb_frontend.Ctype.ctype) ->
  env ->
  (Cerb_frontend__Symbol.sym
  * ((Request.t * BaseTypes.Surface.t)
    * (LogicalConstraints.t * (Locations.t * string option)) list)
  * BaseTypes.Surface.t IndexTerms.annot)
    Or_Error.t

val allocation_token
  :  Locations.t ->
  Sym.t ->
  (Cerb_frontend__Symbol.sym * (Request.t * BaseTypes.t)) * (Locations.t * 'a option)

val predicate
  :  env ->
  (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_predicate ->
  (Sym.t * Definition.Predicate.t) Or_Error.t

val return_type
  :  Locations.t ->
  env ->
  C_vars.env ->
  Sym.t * Cerb_frontend.Ctype.ctype ->
  (Locations.t * (Sym.t * Cerb_frontend.Ctype.ctype)) list
  * (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_condition list ->
  ReturnTypes.t Or_Error.t

val lemma
  :  env ->
  (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_lemma ->
  (Sym.t * (Cerb_location.t * LogicalReturnTypes.t ArgumentTypes.t)) Or_Error.t

val statement
  :  (Sym.t -> Cerb_frontend.Ctype.ctype) ->
  C_vars.named_scopes ->
  env ->
  (Sym.t, Cerb_frontend.Ctype.ctype) Cerb_frontend.Cn.cn_statement ->
  Cnprog.t Or_Error.t
