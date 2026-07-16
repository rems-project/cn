module Function : sig
  type body =
    | Def of Terms.Normal.t
    | Rec_Def of Terms.Normal.t
    | Uninterp

  val subst_body : [ `Rename of Sym.t | `Term of Terms.Normal.t ] Subst.t -> body -> body

  type t =
    { loc : Locations.t;
      args : (Sym.t * BaseTypes.t) list;
      return_bt : BaseTypes.t;
      emit_coq : bool;
      body : body
    }

  val is_recursive : t -> bool

  val given_to_solver : t -> bool

  val pp_args : (Cerb_frontend.Symbol.sym * unit BaseTypes.t_gen) list -> Pp.document

  val pp_sig : Pp.document -> t -> Pp.document

  val pp : Pp.document -> t -> Pp.document

  val open_ : (Sym.t * 'a) list -> Terms.Normal.t -> Terms.Normal.t list -> Terms.Normal.t

  val unroll_once : t -> Terms.Normal.t list -> Terms.Normal.t option

  val try_open : t -> Terms.Normal.t list -> Terms.Normal.t option

  val is_interesting : t -> bool
end

module Clause : sig
  type t =
    { loc : Locations.t;
      guard : Terms.Normal.t;
      packing_ft : LogicalArgumentTypes.packing_ft
    }

  val pp : t -> Pp.document

  val subst : [ `Rename of Sym.t | `Term of Terms.Normal.t ] Subst.t -> t -> t

  val lrt : Terms.Normal.t -> Terms.Normal.t LogicalArgumentTypes.t -> LogicalReturnTypes.t

  val explicit_negative_guards : t list -> t list

  val free_vars : t -> Sym.Set.t
end

module Predicate : sig
  type t =
    { loc : Locations.t;
      pointer : Sym.t;
      iargs : (Sym.t * BaseTypes.t) list;
      oarg : Locations.t * BaseTypes.t;
      clauses : Clause.t list option;
      recursive : bool;
      attrs : Id.t list
    }

  val is_nounfold : t -> bool

  val is_multiclause : t -> bool

  val pp : t -> Pp.document

  val instantiate : t -> Terms.Normal.t -> Terms.Normal.t list -> Clause.t list option

  val identify_right_clause
    :  (LogicalConstraints.t -> [< `False | `True ]) ->
    t ->
    Terms.Normal.t ->
    Terms.Normal.t list ->
    Clause.t option

  val given_to_solver : t -> bool

  val free_vars : t -> Sym.Set.t
end

val alloc : Predicate.t

val is_interesting : Predicate.t -> bool
