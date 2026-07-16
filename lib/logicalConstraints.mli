type t =
  | T of Terms.Normal.t
  | Forall of (Sym.t * BaseTypes.t) * Terms.Normal.t

val equal : t -> t -> bool

val compare : t -> t -> int

module Set : Set.S with type elt = t

val pp : t -> Pp.document

val json : t -> Yojson.Safe.t

val subst : [ `Rename of Sym.t | `Term of Terms.Normal.t ] Subst.t -> t -> t

val subst_ : (Sym.t * Terms.Normal.t) list -> t -> t

val free_vars_bts : t -> BaseTypes.t Sym.Map.t

val free_vars : t -> Sym.Set.t

val preds_of : t -> Sym.Set.t

val alpha_equivalent : t -> t -> bool

val forall_ : Sym.t * BaseTypes.t -> Terms.Normal.t -> t

val is_sym_lhs_equality : t -> (Sym.t * Terms.Normal.t) option

val is_sym_equality : t -> (Sym.t * Sym.t) option

val is_equality : t -> ((Terms.Normal.t * Terms.Normal.t) * bool) option

val equates_to : Terms.Normal.t -> t -> Terms.Normal.t option

val dtree : t -> Cerb_frontend.Pp_ast.doc_tree

val is_forall : t -> bool

val is_interesting : t -> bool

val impl : Locations.t -> Terms.Normal.t -> t -> t
