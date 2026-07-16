type output = O of Terms.Normal.t [@@unboxed]

val pp_output : output -> Pp.document

type predicate = Request.Predicate.t * output

type qpredicate = Request.QPredicate.t * output

type t = Request.t * output

val equal : t -> t -> bool

val pp : Request.t * output -> Pp.document

val json : Request.t * output -> Yojson.Safe.t

val subst : [ `Rename of Sym.t | `Term of Terms.Normal.t ] Subst.t -> t -> t

val free_vars : t -> Sym.Set.t

val derived_lc1 : t -> Terms.Normal.t list

val derived_lc2 : t -> t -> Terms.Normal.t list

val disable_resource_derived_constraints : bool ref

val pointer_facts : new_resource:t -> old_resources:t list -> Terms.Normal.t list
