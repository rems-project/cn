type l_info = Locations.t * Pp.document Lazy.t

val pp_l_info : Pp.document -> l_info -> Pp.document

type basetype_or_value =
  | BaseType of BaseTypes.t
  | Value of IndexTerms.t

val bt_of : basetype_or_value -> BaseTypes.t

val has_value : basetype_or_value -> bool

type t =
  { computational : (basetype_or_value * l_info) Sym.Map.t;
    logical : (basetype_or_value * l_info) Sym.Map.t;
    resources : Resource.t list;
    constraints : LogicalConstraints.Set.t;
    global : Global.t;
    where : Where.t
  }

val empty : t

val get_rs : t -> Resource.t list

val pp_basetype_or_value : basetype_or_value -> Pp.document

val pp_variable_bindings : (basetype_or_value * 'a) Sym.Map.t -> Pp.document

val pp_constraints : LogicalConstraints.Set.t -> Pp.document

val pp : t -> Pp.document

val bound_a : Sym.t -> t -> bool

val bound_l : Sym.t -> t -> bool

val bound : Sym.t -> t -> bool

val get_a : Sym.t -> t -> basetype_or_value

val get_l : Sym.t -> t -> basetype_or_value

val add_a_binding : Sym.t -> basetype_or_value -> l_info -> t -> t

val add_a : Sym.t -> BaseTypes.t -> l_info -> t -> t

val add_a_value : Sym.t -> IndexTerms.t -> l_info -> t -> t

val add_l_binding : Sym.t -> basetype_or_value -> l_info -> t -> t

val add_l : Sym.t -> BaseTypes.t -> l_info -> t -> t

val add_l_value : Sym.t -> IndexTerms.t -> l_info -> t -> t

val remove_a : Sym.t -> t -> t

val add_c : LogicalConstraints.Set.elt -> t -> t

val modify_where : (Where.t -> Where.t) -> t -> t

val add_r : Locations.t -> Resource.t -> t -> t

val not_given_to_solver
  :  t ->
  LogicalConstraints.t list
  * (Sym.t * Definition.Function.t) list
  * (Sym.t * Definition.Predicate.t) list
