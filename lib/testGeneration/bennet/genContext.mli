module Make (GT : GenTerms.T) : sig
  type t = (Sym.t * GenDefinitions.Make(GT).t) list [@@deriving eq, ord]

  val empty : t

  val find : Sym.t -> t -> GenDefinitions.Make(GT).t

  val find_opt : Sym.t -> t -> GenDefinitions.Make(GT).t option

  val pp : t -> Pp.document

  val get_call_graph : t -> Sym.Digraph.t
end

module MakeOptional (GT : GenTerms.T) : sig
  type t = (Sym.t * GenDefinitions.MakeOptional(GT).t) list [@@deriving eq, ord]

  val empty : t

  val find : Sym.t -> t -> GenDefinitions.MakeOptional(GT).t

  val find_opt : Sym.t -> t -> GenDefinitions.MakeOptional(GT).t option

  val pp : t -> Pp.document

  val add : GenDefinitions.MakeOptional(GT).t -> t -> t

  val drop_nones : t -> Make(GT).t

  val get_call_graph : t -> Sym.Digraph.t
end
