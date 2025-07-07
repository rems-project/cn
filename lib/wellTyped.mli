module CTS : Set.S with type elt = Sctypes.t

val use_ity : bool ref

val add_ct : Sctypes.t -> unit

val maybe_add_ct : Sctypes.t option -> unit

val get_cts : unit -> CTS.t

type message =
  | Global of Global.message
  | Mismatch of
      { has : Pp.document;
        expect : Pp.document
      }
  | Generic of Pp.document [@deprecated "Please add a specific constructor"]
  | Illtyped_it of
      { it : Pp.document;
        has : Pp.document;
        expected : string;
        reason : string
      }
  | Number_arguments of
      { type_ : [ `Computational | `Ghost | `Other | `Input | `Output ];
        has : int;
        expect : int
      }
  | Unexpected_computational_args_in_lemma
  | Missing_member of Id.t
  | NIA of
      { it : IndexTerms.t;
        hint : string
      }
  | Empty_pattern
  | Redundant_pattern of Pp.document
  | Unknown_variable of Sym.t
  | Void_ctype of [ `Sizeof | `Array_shift | `RW | `W ]

type error =
  { loc : Locations.t;
    msg : message
  }

include WellTyped_intf.S

module type ErrorReader = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val get_context : unit -> Context.t t

  val lift : ('a, error) Result.t -> 'a t
end

module Lift : functor (M : ErrorReader) -> WellTyped_intf.S with type 'a t := 'a M.t
