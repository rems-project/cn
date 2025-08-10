let ret_sym = Sym.fresh "return"

(** Domain interface for C code generation *)
module type C_INTERFACE = sig
  val name : Pp.document

  val definitions : unit -> Pp.document
end

module type RELATIVE_VIEW = sig
  type t [@@deriving eq, ord]

  val pp : t -> Pp.document

  val pp_c : BaseTypes.t -> t -> Pp.document
end

module type T = sig
  module CInt : C_INTERFACE

  module Relative : RELATIVE_VIEW

  val name : string

  type t [@@deriving eq, ord]

  (** The bottom element of the domain *)
  val bottom : t

  (** The top element of the domain *)
  val top : t

  (** Partial order: [leq x y] holds if x ≤ y in the lattice *)
  val leq : t -> t -> bool

  (** Least upper bound (join) *)
  val join : t -> t -> t

  val join_many : t list -> t

  (** Greatest lower bound (meet) *)
  val meet : t -> t -> t

  val meet_many : t list -> t

  (* (** Widening operation to ensure fixpoint convergence.
      [widen ~prev ~next] returns an over-approximation of the union
      of [prev] and [next]. *)
  val widen : prev:t -> next:t -> t *)

  (* (** Narrowing operation to refine after widening.
      [narrow ~prev ~next] returns a refined approximation of the intersection
      of [prev] and [next]. *)
  val narrow : prev:t -> next:t -> t *)

  (** Rename a variable *)
  val rename : from:Sym.t -> to_:Sym.t -> t -> t

  (** Remove a variable *)
  val remove : Sym.t -> t -> t

  (** Retain only the provided variables *)
  val retain : Sym.Set.t -> t -> t

  val relative_to : Sym.t -> t -> Relative.t

  val free_vars : t -> Sym.Set.t

  val pp : t -> Pp.document
end

module CodeGen (CInt : C_INTERFACE) = struct
  open Pp

  let setup () =
    CInt.definitions ()
    ^^ twice hardline
    ^^ escape_lines
         (!^"#define BENNET_DOMAIN_INDIRECTION(ty)"
          ^/^ !^"bennet_domain(ty)"
          ^^^ braces (!^"bennet_domain_" ^^ CInt.name ^^ parens !^"ty" ^^^ !^"car" ^^ semi)
          ^^ semi
          ^/^ hardline
          ^^ !^"bennet_domain(ty)* bennet_domain_top_##ty(void)"
          ^^^ braces
                (!^"return (bennet_domain(ty)*)bennet_domain_"
                 ^^ CInt.name
                 ^^ !^"_top(ty);")
          ^/^ !^"bool bennet_domain_is_top_##ty(bennet_domain(ty)* cs)"
          ^^^ braces (!^"return bennet_domain_" ^^ CInt.name ^^ !^"_is_top(ty, &cs->car);")
          ^/^ hardline
          ^^ !^"bennet_domain(ty)* bennet_domain_bottom_##ty(void)"
          ^^^ braces
                (!^"return (bennet_domain(ty)*)bennet_domain_"
                 ^^ CInt.name
                 ^^ !^"_bottom(ty);")
          ^/^ !^"bool bennet_domain_is_bottom_##ty(bennet_domain(ty)* cs)"
          ^^^ braces
                (!^"return bennet_domain_" ^^ CInt.name ^^ !^"_is_bottom(ty, &cs->car);")
          ^/^ hardline
          ^^ !^"bool bennet_domain_leq_##ty(bennet_domain(ty)* cs1, bennet_domain(ty)* \
                cs2)"
          ^^^ braces
                (!^"return bennet_domain_"
                 ^^ CInt.name
                 ^^ !^"_leq_##ty(&cs1->car, &cs2->car);")
          ^/^ !^"bool bennet_domain_equal_##ty(bennet_domain(ty)* cs1, \
                 bennet_domain(ty)* cs2)"
          ^^^ braces
                (!^"return bennet_domain_"
                 ^^ CInt.name
                 ^^ !^"_equal_##ty(&cs1->car, &cs2->car);")
          ^/^ hardline
          ^^ !^"bennet_domain(ty)* bennet_domain_join_##ty(bennet_domain(ty)* cs1, \
                bennet_domain(ty)* cs2)"
          ^^^ braces
                (!^"return (bennet_domain(ty)*)bennet_domain_"
                 ^^ CInt.name
                 ^^ !^"_join_##ty(&cs1->car, &cs2->car);")
          ^/^ !^"bennet_domain(ty)* bennet_domain_meet_##ty(bennet_domain(ty)* cs1, \
                 bennet_domain(ty)* cs2)"
          ^^^ braces
                (!^"return (bennet_domain(ty)*)bennet_domain_"
                 ^^ CInt.name
                 ^^ !^"_meet_##ty(&cs1->car, &cs2->car);")
          ^/^ hardline
          ^^ !^"bennet_domain(ty)* bennet_domain_copy_##ty(bennet_domain(ty)* cs)"
          ^^^ braces
                (!^"return (bennet_domain(ty)*)bennet_domain_"
                 ^^ CInt.name
                 ^^ !^"_copy_##ty(&cs->car);")
          ^/^ !^"ty bennet_domain_arbitrary_##ty(bennet_domain(ty)* cs)"
          ^^^ braces
                (!^"return bennet_domain_" ^^ CInt.name ^^ !^"_arbitrary_##ty(&cs->car);")
          ^/^ hardline
          ^^ !^"bennet_domain(ty)* bennet_domain_from_assignment_##ty(void *base_ptr, \
                void *addr, size_t bytes)"
          ^^^ braces
                (!^"return (bennet_domain(ty)*)bennet_domain_"
                 ^^ CInt.name
                 ^^ !^"_from_assignment_##ty(base_ptr, addr, bytes);")
          ^/^ empty)
    ^/^ hardline
    ^^ separate_map
         hardline
         (fun ty -> !^"BENNET_DOMAIN_INDIRECTION" ^^ parens !^ty)
         [ "int8_t";
           "uint8_t";
           "int16_t";
           "uint16_t";
           "int32_t";
           "uint32_t";
           "int64_t";
           "uint64_t";
           "uintptr_t"
         ]
    ^/^ hardline
end
