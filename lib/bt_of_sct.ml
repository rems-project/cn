open BaseTypes
open Sctypes

module type Memory = sig
  val is_signed_integer_type : IntegerTypes.t -> bool
  val size_of_integer_type : IntegerTypes.t -> int
end

module type Mode = sig
  val bvmode : bool
end

module type Repr = sig

  val bvmode : bool

  val bt_of_sct : Sctypes.t -> BaseTypes.t
  val sbt_of_sct : Sctypes.t -> BaseTypes.Surface.t

  val uintptr_bt : BaseTypes.t
  val uintptr_sbt : BaseTypes.Surface.t

  val intptr_bt : BaseTypes.t
  val intptr_sbt : BaseTypes.Surface.t

  val size_bt : BaseTypes.t
  val size_sbt : BaseTypes.Surface.t

  val sint_bt : BaseTypes.t
  val sint_sbt : BaseTypes.Surface.t

end


module F (Memory: Memory) (Mode: Mode) : Repr = struct

  open Memory

  let bvmode = Mode.bvmode

  let of_sct loc = 
    let rec aux = function
      | Sctypes.Void -> Unit
      | Integer ity ->
	if bvmode then
	  Bits ((if is_signed_integer_type ity then Signed else Unsigned), size_of_integer_type ity * 8)
	else
	  Integer
      | Array (sct, _) ->
	Map (aux Sctypes.(Integer (Unsigned Intptr_t)), aux sct)
      | Pointer sct -> Loc (loc sct)
      | Struct tag -> Struct tag
      | Byte -> Option MemByte
      | Function _ -> Cerb_debug.error "todo: function types"
    in
    aux

  let bt_of_sct = of_sct (Fun.const ())
  let sbt_of_sct = of_sct Option.some

  let uintptr_bt = bt_of_sct (Integer (Unsigned Intptr_t))
  let uintptr_sbt = sbt_of_sct (Integer (Unsigned Intptr_t))

  let intptr_bt = bt_of_sct (Integer (Signed Intptr_t))
  let intptr_sbt = sbt_of_sct (Integer (Signed Intptr_t))

  let size_bt = bt_of_sct (Integer Size_t)
  let size_sbt = sbt_of_sct (Integer Size_t)

  let sint_bt = bt_of_sct (Integer (Signed Int_))
  let sint_sbt = sbt_of_sct (Integer (Signed Int_))

end

module BV = F(Memory)(struct let bvmode = true end)
module Z = F(Memory)(struct let bvmode = false end)
