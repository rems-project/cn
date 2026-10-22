module CF = Cerb_frontend
module BT = BaseTypes

module IM = struct
  include CF.Impl_mem
  include CF.Mem
end

module OI = CF.Ocaml_implementation
open Sctypes

(* ... adapting from impl_mem *)

let bits_per_byte = 8

let z_of_ival iv = Option.get (IM.eval_integer_value iv)

let int_of_ival iv = Z.to_int (Option.get (IM.eval_integer_value iv))

let size_of_integer_type it = Option.get ((OI.get ()).sizeof_ity it)

let align_of_integer_type it = Option.get ((OI.get ()).alignof_ity it)

let is_signed_integer_type ity = (OI.get ()).is_signed_ity ity

let max_integer_type it = z_of_ival (CF.Impl_mem.max_ival it)

let min_integer_type it = z_of_ival (CF.Impl_mem.min_ival it)

let size_of_pointer = Option.get (OI.get ()).sizeof_pointer

let align_of_pointer = Option.get (OI.get ()).alignof_pointer

let max_pointer =
  let pointer_bits = size_of_pointer * bits_per_byte in
  Z.sub (Z.pow (Z.of_int 2) pointer_bits) (Z.of_int 1)


let bt_of_sct = BT.of_sct is_signed_integer_type size_of_integer_type

let sbt_of_sct = BaseTypes.Surface.of_sct is_signed_integer_type size_of_integer_type

let uintptr_bt = BT.uintptr_bt is_signed_integer_type size_of_integer_type

let intptr_bt = BT.intptr_bt is_signed_integer_type size_of_integer_type

let size_bt = BT.size_bt is_signed_integer_type size_of_integer_type

let size_sbt = BaseTypes.Surface.size_bt is_signed_integer_type size_of_integer_type

let sint_bt = bt_of_sct Sctypes.(Integer (Signed Int_))

let sint_sbt = sbt_of_sct Sctypes.(Integer (Signed Int_))

let size_of_ctype = function
  | Void -> Cerb_diagnostic.fatal "size_of_ctype applied to void"
  | ct -> Z.to_int (OI.sizeof !Sctypes.tagDefs_ref (to_ctype ct))


let align_of_ctype = function
  | Void -> 1
  | Function _ -> 1
  | ct -> Z.to_int (OI.alignof !Sctypes.tagDefs_ref (Sctypes.to_ctype ct))


type struct_piece =
  { offset : int;
    size : int;
    member_or_padding : (Id.t * Sctypes.t) option
  }

type struct_member =
  { offset : int;
    size : int;
    member : Id.t * Sctypes.t
  }

type flexible_array_member =
  { offset : int;
    member : Id.t * Sctypes.t
  }

type struct_layout = struct_piece list * flexible_array_member option

type struct_decl = struct_layout

type struct_decls = struct_layout Sym.Map.t

let members ((pieces, fam) : struct_layout) =
  ( List.filter_map
      (fun { member_or_padding; _ } -> Option.map fst member_or_padding)
      pieces,
    Option.map (fun f -> fst f.member) fam )


let member_types ((pieces, fam) : struct_layout) =
  ( List.filter_map (fun { member_or_padding; _ } -> member_or_padding) pieces,
    Option.map (fun f -> f.member) fam )


(* let member_number layout member = *)
(*   let rec aux i layout = *)
(*     match layout with *)
(*     | [] -> None *)
(*     | sp :: layout -> *)
(*       (match sp.member_or_padding with *)
(*        | Some (member', _) when Id.equal member member' -> Some i *)
(*        | Some (_, _) -> aux (i + 1) layout *)
(*        | None -> aux i layout) *)
(*   in *)
(*   aux 0 layout *)

let member_offset ((pieces, fam) : struct_layout) member : int option =
  let found_in_pieces =
    List.find_map
      (fun sp ->
         match sp.member_or_padding with
         | Some (member', _) when Id.equal member member' -> Some sp.offset
         | _ -> None)
      pieces
  in
  match (found_in_pieces, fam) with
  | Some i, _ -> Some i
  | None, Some { member = member', _; offset } when Id.equal member member' -> Some offset
  | _ -> None


(* tries to implement all_values_representable_in from runtime std.core *)
let all_values_representable_in (ity1, ity2) =
  Z.leq (min_integer_type ity2) (min_integer_type ity1)
  && Z.leq (max_integer_type ity1) (max_integer_type ity2)
