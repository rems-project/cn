(** Ownership domain for tracking memory ownership information.
   
   This module implements an abstract domain that tracks ownership of memory
   locations through front/back byte counts. The domain maps symbols to pairs
   of integers representing owned bytes before and after a memory location.
   
   - None represents bottom (no ownership information)
   - Some empty_map represents top (all possible ownerships)
   - Some map tracks specific ownership bounds per symbol
*)
open struct
  module Inner = struct
    let name = "ownership"

    module CInt : Domain.C_INTERFACE = struct
      open Pp

      let name = !^name

      let definitions () = empty
    end

    (* Type represents ownership information as optional map from symbols to (front, back) byte counts *)
    type t = (int * int) Sym.Map.t option [@@deriving eq, ord]

    let bottom = None

    let top = Some Sym.Map.empty

    (* Partial order: d1 ≤ d2 if d1 is more restrictive than d2
     Bottom is ≤ everything; for maps, d1 ≤ d2 if d1 has wider ownership bounds *)
    let leq d1 d2 =
      if Option.is_none d1 then
        true
      else if Option.is_none d2 then
        false
      else (
        let d1 = Option.get d1 in
        let d2 = Option.get d2 in
        (* d1 ≤ d2 if for each symbol in d1, its ownership bounds are >= those in d2 *)
        Sym.Map.for_all
          (fun x (front, back) ->
             match Sym.Map.find_opt x d2 with
             | Some (front', back') -> front >= front' && back >= back'
             | None -> true)
          d1)


    (* Join (least upper bound): combines ownership information by taking minimum bounds
     This represents the least permissive ownership that satisfies both inputs *)
    let join d1 d2 =
      match (d1, d2) with
      | Some d1, Some d2 ->
        Some
          (Sym.Map.merge
             (fun _ o1 o2 ->
                match (o1, o2) with
                | Some (front1, back1), Some (front2, back2) ->
                  (* Take minimum bounds - more permissive ownership *)
                  Some (min front1 front2, min back1 back2)
                | None, _ | _, None -> None)
             d1
             d2)
      | None, d | d, None -> d


    (* Meet (greatest lower bound): combines ownership by taking maximum bounds
     This represents the least restrictive ownership that satisfies both inputs *)
    let meet d1 d2 =
      match (d1, d2) with
      | Some d1, Some d2 ->
        Some
          (Sym.Map.merge
             (fun _ o1 o2 ->
                match (o1, o2) with
                | Some (front1, back1), Some (front2, back2) ->
                  (* Take maximum bounds - more restrictive ownership *)
                  Some (max front1 front2, max back1 back2)
                | None, o | o, None -> o)
             d1
             d2)
      | None, _ | _, None -> None


    let rename ~(from : Sym.t) ~(to_ : Sym.t) (d : t) =
      let open Option in
      let@ d = d in
      let o = Sym.Map.find from d in
      return (Sym.Map.add to_ o (Sym.Map.remove from d))


    let remove (x : Sym.t) (d : t) : t =
      let open Option in
      let@ d = d in
      return (Sym.Map.remove x d)


    let retain (xs : Sym.Set.t) (d : t) : t =
      let open Option in
      let@ d = d in
      return (Sym.Map.filter (fun x _ -> not (Sym.Set.mem x xs)) d)


    let pp d =
      let open Pp in
      match d with
      | Some d when Sym.Map.is_empty d -> !^"⊤"
      | Some d ->
        separate_map
          (semi ^^ break 1)
          (fun (x, (front, back)) ->
             Sym.pp x
             ^^^ !^"owns"
             ^^^ int front
             ^^^ !^"bytes in front and"
             ^^^ int back
             ^^^ !^"bytes after")
          (Sym.Map.bindings d)
      | None -> !^"⊥"
  end
end

include Inner

module Interpreter : Interpreter.Part = struct
  module AD = Inner

  (** Abstract interpretation of statements *)
  let abs_stmt
        (ctx : t Sym.Map.t)
        (tm : ('tag, [< ('tag, 'recur) GenTerms.Make(AD).Inner.ast ]) GenTerms.annot)
        (d : t)
    : t
    =
    let (GenTerms.Annot (tm_, _tag, _bt, _loc)) = tm in
    match tm_ with
    | `Arbitrary _ | `LetStar _ | `Return _ | `Assert _ | `ITE _ | `Map _ | `Pick _
    | `PickSized _ | `SplitSize _ | `MapElab _ | `PickSizedElab _ | `SplitSizeElab _ ->
      d
    | `Call (fsym, _) | `CallSized (fsym, _, _) ->
      (match Sym.Map.find_opt fsym ctx with Some d' -> meet d d' | None -> d)
    | `Asgn ((it_addr, sct), _, _) | `AsgnElab (_, ((_, it_addr), sct), _, _) ->
      let rec pointer_and_offset (it : IndexTerms.t)
        : ((Sym.t * BaseTypes.t) * int) option
        =
        let open Option in
        match it with
        | IT (CopyAllocId { loc = ptr; _ }, _, _) ->
          let@ p, offset = pointer_and_offset ptr in
          return (p, offset)
        | IT (MemberShift (base, tag, member), _, loc) ->
          pointer_and_offset
            (IndexTerms.pointer_offset_
               (base, IT (OffsetOf (tag, member), Memory.size_bt, loc))
               loc)
        | IT (ArrayShift { base = ptr; ct; index = IT (Const (Z n), _, _) }, _, _)
        | IT (ArrayShift { base = ptr; ct; index = IT (Const (Bits (_, n)), _, _) }, _, _)
          ->
          let@ p, offset = pointer_and_offset ptr in
          return (p, offset + (Memory.size_of_ctype ct * Z.to_int n))
        | IT (Sym x, bt, _) | IT (Cast (_, IT (Sym x, bt, _)), _, _) -> return ((x, bt), 0)
        | _ -> None
      in
      (match pointer_and_offset it_addr with
       | Some ((p, p_bt), start_offset) ->
         let () =
           match p_bt with
           | Loc () -> ()
           | Bits (_, n) -> assert (n >= Memory.size_of_pointer)
           | _ -> failwith ("Unsupported type '" ^ Pp.plain (BaseTypes.pp p_bt) ^ "'")
         in
         let end_offset = start_offset + Memory.size_of_ctype sct in
         if start_offset < 0 then
           (* If [start_offset] is negative, [end_offset] might be negative *)
           meet (Some (Sym.Map.singleton p (-start_offset, max 0 end_offset))) d
         else (* If [start_offset] is positive, [end_offset] must be positive *)
           meet (Some (Sym.Map.singleton p (0, end_offset))) d
       | None -> d)
end

module C : Domain.C_INTERFACE = struct
  open Pp

  let name = !^"ownership"

  let definitions () = empty
end
