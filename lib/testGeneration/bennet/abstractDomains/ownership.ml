(** Ownership domain for tracking memory ownership information.
   
   This module implements an abstract domain that tracks ownership of memory
   locations through front/back byte counts. The domain maps symbols to pairs
   of integers representing owned bytes before and after a memory location.
   
   - None represents bottom (no ownership information)
   - Some empty_map represents top (all possible ownerships)
   - Some map tracks specific ownership bounds per symbol
*)

module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

module Inner = struct
  let name = "ownership"

  module CInt : Domain.C_INTERFACE = struct
    let name = name

    open Pp

    let definitions () = empty
  end

  module Relative = struct
    type t = (BT.t * (int * int)) option [@@deriving eq, ord]

    let name = name

    let is_top oo =
      match oo with
      | Some (_, (before, after)) -> before == 0 && after == 0
      | None -> false


    let is_bottom oo = Option.is_none oo

    let pp ob =
      let open Pp in
      match ob with
      | Some (_, (0, 0)) -> !^"⊤"
      | Some (_, (front, back)) ->
        !^"owns" ^^^ int front ^^^ !^"bytes in front and" ^^^ int back ^^^ !^"bytes after"
      | None -> !^"⊥"


    let pp_args oo =
      (* Should only be called on non-top/bottom *)
      let _, (before, after) = Option.get oo in
      string_of_int before ^ ", " ^ string_of_int after


    let to_it (oo : t) (sym : Sym.t) : IT.t =
      let loc = Locations.other __LOC__ in
      match oo with
      | Some (_, (0, 0)) | None -> IT.bool_ true loc
      | Some (bt, (_, _)) ->
        IT.not_
          (IT.eq_ (IT.cast_ (BT.Loc ()) (IT.sym_ (sym, bt, loc)) loc, IT.null_ loc) loc)
          loc


    let to_lc (oo : t) (sym : Sym.t) : LC.t = LC.T (to_it oo sym)
  end

  (* Type represents ownership information as optional map from symbols to (front, back) byte counts *)
  type t = (BT.t * (int * int)) Sym.Map.t option [@@deriving eq, ord]

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
        (fun x (_, (front, back)) ->
           match Sym.Map.find_opt x d2 with
           | Some (_, (front', back')) -> front >= front' && back >= back'
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
              | Some (bt, (front1, back1)), Some (_, (front2, back2)) ->
                (* Take minimum bounds - more permissive ownership *)
                Some (bt, (min front1 front2, min back1 back2))
              | None, _ | _, None -> None)
           d1
           d2)
    | None, d | d, None -> d


  let join_many ds = List.fold_left join bottom ds

  (* Meet (greatest lower bound): combines ownership by taking maximum bounds
     This represents the least restrictive ownership that satisfies both inputs *)
  let meet d1 d2 =
    match (d1, d2) with
    | Some d1, Some d2 ->
      Some
        (Sym.Map.merge
           (fun _ o1 o2 ->
              match (o1, o2) with
              | Some (bt, (front1, back1)), Some (_, (front2, back2)) ->
                (* Take maximum bounds - more restrictive ownership *)
                Some (bt, (max front1 front2, max back1 back2))
              | None, o | o, None -> o) (* include ownership from either side *)
           d1
           d2)
    | None, _ | _, None -> None (* meet with bottom gives bottom *)


  let meet_many ds = List.fold_left meet top ds

  let rename ~(from : Sym.t) ~(to_ : Sym.t) (d : t) =
    let open Option in
    let@ d = d in
    match Sym.Map.find_opt from d with
    | Some o -> return (Sym.Map.add to_ o (Sym.Map.remove from d))
    | None -> return d


  let remove (x : Sym.t) (d : t) : t =
    let open Option in
    let@ d = d in
    return (Sym.Map.remove x d)


  let retain (xs : Sym.Set.t) (d : t) : t =
    let open Option in
    let@ d = d in
    return (Sym.Map.filter (fun x _ -> Sym.Set.mem x xs) d)


  let relative_to (x : Sym.t) (bt : BaseTypes.t) (d : t) : Relative.t =
    let open Option in
    let@ d = d in
    return (Option.value ~default:(bt, (0, 0)) (Sym.Map.find_opt x d))


  let free_vars (od : t) =
    match od with
    | Some d -> Sym.Set.of_seq (List.to_seq (List.map fst (Sym.Map.bindings d)))
    | None -> Sym.Set.empty


  let free_vars_bts (od : t) : BaseTypes.t Sym.Map.t =
    match od with Some d -> Sym.Map.map (fun (bt, _) -> bt) d | None -> Sym.Map.empty


  let pp d =
    let open Pp in
    match d with
    | Some d when Sym.Map.is_empty d -> !^"⊤"
    | Some d ->
      separate_map
        (semi ^^ break 1)
        (fun (x, (_, (front, back))) ->
           Sym.pp x
           ^^^ !^"owns"
           ^^^ int front
           ^^^ !^"bytes in front and"
           ^^^ int back
           ^^^ !^"bytes after")
        (List.filter
           (fun (_, (_, (front, back))) -> not (front = 0 && back = 0))
           (Sym.Map.bindings d))
    | None -> !^"⊥"


  let abs_assert _ d : t = d

  let abs_assign (((it_addr, sct), _) : (IT.t * Sctypes.t) * IT.t) (d : t) : t =
    let rec pointer_and_offset (it : IT.t) : ((Sym.t * BaseTypes.t) * int) option =
      let open Option in
      match it with
      | IT (CopyAllocId { loc = ptr; _ }, _, _) ->
        let@ p, offset = pointer_and_offset ptr in
        return (p, offset)
      | IT (MemberShift (base, tag, member), _, loc) ->
        pointer_and_offset
          (IT.pointer_offset_
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
    match pointer_and_offset it_addr with
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
        meet (Some (Sym.Map.singleton p (p_bt, (-start_offset, max 0 end_offset)))) d
      else (* If [start_offset] is positive, [end_offset] must be positive *)
        meet (Some (Sym.Map.singleton p (p_bt, (0, end_offset)))) d
    | None -> d


  let pp_params () = "size_t before, size_t after"

  let pp_args () = "before, after"

  let to_it (d : t) : IT.t =
    let loc = Locations.other __LOC__ in
    match d with
    | None -> IT.bool_ false loc (* bottom = unsatisfiable *)
    | Some _ -> IT.bool_ true loc (* ownership has no numeric constraints to express *)


  let to_lc (d : t) : LC.t = LC.T (to_it d)

  let is_meet_assoc = true

  let is_join_assoc = true
end
