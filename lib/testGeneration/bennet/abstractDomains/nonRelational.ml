module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

module type BASIS = sig
  val name : string

  type t [@@deriving eq, ord]

  val bt : t -> BT.t

  val bottom : BT.t -> t

  val top : BT.t -> t

  val leq : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val of_interval : BT.t -> Z.t -> Z.t -> t

  val forward_abs_it : IT.t -> t list -> t

  val backward_abs_it : IT.t -> t list -> t list

  val widen : t -> t -> t

  val narrow : t -> t -> t

  val pp : t -> Pp.document

  val pp_c : t -> Pp.document
end

module Make (B : BASIS) = struct
  let name = B.name

  module CInt : Domain.C_INTERFACE = struct
    open Pp

    let name = !^name

    let definitions () = empty
  end

  module Relative = struct
    type t = B.t option [@@deriving eq, ord]

    let pp ob = match ob with Some b -> B.pp b | None -> Pp.string "⊥"

    let pp_c (_ : BT.t) = Option.pp B.pp_c
  end

  type t = B.t Sym.Map.t option [@@deriving eq, ord]

  let bottom = None

  let top = Some Sym.Map.empty

  let leq od1 od2 =
    match (od1, od2) with
    | None, _ -> true
    | _, None -> false
    | Some d1, Some d2 ->
      Sym.Map.bindings d2
      |> List.for_all (fun (x, b2) ->
        match Sym.Map.find_opt x d1 with
        | Some b1 ->
          assert (BT.equal (B.bt b1) (B.bt b2));
          B.leq b1 b2
        | None -> false)


  let join od1 od2 : t =
    match (od1, od2) with
    | None, d | d, None -> d
    | Some d1, Some d2 ->
      Some
        (Sym.Map.merge
           (fun _ ob1 ob2 ->
              match (ob1, ob2) with
              | Some b1, Some b2 ->
                assert (BT.equal (B.bt b1) (B.bt b2));
                Some (B.join b1 b2)
              | None, _ | _, None -> None)
           d1
           d2)


  let join_many (l : t list) = List.fold_left join bottom l

  let meet od1 od2 : t =
    match (od1, od2) with
    | None, _ | _, None -> None
    | Some d1, Some d2 ->
      Some
        (Sym.Map.merge
           (fun _ ob1 ob2 ->
              match (ob1, ob2) with
              | Some b1, Some b2 ->
                assert (BT.equal (B.bt b1) (B.bt b2));
                Some (B.meet b1 b2)
              | None, Some b | Some b, None -> Some b
              | None, None -> None)
           d1
           d2)


  let meet_many (l : t list) = List.fold_left meet top l

  let rename ~(from : Sym.t) ~(to_ : Sym.t) (od : t) : t =
    match od with
    | None -> None
    | Some d ->
      Some
        (match Sym.Map.find_opt from d with
         | Some b -> Sym.Map.add to_ b (Sym.Map.remove from d)
         | None -> d)


  let remove (x : Sym.t) (od : t) : t =
    match od with None -> None | Some d -> Some (Sym.Map.remove x d)


  let retain (xs : Sym.Set.t) (od : t) : t =
    match od with
    | None -> None
    | Some d -> Some (Sym.Map.filter (fun x _ -> Sym.Set.mem x xs) d)


  let relative_to (x : Sym.t) (d : t) : Relative.t =
    let open Option in
    let@ d = d in
    return (Option.value ~default:(failwith __LOC__) (Sym.Map.find_opt x d))


  let free_vars (od : t) =
    match od with
    | Some d -> Sym.Set.of_seq (List.to_seq (List.map fst (Sym.Map.bindings d)))
    | None -> Sym.Set.empty


  let pp (od : t) : Pp.document =
    let open Pp in
    match od with
    | None -> !^"⊥"
    | Some d ->
      (match Sym.Map.bindings d with
       | [] -> !^"⊤"
       | bindings ->
         brackets
           (separate_map
              (!^";" ^^^ break 1)
              (fun (sym, b) -> Sym.pp sym ^^ !^"↦" ^^ B.pp b)
              bindings))


  let pp_c = failwith __LOC__
end

module Interpreter (B : BASIS) : Interpreter.Part = struct
  module AD = Make (B)

  type forward_tree =
    | Test of forward_tree * forward_tree
    | Op of forward_tree list * B.t
    | Leaf of B.t

  let tree_b t = match t with Op (_, b) | Leaf b -> b | _ -> failwith __LOC__

  let tree_meet_b t b' =
    match t with Op (args, b) -> Op (args, B.meet b b') | _ -> failwith __LOC__


  let tree_test t = match t with Test (t1, t2) -> (t1, t2) | _ -> failwith __LOC__

  let rec forward_abs_it (it : IT.t) (od : AD.t) : forward_tree =
    let (IT (it_, bt, _loc)) = it in
    let single_arg it1 =
      let b1 = forward_abs_it it1 od in
      Op ([ b1 ], B.forward_abs_it it [ tree_b b1 ])
    in
    let dual_arg it1 it2 =
      let b1 = forward_abs_it it1 od in
      let b2 = forward_abs_it it2 od in
      Op ([ b1; b2 ], B.forward_abs_it it [ tree_b b1; tree_b b2 ])
    in
    let triple_arg it1 it2 it3 =
      let b1 = forward_abs_it it1 od in
      let b2 = forward_abs_it it2 od in
      let b3 = forward_abs_it it3 od in
      Op ([ b1; b2; b3 ], B.forward_abs_it it [ tree_b b1; tree_b b2; tree_b b3 ])
    in
    let list_args its =
      let bs = List.map (fun it1 -> forward_abs_it it1 od) its in
      Op (bs, B.forward_abs_it it (List.map tree_b bs))
    in
    match it_ with
    | Sym x ->
      (match od with
       | Some d -> Leaf (match Sym.Map.find_opt x d with Some b -> b | None -> B.top bt)
       | None -> Leaf (B.bottom bt))
    (* Constants and nullary constructors *)
    | Const _ | SizeOf _ | OffsetOf (_, _) | Nil _ | CN_None _ ->
      Leaf (B.forward_abs_it it [])
    (* Tests *)
    | Binop (EQ, it1, it2)
    | Binop (LE, it1, it2)
    | Binop (LT, it1, it2)
    | Unop (Not, IT (Binop (EQ, it1, it2), _, _))
    | Unop (Not, IT (Binop (LE, it1, it2), _, _))
    | Unop (Not, IT (Binop (LT, it1, it2), _, _)) ->
      let b1 = forward_abs_it it1 od in
      let b2 = forward_abs_it it2 od in
      Test (b1, b2)
    (* Single argument cases *)
    | Unop (_, it1)
    | NthTuple (_, it1)
    | StructMember (it1, _)
    | RecordMember (it1, _)
    | MemberShift (it1, _, _)
    | HasAllocId it1
    | Head it1
    | Tail it1
    | Representable (_, it1)
    | Good (_, it1)
    | WrapI (_, it1)
    | MapConst (_, it1)
    | Cast (_, it1)
    | CN_Some it1
    | IsSome it1
    | GetOpt it1 ->
      single_arg it1
    | EachI ((_, (_, _), _), it1) | MapDef ((_, _), it1) -> single_arg it1
    (* Dual argument cases *)
    | Binop (_, it1, it2)
    | StructUpdate ((it1, _), it2)
    | RecordUpdate ((it1, _), it2)
    | Cons (it1, it2)
    | MapGet (it1, it2)
    | ArrayShift { base = it1; index = it2; _ }
    | CopyAllocId { addr = it1; loc = it2 }
    | Aligned { t = it1; align = it2 }
    | Let ((_, it1), it2) ->
      dual_arg it1 it2
    (* Triple argument cases *)
    | ITE (it1, it2, it3) | MapSet (it1, it2, it3) -> triple_arg it1 it2 it3
    (* List argument cases *)
    | Tuple its | Apply (_, its) -> list_args its
    | Struct (_, its) | Record its | Constructor (_, its) -> list_args (List.map snd its)
    (* Complex cases *)
    | Match (it1, cases) ->
      let b1 = forward_abs_it it1 od in
      let case_bs = List.map (fun (_, case_it) -> forward_abs_it case_it od) cases in
      let all_bs = b1 :: case_bs in
      Op (all_bs, B.forward_abs_it it (List.map tree_b all_bs))


  let rec backward_abs_it (it : IT.t) (t : forward_tree) : AD.t =
    let (IT (it_, _bt, _loc)) = it in
    let propagate_single it1 =
      match t with
      | Op ([ t1 ], b_res) ->
        (match B.backward_abs_it it [ b_res; tree_b t1 ] with
         | [ b1 ] ->
           let t1' = tree_meet_b t1 b1 in
           backward_abs_it it1 t1'
         | _ -> failwith __LOC__)
      | _ -> failwith __LOC__
    in
    let propagate_dual it1 it2 =
      match t with
      | Op ([ t1; t2 ], b_res) ->
        (match B.backward_abs_it it [ b_res; tree_b t1; tree_b t2 ] with
         | [ b1; b2 ] ->
           let t1' = tree_meet_b t1 b1 in
           let t2' = tree_meet_b t2 b2 in
           AD.meet (backward_abs_it it1 t1') (backward_abs_it it2 t2')
         | _ -> failwith __LOC__)
      | _ -> failwith __LOC__
    in
    let propagate_triple it1 it2 it3 =
      match t with
      | Op ([ t1; t2; t3 ], b_res) ->
        (match B.backward_abs_it it [ b_res; tree_b t1; tree_b t2; tree_b t3 ] with
         | [ b1; b2; b3 ] ->
           let t1' = tree_meet_b t1 b1 in
           let t2' = tree_meet_b t2 b2 in
           let t3' = tree_meet_b t3 b3 in
           AD.meet
             (AD.meet (backward_abs_it it1 t1') (backward_abs_it it2 t2'))
             (backward_abs_it it3 t3')
         | _ -> failwith __LOC__)
      | _ -> failwith __LOC__
    in
    let propagate_list its =
      match t with
      | Op (ts, b_res) ->
        assert (List.length ts = List.length its);
        let bs = B.backward_abs_it it (b_res :: List.map tree_b ts) in
        assert (List.length bs = List.length ts);
        let ts' = List.map2 tree_meet_b ts bs in
        let results = List.map2 backward_abs_it its ts' in
        List.fold_left AD.meet AD.top results
      | _ -> failwith __LOC__
    in
    match it_ with
    | Sym x -> Some (Sym.Map.singleton x (tree_b t))
    (* Constants and nullary constructors - no backward constraints *)
    | Const _ | SizeOf _ | OffsetOf (_, _) | Nil _ | CN_None _ -> AD.top
    (* Tests - special tree_test logic *)
    | Binop (EQ, it1, it2)
    | Binop (LE, it1, it2)
    | Binop (LT, it1, it2)
    | Unop (Not, IT (Binop (EQ, it1, it2), _, _))
    | Unop (Not, IT (Binop (LE, it1, it2), _, _))
    | Unop (Not, IT (Binop (LT, it1, it2), _, _)) ->
      let t1, t2 = tree_test t in
      let b1, b2 =
        match B.backward_abs_it it [ tree_b t1; tree_b t2 ] with
        | [ b1; b2 ] -> (b1, b2)
        | _ -> failwith __LOC__
      in
      let t1' = tree_meet_b t1 b1 in
      let t2' = tree_meet_b t2 b2 in
      AD.meet (backward_abs_it it1 t1') (backward_abs_it it2 t2')
    (* Single argument propagating operations *)
    | Unop (_, it1)
    | NthTuple (_, it1)
    | StructMember (it1, _)
    | RecordMember (it1, _)
    | MemberShift (it1, _, _)
    | HasAllocId it1
    | Head it1
    | Tail it1
    | Representable (_, it1)
    | Good (_, it1)
    | WrapI (_, it1)
    | MapConst (_, it1)
    | Cast (_, it1)
    | CN_Some it1
    | IsSome it1
    | GetOpt it1 ->
      propagate_single it1
    (* Dual argument propagating operations *)
    | Binop (_, it1, it2)
    | StructUpdate ((it1, _), it2)
    | RecordUpdate ((it1, _), it2)
    | Cons (it1, it2)
    | MapGet (it1, it2)
    | ArrayShift { base = it1; index = it2; _ }
    | CopyAllocId { addr = it1; loc = it2 }
    | Aligned { t = it1; align = it2 } ->
      propagate_dual it1 it2
    (* Triple argument propagating operations *)
    | ITE (it1, it2, it3) | MapSet (it1, it2, it3) -> propagate_triple it1 it2 it3
    (* List argument propagating operations *)
    | Tuple its | Apply (_, its) -> propagate_list its
    | Struct (_, its) | Record its | Constructor (_, its) ->
      propagate_list (List.map snd its)
    (* Binding operations - handle variable scoping *)
    | EachI ((_, (x, _), _), it1) | MapDef ((x, _), it1) ->
      let result = propagate_single it1 in
      AD.remove x result
    | Let ((_x, it1), it2) ->
      (* FIXME: Figure out *)
      (* AD.rename ~from:Domain.ret_sym ~to_:x result *)
      propagate_dual it1 it2
    (* Complex cases *)
    | Match (it1, cases) ->
      (match t with
       | Op (t1 :: case_ts, _) when List.length case_ts = List.length cases ->
         let scrutinee_result = backward_abs_it it1 t1 in
         let case_results =
           List.map2
             (fun (_, case_it) case_t -> backward_abs_it case_it case_t)
             cases
             case_ts
         in
         List.fold_left AD.meet scrutinee_result case_results
       | _ -> AD.top)


  let local_iteration (it : IT.t) (d : AD.t) : AD.t =
    let rec aux (d : AD.t) (fuel : int) : AD.t =
      if fuel <= 0 then
        d
      else (
        let t = forward_abs_it it d in
        let d' = backward_abs_it it t in
        if AD.equal d d' then d' else aux d' (fuel - 1))
    in
    aux d 10


  let abs_assert (lc : LC.t) (d : AD.t) : AD.t =
    match lc with T it | Forall (_, it) -> local_iteration it d


  let abs_assign ((it_addr, sct), _) d =
    let _, max = BT.bits_range (Option.get (BT.is_bits_bt Memory.uintptr_bt)) in
    let bytes = Z.of_int (Memory.size_of_ctype sct) in
    assert (Z.lt bytes max);
    local_iteration
      ((IT.lePointer_
          ( it_addr,
            IT.num_lit_ (Z.sub max bytes) Memory.uintptr_bt (Locations.other __LOC__) ))
         (Locations.other __LOC__))
      d
end
