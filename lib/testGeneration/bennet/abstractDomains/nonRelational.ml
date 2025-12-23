(** Non-relational numeric domain *)

module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

module type BASIS = sig
  include Domain.RELATIVE_VIEW

  val name : string

  val c_name : string

  val supported : BT.t -> bool

  val bt : t -> BT.t

  val bottom : BT.t -> t

  val top : BT.t -> t

  val leq : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val join_many : t list -> t

  val meet_many : t list -> t

  val is_meet_assoc : bool

  val is_join_assoc : bool

  val of_interval : BT.t -> Z.t -> Z.t -> t

  val forward_abs_it : IT.t -> t list -> t option

  val backward_abs_it : IT.t -> t list -> t list

  val widen : t -> t -> t

  val narrow : t -> t -> t

  val pp : t -> Pp.document

  val pp_params : unit -> string

  val pp_sym_args : unit -> string

  val pp_args : t -> string

  val definitions : unit -> Pp.document

  val to_it : Sym.t -> t -> IT.t
end

module Make (B : BASIS) = struct
  let name = B.name

  module CInt : Domain.C_INTERFACE = struct
    let name = B.c_name

    let definitions = B.definitions
  end

  module Relative = B

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


  let relative_to (x : Sym.t) (bt : BT.t) (od : t) : Relative.t =
    let open Option in
    match od with
    | Some d -> Option.value ~default:(B.top bt) (Sym.Map.find_opt x d)
    | None -> B.bottom bt


  let free_vars (od : t) =
    match od with
    | Some d -> Sym.Set.of_seq (List.to_seq (List.map fst (Sym.Map.bindings d)))
    | None -> Sym.Set.empty


  let free_vars_bts (od : t) : (Sym.t * BT.t) list =
    match od with
    | Some d -> Sym.Map.bindings d |> List.map (fun (sym, basis) -> (sym, B.bt basis))
    | None -> []


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


  (* Abstract interpretation functionality - moved from Interpreter functor *)

  type forward_tree =
    | Test of forward_tree option * forward_tree option
    | Op of forward_tree option list * B.t
    | Leaf of B.t

  let rec pp_tree t =
    let open Pp in
    match t with
    | Test (t1, t2) -> Option.pp pp_tree t1 ^^^ equals ^^^ Option.pp pp_tree t2
    | Op (ts, b) -> separate_map comma (Option.pp pp_tree) ts ^^^ equals ^^^ B.pp b
    | Leaf b -> B.pp b


  let tree_b (t : forward_tree) : B.t =
    match t with Op (_, b) | Leaf b -> b | _ -> failwith __LOC__


  let tree_meet_b (ot : forward_tree option) b' =
    match ot with
    | Some (Op (args, b)) -> Op (args, B.meet b b')
    | Some (Leaf b) -> Leaf (B.meet b b')
    | None -> Leaf b'
    | _ -> failwith __LOC__


  let tree_test t = match t with Test (t1, t2) -> (t1, t2) | _ -> failwith __LOC__

  let rec cnf_ (e : BT.t IT.term) : BT.t IT.term =
    match e with
    | Unop (Not, e') ->
      (match cnf e' with
       (* Double negation elimination *)
       | IT (Unop (Not, IT (e, _, _)), _, _) -> e
       (* Flip inequalities *)
       | IT (Binop (LT, e1, e2), _, _) -> Binop (LE, e2, e1)
       | IT (Binop (LE, e1, e2), _, _) -> Binop (LT, e2, e1)
       (* De Morgan's Law *)
       | IT (Binop (And, e1, e2), info, loc) ->
         Binop
           ( Or,
             cnf (IT.IT (Unop (Not, e1), info, loc)),
             cnf (IT (Unop (Not, e2), info, loc)) )
       | IT (Binop (Or, e1, e2), info, loc) ->
         Binop
           ( And,
             cnf (IT (Unop (Not, e1), info, loc)),
             cnf (IT (Unop (Not, e2), info, loc)) )
       (* Otherwise *)
       | e'' -> Unop (Not, e''))
    | Binop (Or, e1, e2) ->
      (match (cnf e1, cnf e2) with
       (* Distribute disjunction *)
       | e', IT (Binop (And, e_x, e_y), info, loc)
       | IT (Binop (And, e_x, e_y), info, loc), e' ->
         Binop
           ( And,
             cnf (IT (Binop (Or, e', e_x), info, loc)),
             cnf (IT (Binop (Or, e', e_y), info, loc)) )
       | e1, e2 -> Binop (Or, e1, e2))
    | _ -> e


  and cnf (e : IT.t) : IT.t =
    let (IT (e, info, loc)) = e in
    IT (cnf_ e, info, loc)


  let rec forward_abs_it (it : IT.t) (od : t) : forward_tree option =
    let open Option in
    let (IT (it_, bt, _loc)) = it in
    let single_arg it1 =
      let ot = forward_abs_it it1 od in
      let@ b' =
        let@ t = ot in
        B.forward_abs_it it [ tree_b t ]
      in
      return (Op ([ ot ], b'))
    in
    let dual_arg it1 it2 =
      let ot1 = forward_abs_it it1 od in
      let ot2 = forward_abs_it it2 od in
      let@ b =
        let@ t1 = ot1 in
        let@ t2 = ot2 in
        B.forward_abs_it it [ tree_b t1; tree_b t2 ]
      in
      return (Op ([ ot1; ot2 ], b))
    in
    let triple_arg it1 it2 it3 =
      let ot1 = forward_abs_it it1 od in
      let ot2 = forward_abs_it it2 od in
      let ot3 = forward_abs_it it3 od in
      let@ b =
        let@ t1 = ot1 in
        let@ t2 = ot2 in
        let@ t3 = ot3 in
        B.forward_abs_it it [ tree_b t1; tree_b t2; tree_b t3 ]
      in
      return (Op ([ ot1; ot2; ot3 ], b))
    in
    let list_args its =
      let obs = List.map (fun it1 -> forward_abs_it it1 od) its in
      let@ b =
        let@ bs = Option.ListM.mapM (fun x -> x) obs in
        B.forward_abs_it it (List.map tree_b bs)
      in
      return (Op (obs, b))
    in
    match it_ with
    | Sym x ->
      if B.supported bt then
        return
          (match od with
           | Some d ->
             Leaf (match Sym.Map.find_opt x d with Some b -> b | None -> B.top bt)
           | None -> Leaf (B.bottom bt))
      else
        None
    (* Constants and nullary constructors *)
    | Const _ | SizeOf _ | OffsetOf (_, _) | Nil _ | CN_None _ ->
      let@ b = B.forward_abs_it it [] in
      return (Leaf b)
    (* Tests *)
    | Binop (EQ, it1, it2)
    | Binop (LE, it1, it2)
    | Binop (LEPointer, it1, it2)
    | Binop (LT, it1, it2)
    | Binop (LTPointer, it1, it2)
    | Binop (And, it1, it2)
    | Binop (Or, it1, it2)
    | Binop (Implies, it1, it2)
    | Unop (Not, IT (Binop (EQ, it1, it2), _, _))
    | Unop (Not, IT (Binop (LE, it1, it2), _, _))
    | Unop (Not, IT (Binop (LEPointer, it1, it2), _, _))
    | Unop (Not, IT (Binop (LT, it1, it2), _, _))
    | Unop (Not, IT (Binop (LTPointer, it1, it2), _, _)) ->
      let t1 = forward_abs_it it1 od in
      let t2 = forward_abs_it it2 od in
      return (Test (t1, t2))
    | Unop (Not, IT (Unop (_, _), _, _)) | Unop (Not, IT (Binop (_, _, _), _, _)) ->
      forward_abs_it (cnf it) od
    (* Single argument cases *)
    | Good (_, it1)
    | Unop (_, it1)
    | NthTuple (_, it1)
    | StructMember (it1, _)
    | RecordMember (it1, _)
    | MemberShift (it1, _, _)
    | HasAllocId it1
    | Head it1
    | Tail it1
    | Representable (_, it1)
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
    | Aligned { t = it1; align = it2 } ->
      dual_arg it1 it2
    | Let ((x, it1), it2) -> single_arg (IT.subst (IT.make_subst [ (x, it1) ]) it2)
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
      let@ b =
        let@ all_bs = Option.ListM.mapM (Option.map tree_b) all_bs in
        B.forward_abs_it it all_bs
      in
      return (Op (all_bs, b))


  let rec backward_abs_it (it : IT.t) (t : forward_tree option) : t =
    match t with
    | None -> top
    | Some t ->
      let (IT (it_, bt, _loc)) = it in
      let propagate_single it1 =
        if B.supported (IT.get_bt it1) then (
          match t with
          | Op ([ t1 ], b_res) ->
            (match
               B.backward_abs_it
                 it
                 [ b_res;
                   Option.value ~default:(B.top (IT.get_bt it1)) (Option.map tree_b t1)
                 ]
             with
             | [ b1 ] ->
               let t1' = tree_meet_b t1 b1 in
               backward_abs_it it1 (Some t1')
             | _ -> failwith __LOC__)
          | Leaf b ->
            (match B.backward_abs_it it [ b; B.top (IT.get_bt it1) ] with
             | [ b' ] -> backward_abs_it it1 (Some (Leaf b'))
             | _ -> failwith __LOC__)
          | _ ->
            print_endline (Pp.plain (pp_tree t));
            print_endline (Pp.plain (IT.pp it));
            failwith __LOC__)
        else
          top
      in
      let propagate_dual it1 it2 =
        if B.supported (IT.get_bt it1) && B.supported (IT.get_bt it2) then (
          match t with
          | Op ([ t1; t2 ], b_res) ->
            (match
               B.backward_abs_it
                 it
                 [ b_res;
                   Option.value ~default:(B.top (IT.get_bt it1)) (Option.map tree_b t1);
                   Option.value ~default:(B.top (IT.get_bt it2)) (Option.map tree_b t2)
                 ]
             with
             | [ b1; b2 ] ->
               let t1' = tree_meet_b t1 b1 in
               let t2' = tree_meet_b t2 b2 in
               meet (backward_abs_it it1 (Some t1')) (backward_abs_it it2 (Some t2'))
             | _ -> failwith __LOC__)
          | Leaf b ->
            (match
               B.backward_abs_it it [ b; B.top (IT.get_bt it1); B.top (IT.get_bt it2) ]
             with
             | [ b1; b2 ] ->
               meet
                 (backward_abs_it it1 (Some (Leaf b1)))
                 (backward_abs_it it2 (Some (Leaf b2)))
             | _ -> failwith __LOC__)
          | _ -> failwith __LOC__)
        else
          top
      in
      let propagate_triple it1 it2 it3 =
        if
          B.supported (IT.get_bt it1)
          && B.supported (IT.get_bt it2)
          && B.supported (IT.get_bt it3)
        then (
          match t with
          | Op ([ t1; t2; t3 ], b_res) ->
            (match
               B.backward_abs_it
                 it
                 [ b_res;
                   Option.value ~default:(B.top (IT.get_bt it1)) (Option.map tree_b t1);
                   Option.value ~default:(B.top (IT.get_bt it2)) (Option.map tree_b t2);
                   Option.value ~default:(B.top (IT.get_bt it3)) (Option.map tree_b t3)
                 ]
             with
             | [ b1; b2; b3 ] ->
               let t1' = tree_meet_b t1 b1 in
               let t2' = tree_meet_b t2 b2 in
               let t3' = tree_meet_b t3 b3 in
               meet
                 (backward_abs_it it1 (Some t1'))
                 (meet (backward_abs_it it2 (Some t2')) (backward_abs_it it3 (Some t3')))
             | _ -> failwith __LOC__)
          | Leaf b ->
            (match
               B.backward_abs_it it [ b; B.top (IT.get_bt it1); B.top (IT.get_bt it1) ]
             with
             | [ b1; b2 ] ->
               meet
                 (backward_abs_it it1 (Some (Leaf b1)))
                 (meet
                    (backward_abs_it it1 (Some (Leaf b2)))
                    (backward_abs_it it1 (Some (Leaf b2))))
             | _ -> failwith __LOC__)
          | _ -> failwith __LOC__)
        else
          top
      in
      let propagate_list its =
        if its |> List.map IT.get_bt |> List.for_all B.supported then (
          match t with
          | Op (ts, b_res) ->
            assert (List.length ts = List.length its);
            let bs =
              B.backward_abs_it
                it
                (b_res
                 :: List.map
                      (fun (it, t) ->
                         Option.value
                           ~default:(B.top (IT.get_bt it))
                           (Option.map tree_b t))
                      (List.combine its ts))
            in
            assert (List.length bs = List.length ts);
            let ts' = List.map2 tree_meet_b ts bs in
            let results =
              List.map2 backward_abs_it its (List.map (fun t -> Some t) ts')
            in
            List.fold_left meet top results
          | Leaf b ->
            let bs =
              B.backward_abs_it it ([ b ] @ List.map (fun it -> B.top (IT.get_bt it)) its)
            in
            assert (List.length bs = List.length its);
            let ts' = List.map (fun b -> Some (Leaf b)) bs in
            let results = List.map2 backward_abs_it its ts' in
            List.fold_left meet top results
          | _ -> failwith __LOC__)
        else
          top
      in
      (match it_ with
       | Sym x -> Some (Sym.Map.singleton x (tree_b t))
       (* Constants and nullary constructors - no backward constraints *)
       | Const _ | SizeOf _ | OffsetOf (_, _) | Nil _ | CN_None _ -> top
       | Binop (EQ, it1, it2)
       | Binop (LE, it1, it2)
       | Binop (LEPointer, it1, it2)
       | Binop (LT, it1, it2)
       | Binop (LTPointer, it1, it2)
       | Unop (Not, IT (Binop (EQ, it1, it2), _, _))
       | Unop (Not, IT (Binop (LE, it1, it2), _, _))
       | Unop (Not, IT (Binop (LEPointer, it1, it2), _, _))
       | Unop (Not, IT (Binop (LT, it1, it2), _, _))
       | Unop (Not, IT (Binop (LTPointer, it1, it2), _, _)) ->
         if B.supported (IT.get_bt it1) then (
           let t1, t2 = tree_test t in
           let b1, b2 =
             match
               B.backward_abs_it
                 it
                 [ Option.value ~default:(B.top (IT.get_bt it1)) (Option.map tree_b t1);
                   Option.value ~default:(B.top (IT.get_bt it2)) (Option.map tree_b t2)
                 ]
             with
             | [ b1; b2 ] -> (b1, b2)
             | _ -> failwith __LOC__
           in
           let t1' = tree_meet_b t1 b1 in
           let t2' = tree_meet_b t2 b2 in
           meet (backward_abs_it it1 (Some t1')) (backward_abs_it it2 (Some t2')))
         else
           top
       | Unop (Not, IT (Unop (_, _), _, _)) | Unop (Not, IT (Binop (_, _, _), _, _)) ->
         backward_abs_it (cnf it) (Some t)
       | Binop (And, it1, it2) ->
         let t1, t2 = tree_test t in
         meet (backward_abs_it it1 t1) (backward_abs_it it2 t2)
       | Binop (Or, it1, it2) ->
         let t1, t2 = tree_test t in
         join (backward_abs_it it1 t1) (backward_abs_it it2 t2)
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
         if B.supported bt then
           propagate_single it1
         else
           top
       (* Dual argument propagating operations *)
       | Binop (_, it1, it2)
       | StructUpdate ((it1, _), it2)
       | RecordUpdate ((it1, _), it2)
       | Cons (it1, it2)
       | MapGet (it1, it2)
       | ArrayShift { base = it1; index = it2; _ }
       | CopyAllocId { addr = it1; loc = it2 }
       | Aligned { t = it1; align = it2 } ->
         if B.supported bt then
           propagate_dual it1 it2
         else
           top
       (* Triple argument propagating operations *)
       | ITE (it1, it2, it3) | MapSet (it1, it2, it3) ->
         if B.supported bt then propagate_triple it1 it2 it3 else top
       (* List argument propagating operations *)
       | Tuple its | Apply (_, its) -> propagate_list its
       | Struct (_, its) | Record its | Constructor (_, its) ->
         if B.supported bt then propagate_list (List.map snd its) else top
       (* Binding operations - handle variable scoping *)
       | EachI ((_, (x, _), _), it1) | MapDef ((x, _), it1) ->
         if B.supported bt then (
           let result = propagate_single it1 in
           remove x result)
         else
           top
       | Let ((x, it1), it2) ->
         if B.supported bt then
           propagate_single (IT.subst (IT.make_subst [ (x, it1) ]) it2)
         else
           top
       (* Complex cases *)
       | Match (it1, cases) ->
         if B.supported bt then (
           match t with
           | Op (t1 :: case_ts, _) when List.length case_ts = List.length cases ->
             let scrutinee_result = backward_abs_it it1 t1 in
             let case_results =
               List.map2
                 (fun (_, case_it) case_t -> backward_abs_it case_it case_t)
                 cases
                 case_ts
             in
             List.fold_left meet scrutinee_result case_results
           | _ -> top)
         else
           top)


  let local_iteration (it : IT.t) (d : t) : t =
    let rec aux (d : t) (fuel : int) : t =
      if fuel <= 0 then
        d
      else (
        let t = forward_abs_it it d in
        let d' = backward_abs_it it t in
        let d' = meet d d' in
        if equal d d' then d' else aux d' (fuel - 1))
    in
    aux d (TestGenConfig.get_local_iterations ())


  let abs_assert (lc : LC.t) (d : t) : t =
    match lc with T it | Forall (_, it) -> local_iteration it d


  let abs_assign ((it_addr, sct), _) d =
    let _, max = BT.bits_range (Option.get (BT.is_bits_bt Memory.uintptr_bt)) in
    let bytes = Z.of_int (Memory.size_of_ctype sct) in
    assert (Z.lt bytes max);
    let loc = Locations.other __LOC__ in
    local_iteration
      ((IT.le_
          ( IT.cast_ Memory.uintptr_bt it_addr loc,
            IT.num_lit_ (Z.sub max bytes) Memory.uintptr_bt loc ))
         (Locations.other __LOC__))
      d


  let pp_params = B.pp_params

  let pp_args = B.pp_sym_args

  let to_it (od : t) : IT.t =
    let loc = Locations.other __LOC__ in
    match od with
    | None -> IT.bool_ false loc (* bottom = unsatisfiable *)
    | Some d ->
      let constraints =
        Sym.Map.fold (fun sym basis acc -> B.to_it sym basis :: acc) d []
      in
      IT.and_ constraints loc (* conjunction of all basis constraints *)


  let is_meet_assoc = B.is_meet_assoc

  let is_join_assoc = B.is_join_assoc
end
