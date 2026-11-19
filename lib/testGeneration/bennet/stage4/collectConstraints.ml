module BT = BaseTypes
module MT = MakeTerm
module T = Terms.Normal
module LC = LogicalConstraints

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let rec pointer_of (it : T.t) : Sym.t * BT.t =
    match it with
    | IT (CopyAllocId { loc = ptr; _ }, _, _)
    | IT (ArrayShift { base = ptr; _ }, _, _)
    | IT (MemberShift (ptr, _, _), _, _) ->
      pointer_of ptr
    | IT (Sym x, bt, _) | IT (Cast (_, IT (Sym x, bt, _)), _, _) -> (x, bt)
    | _ ->
      let pointers =
        it |> T.free_vars_bts |> Sym.Map.filter (fun _ bt -> BT.equal bt (BT.Loc ()))
      in
      if not (Sym.Map.cardinal pointers == 1) then
        Cerb_debug.print_debug 2 [] (fun () ->
          Pp.(
            plain
              (braces
                 (separate_map
                    (comma ^^ space)
                    (fun (x, bt) -> Sym.pp x ^^ colon ^^^ BT.pp bt)
                    (List.of_seq (Sym.Map.to_seq pointers)))
               ^^^ !^" in "
               ^^ T.pp it)));
      if Sym.Map.is_empty pointers then (
        print_endline (Pp.plain (T.pp it));
        failwith __LOC__);
      Sym.Map.choose pointers


  (** If [gt] is a [Map] over a range [it1 <= i < it2] that contains an [Asgn] with an
      [ArrayShift] indexed by [i] (optionally filtered by [target]), returns a list of
      [(start_addr, ct, index_bound)] tuples where [start_addr] is the address of the
      first element and [index_bound] is the upper-bound index count (i.e. [it2]).
      Returns [[]] when the pattern does not match. *)
  let array_map_of ~(defined : Sym.Set.t) ?target (gt : Term.t)
    : (T.t * Sctypes.t * T.t) list
    =
    let (GenTerms.Annot (gt_, _, _, _)) = gt in
    match gt_ with
    | `Map ((i, _i_bt, it_perm), gt_inner) ->
      (match it_perm with
       | IT
           ( Binop
               ( And,
                 IT (Binop (LE, it1, IT (Sym i1, _, _)), _, _),
                 IT (Binop (LT, IT (Sym i2, _, _), it_max), _, _) ),
             _,
             _ )
       | IT
           ( Binop
               ( And,
                 IT (Binop (LT, IT (Sym i2, _, _), it_max), _, _),
                 IT (Binop (LE, it1, IT (Sym i1, _, _)), _, _) ),
             _,
             _ )
         when Sym.equal i1 i && Sym.equal i2 i ->
         let defined' = Sym.Set.add i defined in
         let rec find_asgn (gt : Term.t) : (T.t * Sctypes.t * T.t) list =
           let (GenTerms.Annot (gt_, _, _, _)) = gt in
           match gt_ with
           | `LetStar ((_, gt_inner), gt_rest) ->
             (match find_asgn gt_inner with [] -> find_asgn gt_rest | results -> results)
           | `Assert (_, gt_rest) | `AssertDomain (_, _, _, gt_rest) -> find_asgn gt_rest
           | `Asgn ((it_addr, _sct), _, gt_rest) ->
             (match it_addr with
              | IT (ArrayShift { base; ct; index = IT (Sym _, _, _) }, _, arr_loc) ->
                let fvs = T.free_vars it_addr in
                let fvs' =
                  match target with Some t -> Sym.Set.remove t fvs | None -> fvs
                in
                let it_max_fvs = T.free_vars it_max in
                let it_max_fvs' =
                  match target with
                  | Some t -> Sym.Set.remove t it_max_fvs
                  | None -> it_max_fvs
                in
                if
                  (match target with Some t -> Sym.Set.mem t fvs | None -> true)
                  && Sym.Set.subset fvs' defined'
                  && Sym.Set.subset it_max_fvs' defined
                then
                  [ ( IT (ArrayShift { base; ct; index = it1 }, BT.Loc (), arr_loc),
                      ct,
                      it_max )
                  ]
                else
                  find_asgn gt_rest
              | _ -> find_asgn gt_rest)
           | _ -> []
         in
         find_asgn gt_inner
       | IT (Binop (LT, IT (Sym i', _, _), it_max), _, _) when Sym.equal i i' ->
         let defined' = Sym.Set.add i defined in
         let rec find_asgn (gt : Term.t) : (T.t * Sctypes.t * T.t) list =
           let (GenTerms.Annot (gt_, _, _, _)) = gt in
           match gt_ with
           | `LetStar ((_, gt_inner), gt_rest) ->
             (match find_asgn gt_inner with [] -> find_asgn gt_rest | results -> results)
           | `Assert (_, gt_rest) | `AssertDomain (_, _, _, gt_rest) -> find_asgn gt_rest
           | `Asgn ((it_addr, _sct), _, gt_rest) ->
             (match it_addr with
              | IT (ArrayShift { base; ct; index = IT (Sym _, _, _) }, _, _) ->
                let fvs = T.free_vars it_addr in
                let fvs' =
                  match target with Some t -> Sym.Set.remove t fvs | None -> fvs
                in
                let it_max_fvs = T.free_vars it_max in
                let it_max_fvs' =
                  match target with
                  | Some t -> Sym.Set.remove t it_max_fvs
                  | None -> it_max_fvs
                in
                if
                  (match target with Some t -> Sym.Set.mem t fvs | None -> true)
                  && Sym.Set.subset fvs' defined'
                  && Sym.Set.subset it_max_fvs' defined
                then
                  [ (base, ct, it_max) ]
                else
                  find_asgn gt_rest
              | _ -> find_asgn gt_rest)
           | _ -> []
         in
         find_asgn gt_inner
       | _ -> [])
    | _ -> []


  (** Collect constraints and assignments from Assert/Asgn nodes in gt_rest that involve
      only defined variables plus the target variable. Constraints stop at LetStar
      boundaries; assignments continue through LetStar (adding the bound variable to
      defined). *)
  let rec collect_constraints
            ?(strip = false)
            ~(defined : Sym.Set.t)
            ?target
            (gt : Term.t)
    : T.t list * (T.t * Sctypes.t * T.t option) list * Term.t
    =
    let (GenTerms.Annot (gt_, tag, _bt, loc)) = gt in
    match gt_ with
    | `Assert (lc, gt_rest) ->
      let constraints_rest, asgns_rest, gt_rest' =
        collect_constraints ~strip ~defined ?target gt_rest
      in
      (match lc with
       | LC.T it ->
         let fvs = T.free_vars it in
         let fvs' = match target with Some t -> Sym.Set.remove t fvs | None -> fvs in
         if Sym.Set.subset fvs' defined then
           ( it :: constraints_rest,
             asgns_rest,
             if strip then gt_rest' else Term.assert_ (lc, gt_rest') tag loc )
         else
           (constraints_rest, asgns_rest, Term.assert_ (lc, gt_rest') tag loc)
       | LC.Forall _ ->
         (* Skip forall constraints - they have quantified variables *)
         (constraints_rest, asgns_rest, Term.assert_ (lc, gt_rest') tag loc))
    | `LetStar ((x, gt_inner), gt_rest) ->
      let inner_asgns =
        let (GenTerms.Annot (gt_inner_, _, _, _)) = gt_inner in
        match gt_inner_ with
        | `Map _ ->
          List.map
            (fun (s, ct, e) -> (s, ct, Some e))
            (array_map_of ~defined ?target gt_inner)
        | _ -> []
      in
      let constraints_rest, asgns_rest, gt_rest' =
        collect_constraints ~strip ~defined ?target gt_rest
      in
      ( constraints_rest,
        inner_asgns @ asgns_rest,
        Term.let_star_ ((x, gt_inner), gt_rest') tag loc )
    | `Map _ ->
      let arr_results = array_map_of ~defined ?target gt in
      ( [],
        List.map
          (fun (start_addr, ct, end_addr) -> (start_addr, ct, Some end_addr))
          arr_results,
        gt )
    | `Pick _ | `ITE _ -> ([], [], gt)
    (* Constraints in branches are conditional
    *)
    | `Return _ -> ([], [], gt)
    | `Asgn ((it_addr, sct), it_val, gt_rest) ->
      let constraints, asgns, gt_rest' =
        collect_constraints ~strip ~defined ?target gt_rest
      in
      let fvs = T.free_vars it_addr in
      let add_res =
        match target with
        | Some t ->
          let is_abt_target = Sym.equal (fst (pointer_of it_addr)) t in
          let fvs' = Sym.Set.remove t fvs in
          is_abt_target && Sym.Set.subset fvs' defined
        | None -> Sym.Set.subset fvs defined
      in
      if add_res then
        ( constraints,
          (it_addr, sct, None) :: asgns,
          Term.asgn_ ((it_addr, sct), it_val, gt_rest') tag loc )
      else
        (constraints, asgns, Term.asgn_ ((it_addr, sct), it_val, gt_rest') tag loc)
    | `AssertDomain (d, cs, asgns, gt_rest) ->
      let constraints, asgns_rest, gt_rest' =
        collect_constraints ~strip ~defined ?target gt_rest
      in
      (constraints, asgns_rest, Term.assert_domain_ (d, cs, asgns, gt_rest') tag loc)
    | _ -> ([], [], gt)


  (** Walk the tree top-down, tracking defined variables and populating
      AssertDomain nodes with constraints collected from their continuation. *)
  let rec transform_gt (defined : Sym.Set.t) (gt : Term.t) : Term.t =
    if
      not
        (TestGenConfig.has_dynamic_assert_domain ()
         || TestGenConfig.has_dynamic_arbitrary_domain ())
    then
      gt
    else (
      let (GenTerms.Annot (gt_, tag, bt, loc)) = gt in
      match gt_ with
      | `LetStar ((x, gt_inner), gt_rest) ->
        let defined' = Sym.Set.add x defined in
        let gt_inner' = transform_gt defined gt_inner in
        let gt_rest' = transform_gt defined' gt_rest in
        Term.let_star_ ((x, gt_inner'), gt_rest') tag loc
      | `AssertDomain (d, _, _asgns, gt_rest) ->
        let cs, asgns', gt_rest_stripped =
          collect_constraints ~strip:true ~defined gt_rest
        in
        let gt_rest' = transform_gt defined gt_rest_stripped in
        Term.assert_domain_ (d, cs, asgns', gt_rest') tag loc
      | `Assert (lc, gt_rest) ->
        let gt_rest' = transform_gt defined gt_rest in
        Term.assert_ (lc, gt_rest') tag loc
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        let gt_rest' = transform_gt defined gt_rest in
        Term.asgn_ ((it_addr, sct), it_val, gt_rest') tag loc
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        let defined' = Sym.Set.add i defined in
        let gt_inner' = transform_gt defined' gt_inner in
        Term.map_ ((i, i_bt, it_perm), gt_inner') tag loc
      | `Pick gts ->
        let gts' = List.map (transform_gt defined) gts in
        Term.pick_ gts' tag bt loc
      | `ITE (it_if, gt_then, gt_else) ->
        let gt_then' = transform_gt defined gt_then in
        let gt_else' = transform_gt defined gt_else in
        Term.ite_ (it_if, gt_then', gt_else') tag loc
      | _ -> gt)


  let transform_gd (gd : Def.t) : Def.t =
    let defined = gd.iargs |> List.map fst |> Sym.Set.of_list in
    let body = transform_gt defined gd.body in
    { gd with body }


  let transform (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "collect_constraints");
    List.map_snd transform_gd ctx
end
