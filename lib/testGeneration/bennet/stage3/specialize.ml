module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  module Equality = struct
    (* Find and extract an equality constraint for variable x from a term *)
    let rec find_and_extract_eq_constraint (vars : Sym.Set.t) (x : Sym.t) (gt : Term.t)
      : Term.t * IT.t option
      =
      let (Annot (gt_, _, bt, loc)) = gt in
      match gt_ with
      | `Arbitrary | `Symbolic | `ArbitrarySpecialized _ | `ArbitraryDomain _ | `Call _
      | `Return _ ->
        (gt, None)
      | `Pick gts ->
        (* Try to find constraint in any branch - if found, extract from all *)
        let results = List.map (find_and_extract_eq_constraint vars x) gts in
        let found_constraint = List.find_map (fun (_, opt) -> opt) results in
        let new_gts = List.map fst results in
        (Term.pick_ new_gts () bt loc, found_constraint)
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        let gt_rest', constraint_opt = find_and_extract_eq_constraint vars x gt_rest in
        (Term.asgn_ ((it_addr, sct), it_val, gt_rest') () loc, constraint_opt)
      | `LetStar ((y, gt_inner), gt_rest) ->
        let gt_rest', constraint_opt = find_and_extract_eq_constraint vars x gt_rest in
        (Term.let_star_ ((y, gt_inner), gt_rest') () loc, constraint_opt)
      | `Assert (T (IT (Binop (EQ, IT (Sym x', _, _), it), _, _)), gt_rest)
        when Sym.equal x x' && Sym.Set.subset (IT.free_vars it) vars ->
        (* Found: x = it, extract it and remove the assertion *)
        (gt_rest, Some it)
      | `Assert (T (IT (Binop (EQ, it, IT (Sym x', _, _)), _, _)), gt_rest)
        when Sym.equal x x' && Sym.Set.subset (IT.free_vars it) vars ->
        (* Found: it = x, extract it and remove the assertion *)
        (gt_rest, Some it)
      | `Assert (lc, gt_rest) ->
        let gt_rest', constraint_opt = find_and_extract_eq_constraint vars x gt_rest in
        (Term.assert_ (lc, gt_rest') () loc, constraint_opt)
      | `AssertDomain (ad, gt_rest) ->
        let gt_rest', constraint_opt = find_and_extract_eq_constraint vars x gt_rest in
        (Term.assert_domain_ (ad, gt_rest') () loc, constraint_opt)
      | `ITE (it_if, gt_then, gt_else) ->
        let gt_then', constraint_then = find_and_extract_eq_constraint vars x gt_then in
        let gt_else', constraint_else = find_and_extract_eq_constraint vars x gt_else in
        (* Take the first constraint found *)
        let constraint_opt =
          match constraint_then with Some _ -> constraint_then | None -> constraint_else
        in
        (Term.ite_ (it_if, gt_then', gt_else') () loc, constraint_opt)
      | `Map ((i_sym, i_bt, it_perm), gt_inner) ->
        let gt_inner', constraint_opt = find_and_extract_eq_constraint vars x gt_inner in
        (Term.map_ ((i_sym, i_bt, it_perm), gt_inner') () loc, constraint_opt)


    let rec specialize (vars : Sym.Set.t) (gt : Term.t) : Term.t =
      let (Annot (gt_, _, bt, loc)) = gt in
      match gt_ with
      | `Arbitrary | `Symbolic | `ArbitrarySpecialized _ | `ArbitraryDomain _ | `Call _
      | `Return _ ->
        gt
      | `Pick gts -> Term.pick_ (List.map (specialize vars) gts) () bt loc
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        Term.asgn_ ((it_addr, sct), it_val, specialize vars gt_rest) () loc
      | `LetStar ((x, Annot (`Arbitrary, _, bt_x, loc_x)), gt_rest) ->
        (* Try to find an equality constraint for x in the continuation *)
        let gt_rest', constraint_opt = find_and_extract_eq_constraint vars x gt_rest in
        let gt_x =
          match constraint_opt with
          | Some it -> Term.return_ it () loc_x
          | None -> Term.arbitrary_ () bt_x loc_x
        in
        Term.let_star_ ((x, gt_x), specialize (Sym.Set.add x vars) gt_rest') () loc
      | `LetStar ((x, gt_inner), gt_rest) ->
        Term.let_star_
          ((x, specialize vars gt_inner), specialize (Sym.Set.add x vars) gt_rest)
          ()
          loc
      | `Assert (lc, gt_rest) -> Term.assert_ (lc, specialize vars gt_rest) () loc
      | `AssertDomain (ad, gt_rest) ->
        Term.assert_domain_ (ad, specialize vars gt_rest) () loc
      | `ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (it_if, specialize vars gt_then, specialize vars gt_else) () loc
      | `Map ((i_sym, i_bt, it_perm), gt_inner) ->
        Term.map_
          ((i_sym, i_bt, it_perm), specialize (Sym.Set.add i_sym vars) gt_inner)
          ()
          loc


    let transform (gd : Def.t) : Def.t =
      { gd with body = specialize Sym.Set.empty gd.body }
  end

  module Integer = struct
    module Rep = struct
      type t =
        { min_inc : IT.t option; (* x >= n *)
          min_ex : IT.t option; (* x > n, i.e., n < x *)
          max_inc : IT.t option; (* x <= n *)
          max_ex : IT.t option (* x < n *)
        }

      let of_min_inc (it : IT.t) : t =
        { min_inc = Some it; min_ex = None; max_inc = None; max_ex = None }


      let of_min_ex (it : IT.t) : t =
        { min_inc = None; min_ex = Some it; max_inc = None; max_ex = None }


      let of_max_inc (it : IT.t) : t =
        { min_inc = None; min_ex = None; max_inc = Some it; max_ex = None }


      let of_max_ex (it : IT.t) : t =
        { min_inc = None; min_ex = None; max_inc = None; max_ex = Some it }


      let of_it (x : Sym.t) (it : IT.t) : t option =
        let (IT (it_, _, _)) = it in
        match it_ with
        | (Binop (LT, IT (Sym x', _, _), it') | Binop (LTPointer, IT (Sym x', _, _), it'))
          when Sym.equal x x' && not (Sym.Set.mem x (IT.free_vars it')) ->
          (* x < it' -> max_ex = it' *)
          Some (of_max_ex it')
        | (Binop (LE, IT (Sym x', _, _), it') | Binop (LEPointer, IT (Sym x', _, _), it'))
          when Sym.equal x x' && not (Sym.Set.mem x (IT.free_vars it')) ->
          (* x <= it' -> max_inc = it' *)
          Some (of_max_inc it')
        | (Binop (LT, it', IT (Sym x', _, _)) | Binop (LTPointer, it', IT (Sym x', _, _)))
          when Sym.equal x x' && not (Sym.Set.mem x (IT.free_vars it')) ->
          (* it' < x -> min_ex = it' *)
          Some (of_min_ex it')
        | (Binop (LE, it', IT (Sym x', _, _)) | Binop (LEPointer, it', IT (Sym x', _, _)))
          when Sym.equal x x' && not (Sym.Set.mem x (IT.free_vars it')) ->
          (* it' <= x, i.e., x >= it' -> min_inc = it' *)
          Some (of_min_inc it')
        | _ -> None


      (* Helper to cast pointer to integer for arithmetic *)
      let to_numeric it =
        match IT.get_bt it with
        | BT.Loc () -> IT.addr_ it (Locations.other __LOC__)
        | _ -> it


      let intersect
            ({ min_inc = min_inc1;
               min_ex = min_ex1;
               max_inc = max_inc1;
               max_ex = max_ex1
             } :
              t)
            ({ min_inc = min_inc2;
               min_ex = min_ex2;
               max_inc = max_inc2;
               max_ex = max_ex2
             } :
              t)
        : t
        =
        let min_inc =
          match (min_inc1, min_inc2) with
          | Some n1, Some n2 ->
            let loc = Locations.other __LOC__ in
            Some
              (Simplify.IndexTerms.simp
                 (Simplify.default Global.empty)
                 (IT.max_ (to_numeric n1, to_numeric n2) loc))
          | Some n, None | None, Some n -> Some n
          | None, None -> None
        in
        let min_ex =
          match (min_ex1, min_ex2) with
          | Some n1, Some n2 ->
            let loc = Locations.other __LOC__ in
            Some
              (Simplify.IndexTerms.simp
                 (Simplify.default Global.empty)
                 (IT.max_ (to_numeric n1, to_numeric n2) loc))
          | Some n, None | None, Some n -> Some n
          | None, None -> None
        in
        let max_inc =
          match (max_inc1, max_inc2) with
          | Some n1, Some n2 ->
            let loc = Locations.other __LOC__ in
            Some
              (Simplify.IndexTerms.simp
                 (Simplify.default Global.empty)
                 (IT.min_ (to_numeric n1, to_numeric n2) loc))
          | Some n, None | None, Some n -> Some n
          | None, None -> None
        in
        let max_ex =
          match (max_ex1, max_ex2) with
          | Some n1, Some n2 ->
            let loc = Locations.other __LOC__ in
            Some
              (Simplify.IndexTerms.simp
                 (Simplify.default Global.empty)
                 (IT.min_ (to_numeric n1, to_numeric n2) loc))
          | Some n, None | None, Some n -> Some n
          | None, None -> None
        in
        { min_inc; min_ex; max_inc; max_ex }
    end

    (* Collect constraints for variable x from a term, returning modified term and constraints *)
    let rec collect_and_extract_constraints (vars : Sym.Set.t) (x : Sym.t) (gt : Term.t)
      : Term.t * Rep.t
      =
      let empty_rep : Rep.t =
        { Rep.min_inc = None; min_ex = None; max_inc = None; max_ex = None }
      in
      let (Annot (gt_, _, bt, loc)) = gt in
      match gt_ with
      | `Arbitrary | `Symbolic | `ArbitrarySpecialized _ | `ArbitraryDomain _ | `Call _
      | `Return _ ->
        (gt, empty_rep)
      | `Pick gts ->
        (* Collect constraints from all branches *)
        let results = List.map (collect_and_extract_constraints vars x) gts in
        let new_gts = List.map fst results in
        let constraints = List.map snd results in
        let merged_rep = List.fold_left Rep.intersect empty_rep constraints in
        (Term.pick_ new_gts () bt loc, merged_rep)
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        let gt_rest', rep = collect_and_extract_constraints vars x gt_rest in
        (Term.asgn_ ((it_addr, sct), it_val, gt_rest') () loc, rep)
      | `LetStar ((y, gt_inner), gt_rest) ->
        let gt_rest', rep = collect_and_extract_constraints vars x gt_rest in
        (Term.let_star_ ((y, gt_inner), gt_rest') () loc, rep)
      | `Assert (T it, gt_rest)
        when Sym.Set.subset (Sym.Set.remove x (IT.free_vars it)) vars ->
        let gt_rest', rep = collect_and_extract_constraints vars x gt_rest in
        (match Rep.of_it x it with
         | Some rep' ->
           (* Found a constraint, extract it and merge *)
           (gt_rest', Rep.intersect rep rep')
         | None ->
           (* Not a constraint for x, keep the assertion *)
           (Term.assert_ (T it, gt_rest') () loc, rep))
      | `Assert (lc, gt_rest) ->
        let gt_rest', rep = collect_and_extract_constraints vars x gt_rest in
        (Term.assert_ (lc, gt_rest') () loc, rep)
      | `AssertDomain (ad, gt_rest) ->
        let gt_rest', rep = collect_and_extract_constraints vars x gt_rest in
        (Term.assert_domain_ (ad, gt_rest') () loc, rep)
      | `ITE (it_if, gt_then, gt_else) ->
        let gt_then', rep_then = collect_and_extract_constraints vars x gt_then in
        let gt_else', rep_else = collect_and_extract_constraints vars x gt_else in
        let merged_rep = Rep.intersect rep_then rep_else in
        (Term.ite_ (it_if, gt_then', gt_else') () loc, merged_rep)
      | `Map ((i_sym, i_bt, it_perm), gt_inner) ->
        let gt_inner', rep = collect_and_extract_constraints vars x gt_inner in
        (Term.map_ ((i_sym, i_bt, it_perm), gt_inner') () loc, rep)


    (* Convert an Arbitrary term to ArbitrarySpecialized if we have constraints *)
    let apply_constraints (v : Rep.t) (gt : Term.t) : Term.t =
      match gt with
      | Annot (`Arbitrary, _, bt, loc) ->
        (* Only create arbitrary_specialized if at least one constraint exists *)
        if
          Option.is_some v.min_inc
          || Option.is_some v.min_ex
          || Option.is_some v.max_inc
          || Option.is_some v.max_ex
        then (
          (* Cast bounds to match target type *)
          let cast_to_bt it = IT.cast_ bt it loc in
          Term.arbitrary_specialized_
            ( (Option.map cast_to_bt v.min_inc, Option.map cast_to_bt v.min_ex),
              (Option.map cast_to_bt v.max_inc, Option.map cast_to_bt v.max_ex) )
            ()
            bt
            loc)
        else
          gt (* No constraints, keep as Arbitrary *)
      | _ -> gt


    let rec specialize (vars : Sym.Set.t) (gt : Term.t) : Term.t =
      let (Annot (gt_, _, bt, loc)) = gt in
      match gt_ with
      | `Arbitrary | `Symbolic | `ArbitrarySpecialized _ | `ArbitraryDomain _ | `Call _
      | `Return _ ->
        gt
      | `Pick gts -> Term.pick_ (List.map (specialize vars) gts) () bt loc
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        Term.asgn_ ((it_addr, sct), it_val, specialize vars gt_rest) () loc
      | `LetStar ((x, Annot (`Arbitrary, _, bt_x, loc_x)), gt_rest)
      (* Try to collect constraints for bits/loc types *)
        when BT.equal bt_x (BT.Loc ()) || Option.is_some (BT.is_bits_bt bt_x) ->
        let gt_rest', constraints = collect_and_extract_constraints vars x gt_rest in
        let gt_x = apply_constraints constraints (Term.arbitrary_ () bt_x loc_x) in
        Term.let_star_ ((x, gt_x), specialize (Sym.Set.add x vars) gt_rest') () loc
      | `LetStar ((x, gt_inner), gt_rest) ->
        Term.let_star_
          ((x, specialize vars gt_inner), specialize (Sym.Set.add x vars) gt_rest)
          ()
          loc
      | `Assert (lc, gt_rest) -> Term.assert_ (lc, specialize vars gt_rest) () loc
      | `AssertDomain (ad, gt_rest) ->
        Term.assert_domain_ (ad, specialize vars gt_rest) () loc
      | `ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (it_if, specialize vars gt_then, specialize vars gt_else) () loc
      | `Map ((i_sym, i_bt, it_perm), gt_inner) ->
        Term.map_
          ((i_sym, i_bt, it_perm), specialize (Sym.Set.add i_sym vars) gt_inner)
          ()
          loc


    let transform_def (gd : Def.t) : Def.t =
      { gd with body = specialize Sym.Set.empty gd.body }


    let transform (ctx : Ctx.t) : Ctx.t = List.map_snd transform_def ctx
  end
end
