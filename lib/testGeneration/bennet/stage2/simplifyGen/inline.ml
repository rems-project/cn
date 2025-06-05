module IT = IndexTerms
module LC = LogicalConstraints

(** This pass inlines generators that just return a constant or symbol *)
module Returns = struct
  let name = "inline_return"

  let transform_gt (gt : Term.t) : Term.t =
    let aux (gt : Term.t) : Term.t =
      let (GT (gt_, _, _loc)) = gt in
      match gt_ with
      | LetStar ((x, GT (Return it, _, _)), gt') ->
        Term.subst (IT.make_subst [ (x, it) ]) gt'
      | _ -> gt
    in
    Term.map_gen_pre aux gt
end

(* This pass inlines terms used a single time *)
module SingleUse = struct
  let name = "inline_single_use_gen"

  let subst (x : Sym.t) (gt_repl : Term.t) (gt : Term.t) : Term.t =
    let aux (gt : Term.t) : Term.t =
      let (GT (gt_, _, _)) = gt in
      match gt_ with Return (IT (Sym y, _, _)) when Sym.equal x y -> gt_repl | _ -> gt
    in
    Term.map_gen_post aux gt


  let of_symset (s : Sym.Set.t) : bool Sym.Map.t =
    s |> Sym.Set.to_seq |> Seq.map (fun x -> (x, false)) |> Sym.Map.of_seq


  let union = Sym.Map.union (fun _ a b -> Some (not (a || b)))

  let rec transform_aux (gt : Term.t) : Term.t * bool Sym.Map.t =
    let (GT (gt_, _, loc)) = gt in
    match gt_ with
    | Uniform | Alloc -> (gt, Sym.Map.empty)
    | Pick wgts ->
      let wgts, only_ret =
        wgts
        |> List.map_snd transform_aux
        |> List.map (fun (a, (b, c)) -> ((a, b), c))
        |> List.split
      in
      (Term.pick_ wgts loc, List.fold_left union Sym.Map.empty only_ret)
    | Call (_fsym, xits) ->
      ( gt,
        xits
        |> List.map snd
        |> List.map IT.free_vars
        |> List.map of_symset
        |> List.fold_left union Sym.Map.empty )
    | Asgn ((it_addr, sct), it_val, gt') ->
      let only_ret =
        [ it_addr; it_val ]
        |> List.map IT.free_vars
        |> List.map of_symset
        |> List.fold_left union Sym.Map.empty
      in
      let gt', only_ret' = transform_aux gt' in
      (Term.asgn_ ((it_addr, sct), it_val, gt') loc, union only_ret only_ret')
    | LetStar ((x, gt_inner), gt') ->
      let gt', only_ret = transform_aux gt' in
      let only_ret = Sym.Map.remove x only_ret in
      if Option.equal Bool.equal (Sym.Map.find_opt x only_ret) (Some true) then
        (subst x gt_inner gt', only_ret)
      else (
        let gt_inner, only_ret' = transform_aux gt_inner in
        (Term.let_star_ ((x, gt_inner), gt') loc, union only_ret only_ret'))
    | Return it ->
      ( gt,
        (match IT.is_sym it with
         | Some (x, _bt) -> Sym.Map.singleton x true
         | None -> it |> IT.free_vars |> of_symset) )
    | Assert (lc, gt') ->
      let only_ret = lc |> LC.free_vars |> of_symset in
      let gt', only_ret' = transform_aux gt' in
      (Term.assert_ (lc, gt') loc, union only_ret only_ret')
    | ITE (it_if, gt_then, gt_else) ->
      let only_ret = it_if |> IT.free_vars |> of_symset in
      let gt_then, only_ret' = transform_aux gt_then in
      let gt_else, only_ret'' = transform_aux gt_else in
      ( Term.ite_ (it_if, gt_then, gt_else) loc,
        [ only_ret; only_ret'; only_ret'' ] |> List.fold_left union Sym.Map.empty )
    | Map ((i, i_bt, it_perm), gt_inner) ->
      let only_ret = it_perm |> IT.free_vars |> Sym.Set.remove i |> of_symset in
      let gt_inner, only_ret' = transform_aux gt_inner in
      let only_ret' = only_ret' |> Sym.Map.remove i |> Sym.Map.map (fun _ -> false) in
      (Term.map_ ((i, i_bt, it_perm), gt_inner) loc, union only_ret only_ret')


  let transform_gt (gt : Term.t) : Term.t = fst (transform_aux gt)
end

let transform_gt (gt : Term.t) = gt |> SingleUse.transform_gt |> Returns.transform_gt
