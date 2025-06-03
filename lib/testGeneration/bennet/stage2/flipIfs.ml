module IT = IndexTerms

let transform_gd (gd : Def.t) : Def.t =
  let iargs = gd.iargs |> List.map fst |> Sym.Set.of_list in
  let rec aux (gt : Term.t) : Term.t =
    let (GT (gt_, _, loc)) = gt in
    match gt_ with
    | Uniform | Alloc | Call _ | Return _ -> gt
    | Pick wgts -> Term.pick_ (List.map_snd aux wgts) loc
    | ITE (it_if, gt_then, gt_else) ->
      let gt_then, gt_else = (aux gt_then, aux gt_else) in
      if not (Sym.Set.subset (IT.free_vars it_if) iargs) then (
        let wgts1 =
          match gt_then with
          | GT (Pick wgts, _, _) ->
            List.map_snd (fun gt' -> Term.assert_ (T it_if, gt') loc) wgts
          | gt' -> [ (Z.one, Term.assert_ (T it_if, gt') loc) ]
        in
        let wgts2 =
          match gt_else with
          | GT (Pick wgts, _, _) ->
            List.map_snd (fun gt' -> Term.assert_ (T (IT.not_ it_if loc), gt') loc) wgts
          | gt' -> [ (Z.one, Term.assert_ (T (IT.not_ it_if loc), gt') loc) ]
        in
        Term.pick_ (wgts1 @ wgts2) loc)
      else
        Term.ite_ (it_if, gt_then, gt_else) loc
    | Asgn ((it_addr, sct), it_val, gt') ->
      Term.asgn_ ((it_addr, sct), it_val, aux gt') loc
    | LetStar ((x, gt_inner), gt') -> Term.let_star_ ((x, aux gt_inner), aux gt') loc
    | Assert (lc, gt') -> Term.assert_ (lc, aux gt') loc
    | Map ((i, i_bt, it_perm), gt_inner) ->
      Term.map_ ((i, i_bt, it_perm), aux gt_inner) loc
  in
  { gd with body = aux gd.body }


let transform (ctx : Ctx.t) : Ctx.t = List.map_snd transform_gd ctx
