module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let rec transform_gt (gt : Term.t) : Term.t =
    let (Annot (gt_, tag, bt, loc)) = gt in
    match gt_ with
    | `LetStar ((x, gt_inner), gt_rest) ->
      let gt_inner' = transform_gt gt_inner in
      let gt_rest' = transform_gt gt_rest in
      let gt_rest'' = Term.assert_domain_ (AD.top, [], [], gt_rest') () loc in
      Term.let_star_ ((x, gt_inner'), gt_rest'') tag loc
    | `ITE (it_if, gt_then, gt_else) ->
      let gt_then' = Term.assert_domain_ (AD.top, [], [], transform_gt gt_then) () loc in
      let gt_else' = Term.assert_domain_ (AD.top, [], [], transform_gt gt_else) () loc in
      Term.ite_ (it_if, gt_then', gt_else') tag loc
    | `Pick gts ->
      let gts' =
        List.map
          (fun gt' ->
             let gt'' = transform_gt gt' in
             Term.assert_domain_
               (AD.top, [], [], gt'')
               ()
               (let (Annot (_, _, _, l)) = gt' in
                l))
          gts
      in
      Term.pick_ gts' tag bt loc
    | `Assert (lc, gt_rest) ->
      let gt_rest' = transform_gt gt_rest in
      Term.assert_ (lc, gt_rest') tag loc
    | `AssertDomain (d, its, asgns, gt_rest) ->
      let gt_rest' = transform_gt gt_rest in
      Term.assert_domain_ (d, its, asgns, gt_rest') tag loc
    | `Asgn ((it_addr, sct), it_val, gt_rest) ->
      let gt_rest' = transform_gt gt_rest in
      Term.asgn_ ((it_addr, sct), it_val, gt_rest') tag loc
    | `Map ((i, i_bt, it_perm), gt_inner) ->
      let gt_inner' = transform_gt gt_inner in
      Term.map_ ((i, i_bt, it_perm), gt_inner') tag loc
    | `Arbitrary | `Symbolic | `ArbitrarySpecialized _
    | `ArbitraryDomain (_, _, _)
    | `Call _ | `Return _ ->
      gt


  let transform_gd (gd : Def.t) : Def.t = { gd with body = transform_gt gd.body }

  let transform (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "insert_assert_domain");
    List.map_snd transform_gd ctx
end
