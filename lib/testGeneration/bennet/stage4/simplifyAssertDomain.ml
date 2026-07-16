module MT = MakeTerm
module T = Terms.Normal

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  type state =
    { ad : AD.t;
      asserts : T.t list;
      asgns : (T.t * Sctypes.t * T.t option) list
    }

  let empty_state = { ad = AD.top; asserts = []; asgns = [] }

  let asgn_equal (a1, s1, e1) (a2, s2, e2) =
    T.equal a1 a2 && Sctypes.equal s1 s2 && Option.equal T.equal e1 e2


  let rec transform_gt (state : state) (gt : Term.t) : Term.t =
    let (GenTerms.Annot (gt_, tag, bt, loc)) = gt in
    match gt_ with
    | `AssertDomain (ad, its, asgns, gt_rest) ->
      let its', asgns' =
        if TestGenConfig.has_dynamic_assert_domain () then
          ( List.filter (fun it -> not (List.exists (T.equal it) state.asserts)) its,
            List.filter (fun a -> not (List.exists (asgn_equal a) state.asgns)) asgns )
        else
          (its, asgns)
      in
      if AD.equal ad state.ad && List.is_empty its' && List.is_empty asgns' then
        transform_gt state gt_rest
      else (
        let state' =
          { ad = AD.meet state.ad ad;
            asserts = its @ state.asserts;
            asgns = asgns @ state.asgns
          }
        in
        let gt_rest' = transform_gt state' gt_rest in
        Term.assert_domain_ (ad, its', asgns', gt_rest') tag loc)
    | `LetStar ((x, gt_inner), gt_rest) ->
      let state' =
        let (GenTerms.Annot (gt_inner_, _, _, _)) = gt_inner in
        match gt_inner_ with
        | `ArbitraryDomain (_, _its, asgns) -> { state with asgns = asgns @ state.asgns }
        | _ -> state
      in
      let gt_inner' = transform_gt empty_state gt_inner in
      let gt_rest' = transform_gt state' gt_rest in
      Term.let_star_ ((x, gt_inner'), gt_rest') tag loc
    | `ITE (it_if, gt_then, gt_else) ->
      let gt_then' = transform_gt state gt_then in
      let gt_else' = transform_gt state gt_else in
      Term.ite_ (it_if, gt_then', gt_else') tag loc
    | `Pick gts ->
      let gts' = List.map (transform_gt state) gts in
      Term.pick_ gts' tag bt loc
    | `Assert (lc, gt_rest) ->
      let gt_rest' = transform_gt state gt_rest in
      Term.assert_ (lc, gt_rest') tag loc
    | `Asgn ((it_addr, sct), it_val, gt_rest) ->
      let gt_rest' = transform_gt state gt_rest in
      Term.asgn_ ((it_addr, sct), it_val, gt_rest') tag loc
    | `Map ((i, i_bt, it_perm), gt_inner) ->
      let gt_inner' = transform_gt state gt_inner in
      Term.map_ ((i, i_bt, it_perm), gt_inner') tag loc
    | `Arbitrary | `Symbolic | `ArbitrarySpecialized _
    | `ArbitraryDomain (_, _, _)
    | `Call _ | `Return _ ->
      gt


  let transform_gd (gd : Def.t) : Def.t =
    { gd with body = transform_gt empty_state gd.body }


  let transform (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "simplify_assert_domain");
    List.map_snd transform_gd ctx
end
