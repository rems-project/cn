module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let rec transform_gt (gt : Term.t) : Term.t =
    let (GenTerms.Annot (gt_, _tag, bt, loc)) = gt in
    match gt_ with
    | `Arbitrary | `Lazy | `Symbolic | `Call _ | `Return _ -> gt
    | `Asgn ((it_addr, sct), it_val, gt_rest) ->
      Term.asgn_ ((it_addr, sct), it_val, transform_gt gt_rest) () loc
    | `LetStar
        ((x, (GenTerms.Annot (`Arbitrary, _, inner_bt, inner_loc) as _gt_inner)), gt_rest)
      ->
      Term.let_star_ ((x, Term.lazy_ () inner_bt inner_loc), transform_gt gt_rest) () loc
    | `LetStar ((x, gt_inner), gt_rest) ->
      Term.let_star_ ((x, transform_gt gt_inner), transform_gt gt_rest) () loc
    | `Assert (lc, gt_rest) -> Term.assert_ (lc, transform_gt gt_rest) () loc
    | `ITE (it_if, gt_then, gt_else) ->
      Term.ite_ (it_if, transform_gt gt_then, transform_gt gt_else) () loc
    | `Map ((i, i_bt, it_perm), gt_inner) ->
      Term.map_ ((i, i_bt, it_perm), transform_gt gt_inner) () loc
    | `Pick gts -> Term.pick_ (List.map transform_gt gts) () bt loc
    | `Instantiate ((x, gt_inner), gt_rest) ->
      Term.instantiate_ ((x, transform_gt gt_inner), transform_gt gt_rest) () loc


  let transform_gd (gd : Def.t) : Def.t = { gd with body = transform_gt gd.body }

  let transform (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "lazify");
    List.map_snd transform_gd ctx
end
