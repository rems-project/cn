let transform_gt (gt : Term.t) =
  gt
  |> Implication.transform_gt
  |> Disjunction.transform_gt
  |> Ite.transform_gt
  |> Let.transform_gt
  |> Conjunction.transform_gt


let transform_gd (gd : Def.t) = { gd with body = transform_gt gd.body }

let transform (ctx : Ctx.t) = List.map_snd transform_gd ctx
