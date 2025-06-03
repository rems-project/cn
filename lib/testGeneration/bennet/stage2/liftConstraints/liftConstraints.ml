let transform (ctx : Ctx.t) =
  ctx
  |> Conjunction.transform
  |> Let.transform
  |> Ite.transform
  |> Disjunction.transform
  |> Implication.transform
