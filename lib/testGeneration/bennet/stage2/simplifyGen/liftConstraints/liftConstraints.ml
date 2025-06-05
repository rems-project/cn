let transform_gt (gt : Term.t) =
  gt
  |> Implication.transform_gt
  |> Disjunction.transform_gt
  |> Ite.transform_gt
  |> Let.transform_gt
  |> Conjunction.transform_gt
