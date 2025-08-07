module Make (AD : Domain.T) = struct
  open struct
    module Implication = Implication.Make (AD)
    module Disjunction = Disjunction.Make (AD)
    module Ite = Ite.Make (AD)
    module Let = Let.Make (AD)
    module Conjunction = Conjunction.Make (AD)
  end

  module Term = Term.Make (AD)

  let transform_gt (gt : Term.t) =
    Cerb_debug.print_debug 2 [] (fun () -> "lift_constraints");
    gt
    |> Implication.transform_gt
    |> Disjunction.transform_gt
    |> Ite.transform_gt
    |> Let.transform_gt
    |> Conjunction.transform_gt
end
