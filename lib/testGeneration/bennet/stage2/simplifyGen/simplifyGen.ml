module Make (AD : Domain.T) = struct
  open struct
    module LiftConstraints = LiftConstraints.Make (AD)
    module BranchPruning = BranchPruning.Make (AD)
    module SimplifyIndexTerm = SimplifyIndexTerm.Make (AD)
    module RemoveUnused = RemoveUnused.Make (AD)
    module InlineTerm = InlineTerm.Make (AD)
    module PushPull = PushPull.Make (AD)
    module PartialEvaluation = PartialEvaluation.Make (AD)
  end

  module MemberIndirection = MemberIndirection.Make (AD)
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let transform_gt prog5 gt =
    let rec aux gt fuel =
      if fuel <= 0 then
        gt
      else (
        let old_gt = gt in
        let new_gt =
          gt
          |> LiftConstraints.transform_gt
          |> BranchPruning.transform_gt
          |> SimplifyIndexTerm.transform_gt prog5
          |> RemoveUnused.transform_gt
          |> InlineTerm.transform_gt
          |> PushPull.transform_gt
          |> PartialEvaluation.transform_gt prog5
          |>
          if TestGenConfig.is_experimental_product_arg_destruction () then
            fun ctx -> ctx
          else
            MemberIndirection.transform_gt
        in
        if Term.equal old_gt new_gt then new_gt else aux new_gt (fuel - 1))
    in
    aux gt 5


  let transform_gd (prog5 : unit Mucore.file) (gd : Def.t) : Def.t =
    { gd with body = transform_gt prog5 gd.body }


  let transform (prog5 : unit Mucore.file) (ctx : Ctx.t) =
    List.map_snd (transform_gd prog5) ctx
end
