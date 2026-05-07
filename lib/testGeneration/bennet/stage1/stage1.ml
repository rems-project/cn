module Private = struct
  module Convert = Convert
  module DestructProducts = DestructProducts
  module PruneArgs = PruneArgs
  module PruneReturns = PruneReturns
  module Unfold = Unfold
  module Term = Term
  module Def = Def
  module Ctx = Ctx
  module DestructArbitrary = DestructArbitrary
end

module Make (AD : Domain.T) = struct
  module DestructArbitrary = DestructArbitrary.Make (AD)

  open struct
    module Convert = Convert.Make (AD)
    module DestructProducts = DestructProducts.Make (AD)
    module PruneArgs = PruneArgs.Make (AD)
    module PruneReturns = PruneReturns.Make (AD)
    module Unfold = Unfold.Make (AD)
  end

  module Term = Term.Make (AD)
  module Def = Def.Make (AD)
  module Ctx = Ctx.Make (AD)

  let transform filename sigma (prog5 : unit Mucore.file) (tests : Test.t list) : Ctx.t =
    tests
    |> Convert.transform
         sigma
         filename
         (List.map fst prog5.globs)
         prog5.resource_predicates
    |> DestructArbitrary.transform prog5
    |> (if TestGenConfig.is_experimental_product_arg_destruction () then
          DestructProducts.transform prog5
        else
          fun ctx -> ctx)
    |> (if TestGenConfig.is_experimental_return_pruning () then
          PruneReturns.transform prog5
        else
          fun ctx -> ctx)
    |> (if TestGenConfig.is_experimental_arg_pruning () then
          PruneArgs.transform
        else
          fun ctx -> ctx)
    |>
    match TestGenConfig.get_max_unfolds () with
    | Some max -> Unfold.unfold max
    | None -> fun ctx -> ctx
end
