module Make (AD : Domain.T) = struct
  open struct
    module Convert = Convert.Make (AD)
    module DestructArbitrary = DestructArbitrary.Make (AD)
    module DestructProducts = DestructProducts.Make (AD)
  end

  module Term = Term.Make (AD)
  module Def = Def.Make (AD)
  module Ctx = Ctx.Make (AD)

  let transform filename (prog5 : unit Mucore.file) (tests : Test.t list) : Ctx.t =
    tests
    |> Convert.transform filename prog5.resource_predicates
    |> DestructArbitrary.transform prog5
    |>
    if TestGenConfig.is_experimental_product_arg_destruction () then
      DestructProducts.transform prog5
    else
      fun ctx -> ctx
end
