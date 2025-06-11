module Term = Term
module Def = Def
module Ctx = Ctx

let transform filename (prog5 : unit Mucore.file) (tests : Test.t list)
  : GenContext.Make(Term).t
  =
  tests
  |> Convert.transform filename prog5.resource_predicates
  |> DestructArbitrary.transform prog5
  |>
  if TestGenConfig.is_experimental_product_arg_destruction () then
    DestructProducts.transform prog5
  else
    fun ctx -> ctx
