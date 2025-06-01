module Term = Term
module Def = Def
module Ctx = Ctx

let transform filename (prog5 : unit Mucore.file) (tests : Test.t list)
  : GenContext.Make(Term).t
  =
  tests
  |> Convert.transform filename prog5.resource_predicates
  |> Destruct_arbitrary.transform prog5
