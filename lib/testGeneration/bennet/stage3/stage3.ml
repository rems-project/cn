(** Stage 3 - Equality specialization and name simplification *)

module Make (AD : Domain.T) = struct
  open struct
    module SpecializeEquality = SpecializeEquality.Make (AD)
    module SimplifyNames = SimplifyNames.Make (AD)
  end

  module Stage2 = Stage2.Make (AD)
  module Term = Stage2.Term
  module Def = Stage2.Def
  module Ctx = Stage2.Ctx

  let transform (ctx : Stage2.Ctx.t) : Ctx.t =
    ctx |> SpecializeEquality.transform |> SimplifyNames.transform
end
