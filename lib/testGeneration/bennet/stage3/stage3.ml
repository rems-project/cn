(** Stage 3 - Lazy generation transformation

    This stage adds Instantiate nodes for lazily-generated values.
    When lazy generation is enabled, values are only instantiated
    when they are first used in the generator. *)

module Make (AD : Domain.T) = struct
  open struct
    module Convert = Convert.Make (AD)
    module Instantiate = Instantiate.Make (AD)
    module SpecializeEquality = SpecializeEquality.Make (AD)
    module SimplifyNames = SimplifyNames.Make (AD)
  end

  module Stage2 = Stage2.Make (AD)
  module Term = Term.Make (AD)
  module Def = Def.Make (AD)
  module Ctx = Ctx.Make (AD)

  let transform (ctx : Stage2.Ctx.t) : Ctx.t =
    ctx
    |> Convert.transform
    |> SpecializeEquality.transform
    |> (if TestGenConfig.is_lazy_gen () then
          fun ctx -> ctx |> Instantiate.transform |> SpecializeEquality.transform
        else
          Fun.id)
    |> SimplifyNames.transform
end
