(** This stage tags the AST with backtracking information *)

module Make (AD : Domain.T) = struct
  open struct
    module Convert = Convert.Make (AD)
  end

  module Term = Term.Make (AD)
  module Def = Def.Make (AD)
  module Ctx = Ctx.Make (AD)

  let transform (ctx : Stage3.Make(AD).Ctx.t) : Ctx.t = Convert.transform ctx
end
