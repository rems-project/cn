(** This stage adds sizing *)

module Make (AD : Domain.T) = struct
  open struct
    module Convert = Convert.Make (AD)
  end

  module Stage4 = Stage4.Make (AD)
  module Term = Term.Make (AD)
  module Def = Def.Make (AD)
  module Ctx = Ctx.Make (AD)

  let transform (ctx : Stage4.Ctx.t) : Ctx.t = Convert.transform ctx
end
