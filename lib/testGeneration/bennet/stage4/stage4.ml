(** This stage adds sizing *)

module Make (AD : Domain.T) = struct
  open struct
    module Convert = Convert.Make (AD)
  end

  module Stage3 = Stage3.Make (AD)
  module Term = Term.Make (AD)
  module Def = Def.Make (AD)
  module Ctx = Ctx.Make (AD)

  let transform (ctx : Stage3.Ctx.t) : Ctx.t = Convert.transform ctx
end
