module Make (AD : Domain.T) = struct
  module Convert = Convert.Make (AD)

  let transform (ctx : Stage2.Make(AD).Ctx.t) = Convert.transform ctx
end
