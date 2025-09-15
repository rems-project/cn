module Make (AD : Domain.T) = struct
  module Convert = Convert.Make (AD)

  let transform (prog5 : unit Mucore.file) (ctx : Stage2.Make(AD).Ctx.t) =
    Convert.transform prog5 ctx
end
