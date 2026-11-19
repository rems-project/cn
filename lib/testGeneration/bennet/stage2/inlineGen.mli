module Make (AD : Domain.T) : sig
  module InlineNonRecursive : sig
    val transform : Ctx.Make(AD).t -> Ctx.Make(AD).t
  end

  module InlineRecursive : sig
    val transform : Ctx.Make(AD).t -> Ctx.Make(AD).t
  end

  val transform : unit Mucore.file -> Ctx.Make(AD).t -> Ctx.Make(AD).t
end
