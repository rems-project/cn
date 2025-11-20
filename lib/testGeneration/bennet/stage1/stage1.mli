module Private : sig
  module Convert = Convert
  module DestructProducts = DestructProducts
  module PruneArgs = PruneArgs
  module PruneReturns = PruneReturns
  module Unfold = Unfold
  module Term = Term
  module Def = Def
  module Ctx = Ctx
  module DestructArbitrary = DestructArbitrary
end

module Make (AD : Domain.T) : sig
  module DestructArbitrary : module type of DestructArbitrary.Make (AD)

  module Term : module type of Term.Make (AD)

  module Def : module type of Def.Make (AD)

  module Ctx : module type of Ctx.Make (AD)

  val transform
    :  string ->
    Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
    unit Mucore.file ->
    Test.t list ->
    Ctx.t
end
