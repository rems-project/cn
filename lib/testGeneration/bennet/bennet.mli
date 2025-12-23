module Private : sig
  module AbstractDomains = AbstractDomains
  module Stage1 = Stage1
end

val test_setup : unit -> Pp.document

val synthesize
  :  string ->
  Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
  unit Mucore.file ->
  _ Typing.pause ->
  Test.t list ->
  Pp.document
