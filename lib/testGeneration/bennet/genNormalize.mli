module MemberIndirection : sig
  val transform : GenTerms.t -> GenTerms.t
end

val normalize
  :  unit Mucore.file ->
  GenContext.Make(GenTerms).t ->
  GenContext.Make(GenTerms).t
