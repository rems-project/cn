val loc : Locations.t

val history_str : string
val history_sym : Sym.t

val predicate_str : string
val predicate_sym : Sym.t
val predicate_name : Request.name

module F (_: Bt_of_sct.Repr) : sig

module History : sig
  val str : string

  val sym : Sym.t

  val base_id : Id.t

  val base_bt : BaseTypes.t

  val size_id : Id.t

  val size_bt : BaseTypes.t

  val value_bt : BaseTypes.t

  val make_value : base:Terms.Normal.t -> size:int -> Locations.t -> Terms.Normal.t

  val bt : BaseTypes.t

  val it : Cerb_location.t -> Terms.Normal.t

  val lookup_ptr : Terms.Normal.t -> Locations.t -> Terms.Normal.t

  type value =
    { base : Terms.Normal.t;
      size : Terms.Normal.t
    }

  val split : Terms.Normal.t -> Cerb_location.t -> value

  val sbt : BaseTypes.Surface.t
end

module Predicate : sig
  val str : string

  val sym : Sym.t

  val name : Request.name

  val def : Definition.Predicate.t

  val make_request : Terms.Normal.t -> Request.Predicate.t

end

end
