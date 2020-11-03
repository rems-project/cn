type t

val unknown : t

val pp : t -> Pp.document

val update : t -> Location_ocaml.t -> t

val update_a : t -> Cerb_frontend.Annot.annot list -> t

val head_pos_of_location : t -> string * string
