(* TODO: BCP: Needs a header explaining what it is *)

module StringSet : Set.S with type elt = string

val io : Cerb_backend.Pipeline.io_helpers

val impl_name : string

(** A command-line macro definition, as the operand of [-D]: [NAME] or
    [NAME=VALUE]. *)
val macro_def : string * string option -> string

val conf
  :  string ->
  (string * string option) list ->
  string list ->
  string list ->
  bool ->
  Cerb_backend.Pipeline.language list ->
  string option ->
  Cerb_backend.Pipeline.configuration
