module CF = Cerb_frontend
module A = CF.AilSyntax

val log_filename : string ref

val get_log_filename : string -> string

val run_autoannot : string -> unit
