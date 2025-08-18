module CF = Cerb_frontend
module A = CF.AilSyntax

val log_filename : string ref

val get_log_filename : string -> string

val run_autoannot
  :  _output_dir:string ->
  _filename:string ->
  CF.Cabs.translation_unit ->
  Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
  unit Mucore.file ->
  int
