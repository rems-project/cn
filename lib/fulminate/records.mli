val populate_record_map : Extract.instrumentation list -> unit Mucore.file -> unit

val generate_all_record_strs : unit -> string

val generate_c_record_funs
  :  Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
  string * string
