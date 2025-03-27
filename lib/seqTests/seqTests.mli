module CF = Cerb_frontend
module A = CF.AilSyntax

type seq_config = SeqTestGenConfig.t

val default_seq_cfg : seq_config

val set_seq_config : seq_config -> unit

val run_seq
  :  output_dir:string ->
  filename:string ->
  Cerb_frontend.Cabs.translation_unit ->
  Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
  unit Mucore.file ->
  int
