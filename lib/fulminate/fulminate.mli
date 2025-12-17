module Extract = Extract
module Cn_to_ail = Cn_to_ail
module Globals = Globals
module Internal = Internal
module Ownership = Ownership
module Records = Records
module Utils = Utils

val get_instrumented_filename : string -> string

val main
  :  without_ownership_checking:bool ->
  without_loop_invariants:bool ->
  with_loop_leak_checks:bool ->
  without_lemma_checks:bool ->
  exec_c_locs_mode:bool ->
  experimental_ownership_stack_mode:bool ->
  experimental_curly_braces:bool ->
  experimental_lua_runtime:bool ->
  with_testing:bool ->
  skip_and_only:string list * string list ->
  ?max_bump_blocks:int ->
  ?bump_block_size:int ->
  String.t ->
  String.t ->
  String.t ->
  String.t ->
  String.t ->
  Cerb_frontend.Cabs.translation_unit ->
  Sym.t option * Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
  unit Mucore.file ->
  unit
