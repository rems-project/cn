module Extract = Extract
module Cn_to_ail = Cn_to_ail
module Globals = Globals
module Internal = Internal
module Ownership = Ownership
module Records = Records
module Utils = Utils

val get_instrumented_filename : string -> string

val get_uninterp_filename : string -> string

val main
  :  cerb_conf:Cerb_frontend.Config.config ->
  without_ownership_checking:bool ->
  without_loop_invariants:bool ->
  with_loop_leak_checks:bool ->
  without_lemma_checks:bool ->
  without_inline_statements:bool ->
  source_locs_mode:bool ->
  correct_missing_ownership_mode:bool ->
  remove_uninterp:bool ->
  accept_uninterp:bool ->
  experimental_ownership_stack_mode:bool ->
  with_testing:bool ->
  skip_and_only:string list * string list ->
  disable_ghost_arg_failure:bool ->
  ?max_bump_blocks:int ->
  ?bump_block_size:int ->
  String.t ->
  String.t ->
  String.t ->
  String.t ->
  String.t ->
  Sym.t option * Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
  unit Mucore.file ->
  unit
