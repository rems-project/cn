module Extract = Extract
module Cn_to_ail = Cn_to_ail
module Internal = Internal
module Ownership = Ownership
module Utils = Utils

val get_instrumented_filename : string -> string

val get_cn_helper_filename : string -> string

val main
  :  ?without_ownership_checking:bool ->
  ?without_loop_invariants:bool ->
  ?with_loop_leak_checks:bool ->
  ?with_test_gen:bool ->
  ?copy_source_dir:bool ->
  String.t ->
  use_preproc:bool ->
  Sym.t option * Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
  string option ->
  string option ->
  unit Mucore.file ->
  unit
