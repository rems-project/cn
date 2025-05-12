module Extract = Extract
module Cn_to_ail = Cn_to_ail
module Globals = Globals
module Internal = Internal
module Ownership = Ownership
module Records = Records
module Utils = Utils

val get_instrumented_filename : string -> string

val main
  :  ?without_ownership_checking:bool ->
  ?without_loop_invariants:bool ->
  ?with_loop_leak_checks:bool ->
  ?with_testing:bool ->
  String.t ->
  String.t ->
  String.t ->
  String.t option ->
  string list ->
  Sym.t option * Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
  unit Mucore.file ->
  unit
