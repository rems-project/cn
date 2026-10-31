module Private : sig
  module SpecExport = SpecExport
end

module Config = TestGenConfig

type engine = TestGenConfig.engine =
  | Bennet
  | Darcy
  | Lucas

type config = TestGenConfig.t

val default_cfg : config

module Options = TestGenConfig.Options

(** Testing roots: specified, non-[main] function definitions that CN can test,
    restricted by [skip_and_only]. *)
val functions_under_test
  :  with_warning:bool ->
  skip_and_only:string list * string list ->
  Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
  unit Mucore.file ->
  _ Typing.pause ->
  Test.t list

(** The name a testing root is exported under, by which Austen selects it. *)
val function_name : Test.t -> string

(** Write the specification JSON for every function definition, in source order,
    whether or not it is a testing root. *)
val export_specs
  :  path:string ->
  filename:string ->
  Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
  unit Mucore.file ->
  _ Typing.pause ->
  unit
