(* The `cn test` command group: engine subcommands (bennet/darcy/lucas), plus a
   deprecated flat interface (`cn test [--symbolic] FILE`) for backward
   compatibility. Cmdliner resolves the first positional of a command group as
   a subcommand name and errors before any default term runs, so the group
   itself cannot handle `cn test FILE`. Instead, main.ml consults
   [wants_legacy] and evaluates a command tree containing [legacy_cmd] (a flat
   `test` command whose Cmd.info is marked deprecated) in place of [cmd]. *)

open Cn
open Cmdliner

let symbolic_flag =
  let doc =
    "(Experimental) Use symbolic execution for test generation instead of concrete value \
     generation."
  in
  let deprecated = "Use 'cn test darcy' instead." in
  Arg.(value & flag & info [ "symbolic" ] ~deprecated ~doc)


(* Deprecated flat interface: `cn test FILE` runs Bennet (the previous default
   behavior) and `cn test --symbolic FILE` still works (as Darcy). It accepts
   the full flat flag set for backward compatibility. *)
let legacy_t =
  let engine =
    let from_symbolic symbolic =
      if symbolic then (
        Cn.Pp.(
          warn_noloc
            !^"Use 'cn test darcy' instead. Plain `cn test` will be removed after July \
               15th.");
        TestGeneration.Darcy)
      else (
        Cn.Pp.(
          warn_noloc
            !^"Use 'cn test bennet' instead. Plain `cn test` will be removed after July \
               15th.");
        TestGeneration.Bennet)
    in
    Term.(const from_symbolic $ symbolic_flag)
  in
  Shared.mk_term ~engine ~engine_flags:(Shared.compose_flags Bennet.term Darcy.term)


let doc =
  "Generates tests for all functions in [FILE] with CN specifications.\n\
  \    The inputs are guaranteed to satisfy the CN precondition.\n\
  \    Engines: bennet (random backtracking search, the default), darcy \
   (symbolic/SMT-based), lucas (randomized refinement of abstract elements).\n\
  \    A script [run_tests.sh] for building and running the tests will be placed in \
   [output-dir]."


let cmd = Cmd.group (Cmd.info "test" ~doc) [ Bennet.cmd; Darcy.cmd; Lucas.cmd ]

let legacy_cmd = Cmd.v (Cmd.info "test" ~doc) legacy_t

(* Whether argv is a legacy flat invocation: `cn test ARG ...` where ARG is
   neither (a prefix of) an engine name nor a help/version request. Engine
   names are prefix-matched because Cmdliner resolves unambiguous command-name
   prefixes. *)
let wants_legacy (argv : string array) : bool =
  let is_prefix_of s cmd' =
    String.length s > 0
    && String.length s <= String.length cmd'
    && String.equal s (String.sub cmd' 0 (String.length s))
  in
  match Array.to_list argv with
  | _exe :: maybe_test :: arg :: _ when is_prefix_of maybe_test "test" ->
    (not (List.exists (is_prefix_of arg) [ "bennet"; "darcy"; "lucas" ]))
    && (not (String.starts_with ~prefix:"--help" arg))
    && not (String.equal arg "--version")
  | _ -> false
