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
  let deprecated = "Use 'cn test darcy' instead." in
  Arg.(value & flag & info [ "symbolic" ] ~deprecated)


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
               15th, 2026.");
        TestGeneration.Darcy)
      else (
        Cn.Pp.(
          warn_noloc
            !^"Use 'cn test bennet' instead. Plain `cn test` will be removed after July \
               15th, 2026.");
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
  \    Paper-release subcommands (e.g. darcy-pldi26) run an engine with frozen settings.\n\
  \    A script [run_tests.sh] for building and running the tests will be placed in \
   [output-dir]."


let cmd =
  Cmd.group (Cmd.info "test" ~doc) ([ Bennet.cmd; Darcy.cmd; Lucas.cmd ] @ Releases.all)


let legacy_cmd = Cmd.v (Cmd.info "test" ~doc) legacy_t

let is_prefix_of s cmd' = String.length s > 0 && String.starts_with ~prefix:s cmd'

(* The argument after `test` in `cn test ARG ...`, when argv has that shape. *)
let test_arg (argv : string array) : string option =
  match Array.to_list argv with
  | _exe :: maybe_test :: arg :: _ when is_prefix_of maybe_test "test" -> Some arg
  | _ -> None


(* The full engine and paper-release subcommand names. *)
let engine_names =
  List.map TestGeneration.Config.cli_name TestGeneration.Config.all_of_engine
  @ Releases.names


(* Engine subcommands must be spelled in full. Cmdliner would otherwise resolve
   an unambiguous command-name prefix (`cn test b` -> bennet), so reject a token
   that is a proper prefix of an engine name but not exactly one, before the
   command tree is evaluated. A token that is not a prefix of any engine name
   (e.g. a file name) is left to the deprecated flat interface. *)
let reject_engine_prefix (argv : string array) : unit =
  match test_arg argv with
  | Some arg
    when (not (List.mem String.equal arg engine_names))
         && List.exists (is_prefix_of arg) engine_names ->
    Printf.eprintf
      "cn test: `%s` is not a recognized engine; use a full name (%s).\n\
       Try 'cn test --help' for more information.\n"
      arg
      (String.concat ", " engine_names);
    Stdlib.exit Cmd.Exit.cli_error
  | _ -> ()


(* Whether argv is a legacy flat invocation: `cn test ARG ...` where ARG is
   neither an engine name nor a help/version request. Partial engine names are
   already rejected by [reject_engine_prefix], so an exact-name check suffices. *)
let wants_legacy (argv : string array) : bool =
  match test_arg argv with
  | Some arg ->
    (not (List.mem String.equal arg engine_names))
    && (not (String.starts_with ~prefix:"--help" arg))
    && not (String.equal arg "--version")
  | None -> false
