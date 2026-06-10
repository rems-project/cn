(* The `cn test` command group: engine subcommands (bennet/darcy/lucas) plus
   the deprecated flat `--symbolic` interface as the default term. *)

open Cn
open Cmdliner

let symbolic_flag =
  let doc =
    "(Experimental) Use symbolic execution for test generation instead of concrete value \
     generation."
  in
  let deprecated = "Use 'cn test darcy' instead." in
  Arg.(value & flag & info [ "symbolic" ] ~deprecated ~doc)


let warn_default_engine () =
  prerr_endline
    "cn test: deprecated: no engine selected, defaulting to 'bennet'; select one \
     explicitly with 'cn test bennet|darcy|lucas'"


(* Deprecated flat interface: `cn test FILE` runs bennet (the previous default
   behavior) and `cn test --symbolic FILE` still works (as darcy). It accepts
   the full flat flag set for backward compatibility. *)
let legacy_t =
  let engine =
    let from_symbolic symbolic =
      if symbolic then
        TestGeneration.Darcy
      else (
        warn_default_engine ();
        TestGeneration.Bennet)
    in
    Term.(const from_symbolic $ symbolic_flag)
  in
  Shared.mk_term ~engine ~engine_flags:(Shared.compose_flags Bennet.term Darcy.term)


(* Cmdliner resolves the first positional of a command group as a subcommand
   name and errors before the default term can run, so `cn test FILE` would
   fail with "unknown command". Rewrite argv to insert "bennet" when the
   argument after "test" is neither a flag nor (a prefix of) an engine name,
   preserving the old default. *)
let argv_with_default_engine (argv : string array) : string array =
  let is_prefix_of s cmd =
    String.length s > 0
    && String.length s <= String.length cmd
    && String.equal s (String.sub cmd 0 (String.length s))
  in
  match Array.to_list argv with
  | exe :: maybe_test :: arg :: rest
    when is_prefix_of maybe_test "test"
         && (not (String.starts_with ~prefix:"-" arg))
         && not (List.exists (is_prefix_of arg) [ "bennet"; "darcy"; "lucas" ]) ->
    warn_default_engine ();
    Array.of_list (exe :: maybe_test :: "bennet" :: arg :: rest)
  | _ -> argv


let cmd =
  let doc =
    "Generates tests for all functions in [FILE] with CN specifications.\n\
    \    The inputs are guaranteed to satisfy the CN precondition.\n\
    \    Engines: bennet (random backtracking search, the default), darcy \
     (symbolic/SMT-based), lucas (randomized refinement of abstract elements).\n\
    \    A script [run_tests.sh] for building and running the tests will be placed in \
     [output-dir]."
  in
  Cmd.group ~default:legacy_t (Cmd.info "test" ~doc) [ Bennet.cmd; Darcy.cmd; Lucas.cmd ]
