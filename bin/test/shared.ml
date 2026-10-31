(* Shared infrastructure for the `cn test` engine subcommands. *)

open Cn

(* An engine's flags are a config updater over the config built from the
   common flags. *)
type engine_flags = TestGeneration.config -> TestGeneration.config

(* Temporary: the previous generator pipeline is disconnected and the Austen
   backend is not yet connected, so every testing command fails here, after
   argument parsing and before preprocessing or creating any output. *)
let run (engine : TestGeneration.engine) : unit =
  let name = TestGeneration.Config.cli_name engine in
  (match engine with
   | Lucas -> Printf.eprintf "cn test %s: the Lucas engine is not supported.\n" name
   | Bennet | Darcy ->
     Printf.eprintf
       "cn test %s: the testing backend is temporarily unavailable.\n\
        Use 'cn test export FILE -o PATH' to export the specifications.\n"
       name);
  exit 2


open Cmdliner

(* Section titles for grouping a test engine's flags in `--help`/man output, so
   related flags read consistently across the engine subcommands. *)
let s_generation = "GENERATION OPTIONS"

let s_selection = "SELECTION OPTIONS"

let s_output = "OUTPUT OPTIONS"

let s_compilation = "COMPILATION OPTIONS"

let s_logging = "LOGGING & DIAGNOSTICS"

(* Where a test engine's experimental flag is filed in `--help`: under its
   natural [section] on experimental engines, hidden (omitted from help/man via
   [Manpage.s_none]) on stable ones. An experimental engine is already flagged
   as not-for-general-use, so its help surfaces every knob beside its stable
   siblings; a stable engine presents a clean, stable-only help. *)
let experimental_docs (engine : TestGeneration.engine) (section : string) : string =
  if TestGeneration.Config.is_experimental engine then section else Manpage.s_none


(* Ordered section headers for a test engine's man page. [extra] are
   engine-specific sections (e.g. SMT / abstract interpretation) shown after the
   shared ones. Experimental flags are filed under these same natural sections
   (see [experimental_docs]), so there is no separate experimental section. *)
let man_sections ~(extra : string list) : Manpage.block list =
  let shared = [ s_generation; s_selection; s_output; s_compilation; s_logging ] in
  List.map (fun s -> `S s) (shared @ extra @ [ Common.s_cn ])


(* Parse size value with optional suffix (k/K for KB, m/M for MB, g/G for GB) *)
let parse_size_value s =
  let len = String.length s in
  if len = 0 then
    Error (`Msg "Size value cannot be empty")
  else (
    let last_char = String.get s (len - 1) in
    let value_str, multiplier =
      match last_char with
      | 'k' | 'K' -> (String.sub s 0 (len - 1), 1024)
      | 'm' | 'M' -> (String.sub s 0 (len - 1), 1024 * 1024)
      | 'g' | 'G' -> (String.sub s 0 (len - 1), 1024 * 1024 * 1024)
      | '0' .. '9' -> (s, 1)
      | _ -> ("", 0)
      (* Invalid suffix *)
    in
    if multiplier = 0 then
      Error
        (`Msg
            (Printf.sprintf
               "Invalid size suffix in '%s'. Use k/K, m/M, g/G, or no suffix."
               s))
    else (
      match int_of_string_opt value_str with
      | Some n when n > 0 ->
        (* Check for overflow *)
        if n > max_int / multiplier then
          Error (`Msg (Printf.sprintf "Size value '%s' is too large" s))
        else
          Ok (n * multiplier)
      | Some _ -> Error (`Msg (Printf.sprintf "Size value must be positive: '%s'" s))
      | None -> Error (`Msg (Printf.sprintf "Invalid size value: '%s'" s))))


let size_converter = Arg.conv (parse_size_value, fun ppf n -> Format.fprintf ppf "%d" n)

(* Flags shared by all engines *)
module Flags = struct
  let print_steps =
    let doc =
      "Print successful stages, such as directory creation, compilation and linking."
    in
    Arg.(value & flag & info ~docs:s_output [ "print-steps" ] ~doc)


  let output_dir =
    let doc = "Place generated tests in the provided directory" in
    Arg.(
      value
      & opt (some string) None
      & info ~docs:s_output [ "output-dir" ] ~docv:"DIR" ~doc)


  let only =
    let doc = "Only test this function (or comma-separated names)" in
    Term.(
      const List.concat
      $ Arg.(value & opt_all (list string) [] & info ~docs:s_selection [ "only" ] ~doc))


  let skip =
    let doc = "Skip testing of this function (or comma-separated names)" in
    Term.(
      const List.concat
      $ Arg.(value & opt_all (list string) [] & info ~docs:s_selection [ "skip" ] ~doc))


  let only_fulminate =
    let doc =
      "Only check the pre- and post-conditions of this function (or comma-separated \
       names)"
    in
    Term.(
      const List.concat
      $ Arg.(
          value
          & opt_all (list string) []
          & info ~docs:s_selection [ "only-fulminate" ] ~doc))


  let skip_fulminate =
    let doc =
      "Skip checking the pre- and post-conditions of this function (or comma-separated \
       names)"
    in
    Term.(
      const List.concat
      $ Arg.(
          value
          & opt_all (list string) []
          & info ~docs:s_selection [ "skip-fulminate" ] ~doc))


  let dont_run =
    let doc = "Do not run tests, only generate them" in
    Arg.(value & flag & info ~docs:s_output [ "no-run" ] ~doc)


  let gen_max_unfolds =
    let doc =
      "Maximum number of times to inline function calls in symbolic mode (default: 10)"
    in
    Arg.(value & opt (some int) None & info ~docs:s_generation [ "max-unfolds" ] ~doc)


  let sanitize =
    let doc = "Forwarded to the '-fsanitize' argument of the C compiler" in
    Arg.(
      value
      & opt (some string) (fst TestGeneration.default_cfg.sanitizers)
      & info ~docs:s_compilation [ "sanitize" ] ~doc)


  let no_sanitize =
    let doc = "Forwarded to the '-fno-sanitize' argument of the C compiler" in
    Arg.(
      value
      & opt (some string) (snd TestGeneration.default_cfg.sanitizers)
      & info ~docs:s_compilation [ "no-sanitize" ] ~doc)


  let print_seed =
    let doc = "Print seed used by PRNG." in
    Arg.(value & flag & info ~docs:s_generation [ "print-seed" ] ~doc)


  let input_timeout =
    let doc = "Timeout for discarding a generation attempt (ms)" in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.input_timeout
      & info ~docs:s_generation [ "input-timeout" ] ~doc)


  let null_in_every =
    let doc = "Set the likelihood of NULL being generated as 1 in every <n>" in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.null_in_every
      & info ~docs:s_generation [ "null-in-every" ] ~doc)


  let seed =
    let doc = "Set the seed for random testing" in
    Arg.(
      value
      & opt (some string) TestGeneration.default_cfg.seed
      & info ~docs:s_generation [ "seed" ] ~doc)


  let logging_level =
    let doc = "Set the logging level for failing inputs from tests" in
    Arg.(
      value
      & opt
          (some (enum TestGeneration.Options.logging_level))
          TestGeneration.default_cfg.logging_level
      & info ~docs:s_logging [ "logging-level" ] ~doc)


  let trace_granularity =
    let doc = "Set the trace granularity for failing inputs from tests" in
    Arg.(
      value
      & opt
          (some (enum TestGeneration.Options.trace_granularity))
          TestGeneration.default_cfg.trace_granularity
      & info ~docs:s_logging [ "trace-granularity" ] ~doc)


  let progress_level =
    let doc = "Set the frequency of progress updates." in
    Arg.(
      value
      & opt
          (some (enum TestGeneration.Options.progress_level))
          TestGeneration.default_cfg.progress_level
      & info ~docs:s_logging [ "progress-level" ] ~doc)


  let until_timeout =
    let doc = "Run tests until the given timeout (in seconds) has been reached" in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.until_timeout
      & info ~docs:s_generation [ "until-timeout" ] ~doc)


  let exit_fast =
    let doc = "Stop testing upon finding the first failure" in
    Arg.(value & flag & info ~docs:s_generation [ "exit-fast" ] ~doc)


  let coverage ~docs =
    let doc = "(Experimental) Record coverage of tests via [lcov]" in
    Arg.(value & flag & info ~docs [ "coverage" ] ~doc)


  let disable_specialization =
    let doc = "Disable integer specialization in the generator pipeline" in
    Arg.(value & flag & info ~docs:s_generation [ "disable-specialization" ] ~doc)


  let disable_passes =
    let doc = "skip this optimization pass (or comma-separated names)" in
    Arg.(
      value
      & opt
          (list
             (enum
                [ ("reorder", "reorder");
                  ("picks", "picks");
                  ("flatten", "flatten");
                  ("consistency", "consistency");
                  ("lift_constraints", "lift_constraints")
                ]))
          []
      & info ~docs:s_generation [ "disable" ] ~doc)


  let trap =
    let doc = "Raise SIGTRAP on test failure" in
    Arg.(value & flag & info ~docs:s_logging [ "trap" ] ~doc)


  let no_replays =
    let doc = "Disable replaying errors for error messages" in
    Arg.(value & flag & info ~docs:s_logging [ "no-replays" ] ~doc)


  let no_replicas =
    let doc = "Disable synthesizing C code to replicate bugs" in
    Arg.(value & flag & info ~docs:s_logging [ "no-replicas" ] ~doc)


  let output_tyche =
    let doc = "Enable output in Tyche format" in
    Arg.(
      value
      & opt (some string) TestGeneration.default_cfg.output_tyche
      & info ~docs:s_output [ "output-tyche" ] ~doc)


  let export_spec_json =
    let doc =
      "Dump the specification IR as one JSON file in AustenTest's spec-module format. \
       Every representable function definition is emitted in CN's deterministic \
       definition-discovery order, which defines its one-byte selector ID, even when CN \
       cannot generate tests for it"
    in
    Arg.(
      value
      & opt (some string) TestGeneration.default_cfg.export_spec_json
      & info ~docs:s_output [ "export-spec-json" ] ~docv:"FILE" ~doc)


  let inline =
    let doc =
      "Set inlining mode: 'nothing' (no inlining, default), 'nonrec' (non-recursive \
       only), 'semirec' (non-recursive and semi-recursive), 'rec' (recursive only), or \
       'everything' (both)"
    in
    Arg.(
      value
      & opt (enum TestGeneration.Options.inline_mode) TestGeneration.default_cfg.inline
      & info ~docs:s_generation [ "inline" ] ~doc)


  let experimental_struct_asgn_destruction ~docs =
    let doc = "Destructs struct assignments" in
    Arg.(value & flag & info ~docs [ "experimental-struct-asgn-destruction" ] ~doc)


  let experimental_product_arg_destruction ~docs =
    let doc = "Destructs all records and structs arguments" in
    Arg.(value & flag & info ~docs [ "experimental-product-arg-destruction" ] ~doc)


  let experimental_arg_pruning ~docs =
    let doc = "Enable experimental unused argument pruning optimization" in
    Arg.(value & flag & info ~docs [ "experimental-arg-pruning" ] ~doc)


  let experimental_return_pruning ~docs =
    let doc = "Enable experimental unused return value pruning optimization" in
    Arg.(value & flag & info ~docs [ "experimental-return-pruning" ] ~doc)


  let print_size_info ~docs =
    let doc = "(Experimental) Print size info" in
    Arg.(value & flag & info ~docs [ "print-size-info" ] ~doc)


  let print_backtrack_info ~docs =
    let doc = "(Experimental) Print backtracking info" in
    Arg.(value & flag & info ~docs [ "print-backtrack-info" ] ~doc)


  let print_satisfaction_info ~docs =
    let doc = "(Experimental) Print satisfaction info" in
    Arg.(value & flag & info ~docs [ "print-satisfaction-info" ] ~doc)


  let print_discard_info ~docs =
    let doc = "(Experimental) Print discard info" in
    Arg.(value & flag & info ~docs [ "print-discard-info" ] ~doc)


  let print_timing_info ~docs =
    let doc = "(Experimental) Print timing info" in
    Arg.(value & flag & info ~docs [ "print-timing-info" ] ~doc)


  let progress_backtracks ~docs =
    let doc =
      "(Experimental) Show a running total of backtracks in the test progress line"
    in
    Arg.(value & flag & info ~docs [ "progress-backtracks" ] ~doc)


  let max_input_alloc =
    let doc =
      "Maximum memory size for the random input allocator (default: 32m). Supports \
       suffixes: k/K for kilobytes, m/M for megabytes, g/G for gigabytes. Examples: 32m, \
       33554432, 64m"
    in
    Arg.(
      value
      & opt (some size_converter) None
      & info ~docs:s_generation [ "max-input-alloc" ] ~doc)


  let dsl_log_dir =
    let doc =
      "Write generator DSL intermediate representations to separate stage files in this \
       directory"
    in
    Arg.(
      value
      & opt (some string) None
      & info ~docs:s_logging [ "dsl-log-dir" ] ~docv:"DIR" ~doc)


  let max_stack_depth =
    let doc = "Maximum stack depth for generators" in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.max_stack_depth
      & info ~docs:s_generation [ "max-stack-depth" ] ~doc)


  let max_depth_failures =
    let doc = "Maximum number of depth failures allowed before giving up" in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.max_depth_failures
      & info ~docs:s_generation [ "max-depth-failures" ] ~doc)


  let max_generator_size =
    let doc =
      "Maximum size for generated values (also bounds generator recursion depth)"
    in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.max_generator_size
      & info ~docs:s_generation [ "max-generator-size" ] ~doc)


  let sizing_strategy =
    let doc = "Strategy for deciding test case size." in
    Arg.(
      value
      & opt
          (some (enum TestGeneration.Options.sizing_strategy))
          TestGeneration.default_cfg.sizing_strategy
      & info ~docs:s_generation [ "sizing-strategy" ] ~doc)


  let discard_factor =
    let doc = "Multiplier for maximum allowed discards (default: 10)" in
    Arg.(
      value
      & opt int TestGeneration.default_cfg.discard_factor
      & info ~docs:s_generation [ "discard-factor" ] ~doc)


  let disable_extrema_skew =
    let doc = "Disable extreme value (MIN/MAX) skewing in sized generators" in
    Arg.(value & flag & info ~docs:s_generation [ "disable-extrema-skew" ] ~doc)


  let max_array_length =
    let doc = "Maximum length of arrays generated for `each` quantifiers (default: 50)" in
    Arg.(
      value
      & opt int TestGeneration.default_cfg.max_array_length
      & info ~docs:s_generation [ "max-array-length" ] ~doc)
end

(* Compose two engine flag updaters *)
let compose_flags (a : engine_flags Term.t) (b : engine_flags Term.t)
  : engine_flags Term.t
  =
  Term.(const (fun f g cfg -> g (f cfg)) $ a $ b)


(* Generation flags shared by all engines. They configure the shared test
   harness and generator sizing, which also bound Darcy's path selection:
   the path selectors check the per-test size (from --max-generator-size and
   --sizing-strategy) and stack depth, the harness loop applies
   --discard-factor, and sized skewing (--disable-extrema-skew) is used by
   both the random sized domain and Darcy's default SMT skewing mode. *)
let gen_term : engine_flags Term.t =
  let make
        max_stack_depth
        max_depth_failures
        max_generator_size
        sizing_strategy
        discard_factor
        disable_extrema_skew
        max_array_length
        (cfg : TestGeneration.config)
    : TestGeneration.config
    =
    { cfg with
      max_stack_depth;
      max_depth_failures;
      max_generator_size;
      sizing_strategy;
      discard_factor;
      disable_extrema_skew;
      max_array_length
    }
  in
  Term.(
    const make
    $ Flags.max_stack_depth
    $ Flags.max_depth_failures
    $ Flags.max_generator_size
    $ Flags.sizing_strategy
    $ Flags.discard_factor
    $ Flags.disable_extrema_skew
    $ Flags.max_array_length)


(* The common flag chain, shared by all engine subcommands. [engine] decides
   which engine runs; [engine_flags] is the engine's config updater, so
   wrong-engine flags are rejected as unknown options. Until a backend is
   connected, [$-] parses each flag but discards its value, and [run] is
   applied only after every flag has been converted. *)
let mk_term
      ~(engine : TestGeneration.engine Term.t)
      ~(experimental_section : string -> string)
      ~(engine_flags : engine_flags Term.t)
  : unit Term.t
  =
  let open Term in
  let ( $- ) t arg = const (fun x _ -> x) $ t $ arg in
  const run
  $ (engine
     $- Common.Flags.file
     $- Common.Flags.cc
     $- Common.Flags.macros
     $- Common.Flags.permissive
     $- Common.Flags.incl_dirs
     $- Common.Flags.incl_files
     $- Common.Flags.debug_level
     $- Common.Flags.print_level
     $- Common.Flags.csv_times
     $- Common.Flags.stdout_times
     $- Common.Flags.astprints
     $- Common.Flags.no_inherit_loc
     $- Common.Flags.magic_comment_char_dollar
     $- Common.Flags.allow_split_magic_comments
     $- Instrument.Flags.without_ownership_checking
     $- Instrument.Flags.source_locs_mode
     $- Instrument.Flags.correct_missing_ownership_mode
     $- Instrument.Flags.remove_uninterp
     $- Instrument.Flags.experimental_ownership_stack_mode
          ~docs:(experimental_section s_compilation)
     $- Flags.print_steps
     $- Flags.output_dir
     $- Flags.only
     $- Flags.skip
     $- Flags.only_fulminate
     $- Flags.skip_fulminate
     $- Flags.dont_run
     $- Flags.gen_max_unfolds
     $- Term.product Flags.sanitize Flags.no_sanitize
     $- Flags.print_seed
     $- Flags.input_timeout
     $- Flags.null_in_every
     $- Flags.seed
     $- Flags.logging_level
     $- Flags.trace_granularity
     $- Flags.progress_level
     $- Flags.until_timeout
     $- Flags.exit_fast
     $- Flags.coverage ~docs:(experimental_section s_output)
     $- Flags.disable_passes
     $- Flags.trap
     $- Flags.no_replays
     $- Flags.no_replicas
     $- Flags.output_tyche
     $- Flags.export_spec_json
     $- Flags.inline
     $- Flags.experimental_struct_asgn_destruction
          ~docs:(experimental_section s_generation)
     $- Flags.experimental_product_arg_destruction
          ~docs:(experimental_section s_generation)
     $- Flags.experimental_arg_pruning ~docs:(experimental_section s_generation)
     $- Flags.experimental_return_pruning ~docs:(experimental_section s_generation)
     $- Flags.print_size_info ~docs:(experimental_section s_logging)
     $- Flags.print_backtrack_info ~docs:(experimental_section s_logging)
     $- Flags.print_satisfaction_info ~docs:(experimental_section s_logging)
     $- Flags.print_discard_info ~docs:(experimental_section s_logging)
     $- Flags.print_timing_info ~docs:(experimental_section s_logging)
     $- Flags.progress_backtracks ~docs:(experimental_section s_logging)
     $- Instrument.Flags.max_bump_blocks
     $- Instrument.Flags.bump_block_size
     $- Flags.max_input_alloc
     $- Flags.dsl_log_dir
     $- Flags.disable_specialization
     $- compose_flags gen_term engine_flags)


(* Build an engine subcommand. [engine] is named once - both the runtime engine
   constant and its experimental-flag visibility derive from it - and the flags
   are ordered under the shared test-gen sections plus [extra] (e.g. SMT for
   darcy, abstract interpretation for lucas). *)
let mk_cmd
      ~(name : string)
      ~(doc : string)
      ~(extra : string list)
      ~(engine : TestGeneration.engine)
      ~(engine_flags : engine_flags Term.t)
  : unit Cmd.t
  =
  Cmd.v
    (Cmd.info name ~doc ~man:(man_sections ~extra))
    (mk_term
       ~engine:(Term.const engine)
       ~experimental_section:(experimental_docs engine)
       ~engine_flags)
