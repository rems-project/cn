module CF = Cerb_frontend
module CB = Cerb_backend
open Cn

let run_tests
      (* Common *)
        filename
      cc
      macros
      permissive
      incl_dirs
      incl_files
      debug_level
      print_level
      csv_times
      astprints
      no_inherit_loc
      magic_comment_char_dollar
      allow_split_magic_comments
      (* Executable spec *)
        without_ownership_checking
      exec_c_locs_mode
      experimental_ownership_stack_mode
      (* without_loop_invariants *)
      (* Test Generation *)
        print_steps
      output_dir
      only
      skip
      only_fulminate
      skip_fulminate
      dont_run
      num_samples
      max_backtracks
      max_unfolds
      max_array_length
      build_tool
      sanitizers
      print_seed
      input_timeout
      null_in_every
      seed
      logging_level
      trace_granularity
      progress_level
      until_timeout
      exit_fast
      max_stack_depth
      _allowed_depth_failures
      max_generator_size
      sizing_strategy
      random_size_splits
      allowed_size_split_backtracks
      _sized_null
      coverage
      disable_passes
      trap
      no_replays
      no_replicas
      output_tyche
      inline
      experimental_struct_asgn_destruction
      experimental_product_arg_destruction
      experimental_learning
      experimental_arg_pruning
      experimental_return_pruning
      static_absint
      local_iterations
      smt_pruning_before_absinst
      smt_pruning_after_absinst
      smt_pruning_keep_redundant_assertions
      smt_pruning_at_runtime
      runtime_assert_domain
      symbolic
      symbolic_timeout
      use_solver_eval
      smt_solver
      smt_logging
      smt_log_unsat_cores
      print_size_info
      print_backtrack_info
      print_satisfaction_info
      print_discard_info
      print_timing_info
      just_reset_solver
      smt_skewing_mode
      max_bump_blocks
      bump_block_size
      max_input_alloc
      smt_skew_pointer_order
      dsl_log_dir
  =
  (* flags *)
  Cerb_debug.debug_level := debug_level;
  Pp.print_level := print_level;
  Sym.executable_spec_enabled := true;
  let handle_error (e : TypeErrors.t) =
    let report = TypeErrors.pp_message e.msg in
    Pp.error e.loc report.short (Option.to_list report.descr);
    match e.msg with TypeErrors.Unsupported _ -> exit 2 | _ -> exit 1
  in
  let filename = Common.there_can_only_be_one filename in
  let output_dir = Common.mk_dir_if_not_exist_maybe_tmp ~mktemp:true Test output_dir in
  let basefile = Filename.basename filename in
  let pp_file = Filename.temp_file "cn_" basefile in
  let out_file = Fulminate.get_instrumented_filename basefile in
  Common.with_well_formedness_check (* CLI arguments *)
    ~filename
    ~cc
    ~macros:(("__CN_TEST", None) :: ("__CN_INSTRUMENT", None) :: macros)
    ~permissive
    ~incl_dirs
    ~incl_files
    ~csv_times
    ~coq_export_file:None
    ~coq_mucore:false
    ~coq_proof_log:false
    ~coq_check_proof_log:false
    ~astprints
    ~no_inherit_loc
    ~magic_comment_char_dollar
    ~allow_split_magic_comments (* Callbacks *)
    ~save_cpp:(Some pp_file)
    ~disable_linemarkers:true
    ~skip_label_inlining:true
    ~handle_error
    ~f:(fun ~cabs_tunit ~prog5 ~ail_prog ~statement_locs:_ ~paused ->
      let config : TestGeneration.config =
        { skip_and_only = (skip, only);
          cc;
          print_steps;
          num_samples;
          max_backtracks;
          build_tool;
          sanitizers;
          inline;
          experimental_struct_asgn_destruction;
          experimental_product_arg_destruction;
          experimental_learning;
          experimental_arg_pruning;
          experimental_return_pruning;
          static_absint;
          local_iterations;
          smt_pruning_before_absinst;
          smt_pruning_after_absinst;
          smt_pruning_remove_redundant_assertions =
            not smt_pruning_keep_redundant_assertions;
          smt_pruning_at_runtime;
          runtime_assert_domain;
          symbolic;
          symbolic_timeout;
          use_solver_eval;
          smt_solver;
          smt_logging;
          smt_log_unsat_cores;
          max_unfolds;
          max_array_length;
          print_seed;
          input_timeout;
          null_in_every;
          seed;
          logging_level;
          trace_granularity;
          progress_level;
          until_timeout;
          exit_fast;
          max_stack_depth;
          max_generator_size;
          sizing_strategy;
          random_size_splits;
          allowed_size_split_backtracks;
          coverage;
          disable_passes;
          trap;
          no_replays;
          no_replicas;
          output_tyche;
          print_size_info;
          print_backtrack_info;
          print_satisfaction_info;
          print_discard_info;
          print_timing_info;
          just_reset_solver;
          smt_skewing_mode;
          max_bump_blocks;
          bump_block_size;
          max_input_alloc;
          smt_skew_pointer_order;
          dsl_log_dir
        }
      in
      TestGeneration.set_config config;
      let _, sigma = ail_prog in
      if
        List.is_empty
          (TestGeneration.functions_under_test
             ~with_warning:true
             cabs_tunit
             sigma
             prog5
             paused)
      then (
        print_endline "No testable functions, trivially passing";
        exit 0);
      Cerb_colour.do_colour := false;
      (try
         Fulminate.main
           ~without_ownership_checking
           ~without_loop_invariants:true
           ~with_loop_leak_checks:false
           ~without_lemma_checks:false
           ~exec_c_locs_mode
           ~experimental_ownership_stack_mode
           ~experimental_curly_braces:false
           ~experimental_lua_runtime:false
           ~with_testing:true
           ~skip_and_only:(skip_fulminate, only_fulminate)
           ?max_bump_blocks
           ?bump_block_size
           filename
           cc
           pp_file
           out_file
           output_dir
           cabs_tunit
           ail_prog
           prog5
       with
       | e -> Common.handle_error_with_user_guidance ~label:"CN-Exec" e);
      (try
         TestGeneration.run
           ~output_dir
           ~filename
           ~without_ownership_checking
           build_tool
           cabs_tunit
           sigma
           prog5
           paused
       with
       | e -> Common.handle_error_with_user_guidance ~label:"CN-Test-Gen" e);
      if not dont_run then (
        Cerb_debug.maybe_close_csv_timing_file ();
        match build_tool with
        | Bash ->
          let build_script = Filename.concat output_dir "run_tests.sh" in
          Unix.execv build_script (Array.of_list [ build_script ])
        | Make ->
          Unix.chdir output_dir;
          Unix.execvp "make" (Array.of_list [ "make"; "-j" ]));
      Result.ok ())


open Cmdliner

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

module Flags = struct
  let print_steps =
    let doc =
      "Print successful stages, such as directory creation, compilation and linking."
    in
    Arg.(value & flag & info [ "print-steps" ] ~doc)


  let output_dir =
    let doc = "Place generated tests in the provided directory" in
    Arg.(value & opt (some string) None & info [ "output-dir" ] ~docv:"DIR" ~doc)


  let only =
    let doc = "Only test this function (or comma-separated names)" in
    Term.(
      const List.concat $ Arg.(value & opt_all (list string) [] & info [ "only" ] ~doc))


  let skip =
    let doc = "Skip testing of this function (or comma-separated names)" in
    Term.(
      const List.concat $ Arg.(value & opt_all (list string) [] & info [ "skip" ] ~doc))


  let only_fulminate =
    let doc =
      "Only check the pre- and post-conditions of this function (or comma-separated \
       names)"
    in
    Term.(
      const List.concat
      $ Arg.(value & opt_all (list string) [] & info [ "only-fulminate" ] ~doc))


  let skip_fulminate =
    let doc =
      "Skip checking the pre- and post-conditions of this function (or comma-separated \
       names)"
    in
    Term.(
      const List.concat
      $ Arg.(value & opt_all (list string) [] & info [ "skip-fulminate" ] ~doc))


  let dont_run =
    let doc = "Do not run tests, only generate them" in
    Arg.(value & flag & info [ "no-run" ] ~doc)


  let gen_num_samples =
    let doc = "Set the number of samples to test" in
    Arg.(
      value & opt int TestGeneration.default_cfg.num_samples & info [ "num-samples" ] ~doc)


  let gen_backtrack_attempts =
    let doc =
      "Set the maximum attempts to satisfy a constraint before backtracking further, \
       during input generation"
    in
    Arg.(
      value
      & opt int TestGeneration.default_cfg.max_backtracks
      & info [ "max-backtrack-attempts" ] ~doc)


  let gen_max_unfolds =
    let doc =
      "Maximum number of times to inline function calls in symbolic mode (default: 10)"
    in
    Arg.(value & opt (some int) None & info [ "max-unfolds" ] ~doc)


  let max_array_length =
    let doc = "Maximum array length for symbolic mode" in
    Arg.(
      value
      & opt int TestGeneration.default_cfg.max_array_length
      & info [ "max-array-length" ] ~doc)


  let build_tool =
    let doc = "Set which build tool to use." in
    Arg.(
      value
      & opt (enum TestGeneration.Options.build_tool) TestGeneration.default_cfg.build_tool
      & info [ "build-tool" ] ~doc)


  let sanitize =
    let doc = "Forwarded to the '-fsanitize' argument of the C compiler" in
    Arg.(
      value
      & opt (some string) (fst TestGeneration.default_cfg.sanitizers)
      & info [ "sanitize" ] ~doc)


  let no_sanitize =
    let doc = "Forwarded to the '-fno-sanitize' argument of the C compiler" in
    Arg.(
      value
      & opt (some string) (snd TestGeneration.default_cfg.sanitizers)
      & info [ "no-sanitize" ] ~doc)


  let print_seed =
    let doc = "Print seed used by PRNG." in
    Arg.(value & flag & info [ "print-seed" ] ~doc)


  let input_timeout =
    let doc = "Timeout for discarding a generation attempt (ms)" in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.input_timeout
      & info [ "input-timeout" ] ~doc)


  let null_in_every =
    let doc = "Set the likelihood of NULL being generated as 1 in every <n>" in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.null_in_every
      & info [ "null-in-every" ] ~doc)


  let seed =
    let doc = "Set the seed for random testing" in
    Arg.(value & opt (some string) TestGeneration.default_cfg.seed & info [ "seed" ] ~doc)


  let logging_level =
    let doc = "Set the logging level for failing inputs from tests" in
    Arg.(
      value
      & opt
          (some (enum TestGeneration.Options.logging_level))
          TestGeneration.default_cfg.logging_level
      & info [ "logging-level" ] ~doc)


  let trace_granularity =
    let doc = "Set the trace granularity for failing inputs from tests" in
    Arg.(
      value
      & opt
          (some (enum TestGeneration.Options.trace_granularity))
          TestGeneration.default_cfg.trace_granularity
      & info [ "trace-granularity" ] ~doc)


  let progress_level =
    let doc = "Set the frequency of progress updates." in
    Arg.(
      value
      & opt
          (some (enum TestGeneration.Options.progress_level))
          TestGeneration.default_cfg.progress_level
      & info [ "progress-level" ] ~doc)


  let until_timeout =
    let doc =
      "Keep rerunning tests until the given timeout (in seconds) has been reached"
    in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.until_timeout
      & info [ "until-timeout" ] ~doc)


  let exit_fast =
    let doc = "Stop testing upon finding the first failure" in
    Arg.(value & flag & info [ "exit-fast" ] ~doc)


  let max_stack_depth =
    let doc = "Maximum stack depth for generators" in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.max_stack_depth
      & info [ "max-stack-depth" ] ~doc)


  let allowed_depth_failures =
    let doc = "Does nothing." in
    let deprecated = "Will be removed after July 31." in
    Arg.(value & opt (some int) None & info [ "allowed-depth-failures" ] ~deprecated ~doc)


  let max_generator_size =
    let doc = "Maximum size for generated values" in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.max_generator_size
      & info [ "max-generator-size" ] ~doc)


  let sizing_strategy =
    let doc = "Strategy for deciding test case size." in
    Arg.(
      value
      & opt
          (some (enum TestGeneration.Options.sizing_strategy))
          TestGeneration.default_cfg.sizing_strategy
      & info [ "sizing-strategy" ] ~doc)


  let random_size_splits =
    let doc = "Randomly split sizes between recursive generator calls" in
    Arg.(value & flag & info [ "random-size-splits" ] ~doc)


  let allowed_size_split_backtracks =
    let doc =
      "Set the maximum attempts to split up a generator's size (between recursive calls) \
       before backtracking further, during input generation"
    in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.allowed_size_split_backtracks
      & info [ "allowed-size-split-backtracks" ] ~doc)


  let sized_null =
    let doc = "Does nothing." in
    let deprecated = "Will be removed after July 31." in
    Arg.(value & flag & info [ "sized-null" ] ~deprecated ~doc)


  let coverage =
    let doc = "(Experimental) Record coverage of tests via [lcov]" in
    Arg.(value & flag & info [ "coverage" ] ~doc)


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
      & info [ "disable" ] ~doc)


  let trap =
    let doc = "Raise SIGTRAP on test failure" in
    Arg.(value & flag & info [ "trap" ] ~doc)


  let no_replays =
    let doc = "Disable replaying errors for error messages" in
    Arg.(value & flag & info [ "no-replays" ] ~doc)


  let no_replicas =
    let doc = "Disable synthesizing C code to replicate bugs" in
    Arg.(value & flag & info [ "no-replicas" ] ~doc)


  let output_tyche =
    let doc = "Enable output in Tyche format" in
    Arg.(
      value
      & opt (some string) TestGeneration.default_cfg.output_tyche
      & info [ "output-tyche" ] ~doc)


  let inline =
    let doc =
      "Set inlining mode: 'nothing' (no inlining, default), 'nonrec' (non-recursive \
       only), 'rec' (recursive only), or 'everything' (both)"
    in
    Arg.(
      value
      & opt (enum TestGeneration.Options.inline_mode) TestGeneration.default_cfg.inline
      & info [ "inline" ] ~doc)


  let experimental_struct_asgn_destruction =
    let doc = "Destructs struct assignments" in
    Arg.(value & flag & info [ "experimental-struct-asgn-destruction" ] ~doc)


  let experimental_product_arg_destruction =
    let doc = "Destructs all records and structs arguments" in
    Arg.(value & flag & info [ "experimental-product-arg-destruction" ] ~doc)


  let experimental_learning =
    let doc = "Use experimental domain learning" in
    Arg.(value & flag & info [ "experimental-learning" ] ~doc)


  let experimental_arg_pruning =
    let doc = "Enable experimental unused argument pruning optimization" in
    Arg.(value & flag & info [ "experimental-arg-pruning" ] ~doc)


  let experimental_return_pruning =
    let doc = "Enable experimental unused return value pruning optimization" in
    Arg.(value & flag & info [ "experimental-return-pruning" ] ~doc)


  let smt_pruning_before_absinst =
    let doc =
      "(Experimental) Use SMT solver to prune unsatisfiable branches before abstract \
       interpretation"
    in
    Arg.(
      value
      & opt (enum [ ("none", `None); ("fast", `Fast); ("slow", `Slow) ]) `None
      & info [ "smt-pruning-before-absint" ] ~doc)


  let smt_pruning_after_absinst =
    let doc =
      "(Experimental) Use SMT solver to prune unsatisfiable branches after abstract \
       interpretation"
    in
    Arg.(
      value
      & opt (enum [ ("none", `None); ("fast", `Fast); ("slow", `Slow) ]) `None
      & info [ "smt-pruning-after-absint" ] ~doc)


  let smt_pruning_keep_redundant_assertions =
    let doc =
      "(Experimental) Keep assertions even if provably redundant during SMT pruning"
    in
    Arg.(value & flag & info [ "smt-pruning-keep-redundant-assertions" ] ~doc)


  let smt_pruning_at_runtime =
    let doc = "(Experimental) Use SMT solver to prune branches at runtime" in
    Arg.(value & flag & info [ "smt-pruning-at-runtime" ] ~doc)


  let runtime_assert_domain =
    let doc = "Enable assert_domain checks at runtime (disabled by default)" in
    Arg.(value & flag & info [ "runtime-assert-domain" ] ~doc)


  let static_absint =
    let doc =
      "(Experimental) Use static abstract interpretation with specified domain (or a \
       comma-separated list). (e.g., 'interval', 'wrapped_interval')"
    in
    Arg.(
      value
      & opt
          (list
             (enum [ ("interval", "interval"); ("wrapped_interval", "wrapped_interval") ]))
          []
      & info [ "static-absint" ] ~docv:"DOMAIN" ~doc)


  let local_iterations =
    let doc = "Maximum iterations for local abstract interpretation refinement" in
    Arg.(
      value
      & opt int TestGeneration.default_cfg.local_iterations
      & info [ "local-iterations" ] ~doc)


  let print_size_info =
    let doc = "(Experimental) Print size info" in
    Arg.(value & flag & info [ "print-size-info" ] ~doc)


  let print_backtrack_info =
    let doc = "(Experimental) Print backtracking info" in
    Arg.(value & flag & info [ "print-backtrack-info" ] ~doc)


  let print_satisfaction_info =
    let doc = "(Experimental) Print satisfaction info" in
    Arg.(value & flag & info [ "print-satisfaction-info" ] ~doc)


  let print_discard_info =
    let doc = "(Experimental) Print discard info" in
    Arg.(value & flag & info [ "print-discard-info" ] ~doc)


  let print_timing_info =
    let doc = "(Experimental) Print timing info" in
    Arg.(value & flag & info [ "print-timing-info" ] ~doc)


  let just_reset_solver =
    let doc =
      "Just reset the SMT solver instead of closing and creating a new one. WARNING: A \
       bunch of stuff breaks."
    in
    Arg.(value & flag & info [ "just-reset-solver" ] ~doc)


  let smt_skewing_mode =
    let doc =
      "Set SMT skewing mode for symbolic test generation. Options: uniform (uniform \
       random values), sized (default, size-based values), none (no skewing)"
    in
    Arg.(
      value
      & opt
          (enum TestGeneration.Options.smt_skewing_mode)
          TestGeneration.default_cfg.smt_skewing_mode
      & info [ "smt-skewing" ] ~docv:"MODE" ~doc)


  let symbolic =
    let doc =
      "(Experimental) Use symbolic execution for test generation instead of concrete \
       value generation."
    in
    Arg.(value & flag & info [ "symbolic" ] ~doc)


  let symbolic_timeout =
    let doc = "Set timeout for SMT solver in symbolic mode (seconds)" in
    Arg.(value & opt (some int) None & info [ "symbolic-timeout" ] ~doc)


  let smt_solver =
    let doc =
      "Choose SMT solver backend for symbolic test generation (z3, cvc5 is unsupported)."
    in
    Arg.(
      value
      & opt (enum TestGeneration.Options.smt_solver) TestGeneration.default_cfg.smt_solver
      & info [ "solver-type" ] ~docv:"SOLVER" ~doc)


  let use_solver_eval =
    let doc = "(Experimental) Use solver-based evaluation" in
    Arg.(value & flag & info [ "use-solver-eval" ] ~doc)


  let smt_logging =
    let doc = "Log SMT solver communication to specified file" in
    Arg.(value & opt (some string) None & info [ "smt-logging" ] ~doc ~docv:"FILE")


  let smt_log_unsat_cores =
    let doc = "Log unsat cores to specified file when constraints are unsatisfiable" in
    Arg.(
      value & opt (some string) None & info [ "smt-log-unsat-cores" ] ~doc ~docv:"FILE")


  let max_input_alloc =
    let doc =
      "Maximum memory size for the random input allocator (default: 32m). Supports \
       suffixes: k/K for kilobytes, m/M for megabytes, g/G for gigabytes. Examples: 32m, \
       33554432, 64m"
    in
    Arg.(value & opt (some size_converter) None & info [ "max-input-alloc" ] ~doc)


  let smt_skew_pointer_order =
    let doc = "Enable pointer ordering skewing in SMT solver" in
    Arg.(value & flag & info [ "smt-skew-pointer-order" ] ~doc)


  let dsl_log_dir =
    let doc =
      "Write generator DSL intermediate representations to separate stage files in this \
       directory"
    in
    Arg.(value & opt (some string) None & info [ "dsl-log-dir" ] ~docv:"DIR" ~doc)
end

let cmd =
  let open Term in
  let test_t =
    const run_tests
    $ Common.Flags.file
    $ Common.Flags.cc
    $ Common.Flags.macros
    $ Common.Flags.permissive
    $ Common.Flags.incl_dirs
    $ Common.Flags.incl_files
    $ Common.Flags.debug_level
    $ Common.Flags.print_level
    $ Common.Flags.csv_times
    $ Common.Flags.astprints
    $ Common.Flags.no_inherit_loc
    $ Common.Flags.magic_comment_char_dollar
    $ Common.Flags.allow_split_magic_comments
    $ Instrument.Flags.without_ownership_checking
    $ Instrument.Flags.exec_c_locs_mode
    $ Instrument.Flags.experimental_ownership_stack_mode
    $ Flags.print_steps
    $ Flags.output_dir
    $ Flags.only
    $ Flags.skip
    $ Flags.only_fulminate
    $ Flags.skip_fulminate
    $ Flags.dont_run
    $ Flags.gen_num_samples
    $ Flags.gen_backtrack_attempts
    $ Flags.gen_max_unfolds
    $ Flags.max_array_length
    $ Flags.build_tool
    $ Term.product Flags.sanitize Flags.no_sanitize
    $ Flags.print_seed
    $ Flags.input_timeout
    $ Flags.null_in_every
    $ Flags.seed
    $ Flags.logging_level
    $ Flags.trace_granularity
    $ Flags.progress_level
    $ Flags.until_timeout
    $ Flags.exit_fast
    $ Flags.max_stack_depth
    $ Flags.allowed_depth_failures
    $ Flags.max_generator_size
    $ Flags.sizing_strategy
    $ Flags.random_size_splits
    $ Flags.allowed_size_split_backtracks
    $ Flags.sized_null
    $ Flags.coverage
    $ Flags.disable_passes
    $ Flags.trap
    $ Flags.no_replays
    $ Flags.no_replicas
    $ Flags.output_tyche
    $ Flags.inline
    $ Flags.experimental_struct_asgn_destruction
    $ Flags.experimental_product_arg_destruction
    $ Flags.experimental_learning
    $ Flags.experimental_arg_pruning
    $ Flags.experimental_return_pruning
    $ Flags.static_absint
    $ Flags.local_iterations
    $ Flags.smt_pruning_before_absinst
    $ Flags.smt_pruning_after_absinst
    $ Flags.smt_pruning_keep_redundant_assertions
    $ Flags.smt_pruning_at_runtime
    $ Flags.runtime_assert_domain
    $ Flags.symbolic
    $ Flags.symbolic_timeout
    $ Flags.use_solver_eval
    $ Flags.smt_solver
    $ Flags.smt_logging
    $ Flags.smt_log_unsat_cores
    $ Flags.print_size_info
    $ Flags.print_backtrack_info
    $ Flags.print_satisfaction_info
    $ Flags.print_discard_info
    $ Flags.print_timing_info
    $ Flags.just_reset_solver
    $ Flags.smt_skewing_mode
    $ Instrument.Flags.max_bump_blocks
    $ Instrument.Flags.bump_block_size
    $ Flags.max_input_alloc
    $ Flags.smt_skew_pointer_order
    $ Flags.dsl_log_dir
  in
  let doc =
    "Generates tests for all functions in [FILE] with CN specifications.\n\
    \    The tests use randomized inputs, which are guaranteed to satisfy the CN \
     precondition.\n\
    \    A script [run_tests.sh] for building and running the tests will be placed in \
     [output-dir]."
  in
  let info = Cmd.info "test" ~doc in
  Cmd.v info test_t
