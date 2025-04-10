module CF = Cerb_frontend
module CB = Cerb_backend
open Cn

let run_tests
      (* Common *)
        filename
      macros
      incl_dirs
      incl_files
      debug_level
      print_level
      csv_times
      log_times
      astprints
      no_inherit_loc
      magic_comment_char_dollar
      (* Executable spec *)
        without_ownership_checking
      (* without_loop_invariants *)
      (* Test Generation *)
        print_steps
      output_dir
      only
      skip
      dont_run
      num_samples
      max_backtracks
      max_unfolds
      max_array_length
      with_static_hack
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
      allowed_depth_failures
      max_generator_size
      sizing_strategy
      random_size_splits
      allowed_size_split_backtracks
      sized_null
      coverage
      disable_passes
      trap
      no_replays
      no_replicas
  =
  (* flags *)
  Cerb_debug.debug_level := debug_level;
  Pp.print_level := print_level;
  Check.skip_and_only := (skip, only);
  Sym.executable_spec_enabled := true;
  let handle_error (e : TypeErrors.t) =
    let report = TypeErrors.pp_message e.msg in
    Pp.error e.loc report.short (Option.to_list report.descr);
    match e.msg with TypeErrors.Unsupported _ -> exit 2 | _ -> exit 1
  in
  let filename = Common.there_can_only_be_one filename in
  Common.with_well_formedness_check (* CLI arguments *)
    ~filename
    ~macros
    ~incl_dirs
    ~incl_files
    ~csv_times
    ~coq_export_file:None
    ~coq_mucore:false
    ~coq_proof_log:false
    ~coq_check_proof_log:false
    ~log_times
    ~astprints
    ~no_inherit_loc
    ~magic_comment_char_dollar (* Callbacks *)
    ~save_cpp:None (* XXX *)
    ~handle_error
    ~f:(fun ~cabs_tunit ~prog5 ~ail_prog ~statement_locs:_ ~paused:_ ->
      let config : TestGeneration.config =
        { print_steps;
          num_samples;
          max_backtracks;
          max_unfolds;
          max_array_length;
          with_static_hack;
          build_tool;
          sanitizers;
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
          allowed_depth_failures;
          max_generator_size;
          sizing_strategy;
          random_size_splits;
          allowed_size_split_backtracks;
          sized_null;
          coverage;
          disable_passes;
          trap;
          no_replays;
          no_replicas
        }
      in
      TestGeneration.set_config config;
      let _, sigma = ail_prog in
      if
        List.is_empty
          (TestGeneration.functions_under_test ~with_warning:true cabs_tunit sigma prog5)
      then (
        print_endline "No testable functions, trivially passing";
        exit 0);
      Cerb_colour.without_colour
        (fun () ->
           let output_dir =
             let dir, mk = output_dir in
             mk dir
           in
           Fulminate.Cn_to_ail.augment_record_map (BaseTypes.Record []);
           (try
              Fulminate.main
                ~without_ownership_checking
                ~without_loop_invariants:true
                ~with_loop_leak_checks:false
                ~with_test_gen:true
                ~copy_source_dir:false
                filename
                ~use_preproc:false (* XXX *)
                ail_prog
                None
                (Some output_dir)
                prog5
            with
            | e -> Common.handle_error_with_user_guidance ~label:"CN-Exec" e);
           (try
              TestGeneration.run
                ~output_dir
                ~filename
                ~without_ownership_checking
                cabs_tunit
                sigma
                prog5
            with
            | e -> Common.handle_error_with_user_guidance ~label:"CN-Test-Gen" e);
           if not dont_run then (
             Cerb_debug.maybe_close_csv_timing_file ();
             Unix.execv (Filename.concat output_dir "run_tests.sh") (Array.of_list [])))
        ();
      Or_TypeError.return ())


open Cmdliner

module Flags = struct
  let print_steps =
    let doc =
      "Print successful stages, such as directory creation, compilation and linking."
    in
    Arg.(value & flag & info [ "print-steps" ] ~doc)


  let output_test_dir =
    let doc = "Place generated tests in the provided directory" in
    Arg.(
      value
      & opt
          Common.dir_and_mk_if_not_exist
          (`May_not_exist ".", fun (`May_not_exist x) -> x)
      & info [ "output-dir" ] ~docv:"DIR" ~doc)


  let only =
    let doc = "Only test this function (or comma-separated names)" in
    Arg.(value & opt (list string) [] & info [ "only" ] ~doc)


  let skip =
    let doc = "Skip testing of this function (or comma-separated names)" in
    Arg.(value & opt (list string) [] & info [ "skip" ] ~doc)


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
    let doc = "Set the maximum number of unfolds for recursive generators" in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.max_unfolds
      & info [ "max-unfolds" ] ~doc)


  let max_array_length =
    let doc = "Set the maximum length for an array generated" in
    Arg.(
      value
      & opt int TestGeneration.default_cfg.max_array_length
      & info [ "max-array-length" ] ~doc)


  let with_static_hack =
    let doc =
      "(HACK) Use an `#include` instead of linking to build testing. Necessary until \
       https://github.com/rems-project/cerberus/issues/784 or equivalent."
    in
    Arg.(value & flag & info [ "with-static-hack" ] ~doc)


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
    let doc = "Maximum stack depth failures before discarding an attempt" in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.allowed_depth_failures
      & info [ "allowed-depth-failures" ] ~doc)


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
    let doc =
      "Scale the likelihood of [NULL] proportionally for a desired size (1/n for size n)"
    in
    Arg.(value & flag & info [ "sized-null" ] ~doc)


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
end

let cmd =
  let open Term in
  let test_t =
    const run_tests
    $ Common.Flags.file
    $ Common.Flags.macros
    $ Common.Flags.incl_dirs
    $ Common.Flags.incl_files
    $ Common.Flags.debug_level
    $ Common.Flags.print_level
    $ Common.Flags.csv_times
    $ Common.Flags.log_times
    $ Common.Flags.astprints
    $ Common.Flags.no_inherit_loc
    $ Common.Flags.magic_comment_char_dollar
    $ Instrument.Flags.without_ownership_checking
    $ Flags.print_steps
    $ Flags.output_test_dir
    $ Flags.only
    $ Flags.skip
    $ Flags.dont_run
    $ Flags.gen_num_samples
    $ Flags.gen_backtrack_attempts
    $ Flags.gen_max_unfolds
    $ Flags.max_array_length
    $ Flags.with_static_hack
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
