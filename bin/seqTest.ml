module CF = Cerb_frontend
module CB = Cerb_backend
open Cn

let run_seq_tests
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
      (* Test Generation *)
        output_dir
      print_steps
      num_calls
      backtrack_attempts
      num_tests
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
  let output_dir =
    Common.mk_dir_if_not_exist_maybe_tmp ~mktemp:false SeqTest output_dir
  in
  let basefile = Filename.basename filename in
  let pp_file = Filename.temp_file "cn_" basefile in
  let out_file = Fulminate.get_instrumented_filename basefile in
  Common.with_well_formedness_check (* CLI arguments *)
    ~filename
    ~cc
    ~macros:(("__CN_SEQ_TEST", None) :: ("__CN_INSTRUMENT", None) :: macros)
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
    ~f:(fun ~cabs_tunit ~prog5 ~ail_prog ~statement_locs:_ ~paused:_ ->
      Cerb_colour.without_colour
        (fun () ->
           let _, sigma = ail_prog in
           Fulminate.Cn_to_ail.augment_record_map (BaseTypes.Record []);
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
             ~skip_and_only:([], [])
             filename
             cc
             pp_file
             out_file
             output_dir
             cabs_tunit
             ail_prog
             prog5;
           let config : SeqTests.seq_config =
             { cc;
               print_steps;
               num_calls;
               max_backtracks = backtrack_attempts;
               num_tests
             }
           in
           SeqTests.set_seq_config config;
           if SeqTests.run_seq ~output_dir ~filename cabs_tunit sigma prog5 <> 0 then
             exit 123)
        ();
      Or_TypeError.return ())


open Cmdliner

module Flags = struct
  let output_dir =
    let doc = "Place generated tests in the provided directory" in
    Arg.(value & opt (some string) None & info [ "output-dir" ] ~docv:"DIR" ~doc)


  let print_steps =
    let doc =
      "Print successful stages, such as directory creation, compilation and linking."
    in
    Arg.(value & flag & info [ "print-steps" ] ~doc)


  let gen_num_calls =
    let doc = "Maximum number of calls per test" in
    Arg.(
      value & opt int SeqTests.default_seq_cfg.num_calls & info [ "max-num-calls" ] ~doc)


  let gen_backtrack_attempts =
    let doc =
      "Set the maximum attempts to satisfy a constraint before backtracking further, \
       during input generation"
    in
    Arg.(
      value
      & opt int SeqTests.default_seq_cfg.max_backtracks
      & info [ "max-backtrack-attempts" ] ~doc)


  let num_tests =
    let doc = "Number of tests to generate" in
    Arg.(value & opt int SeqTests.default_seq_cfg.num_tests & info [ "num-tests" ] ~doc)
end

let cmd =
  let open Term in
  let test_t =
    const run_seq_tests
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
    $ Flags.output_dir
    $ Flags.print_steps
    $ Flags.gen_num_calls
    $ Flags.gen_backtrack_attempts
    $ Flags.num_tests
  in
  let doc =
    "Generates sequences of calls for the API in [FILE].\n\
    \    The tests use randomized inputs or previous calls.\n\
    \    A [.c] file containing the test harnesses will be placed in [output-dir]."
  in
  let info = Cmd.info "seq-test" ~doc in
  Cmd.v info test_t
