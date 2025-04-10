module CF = Cerb_frontend
module CB = Cerb_backend
open Cn

let run_seq_tests
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
        output_dir
      print_steps
      with_static_hack
      num_samples
      backtrack_attempts
      num_resets
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
      Cerb_colour.without_colour
        (fun () ->
           let _, sigma = ail_prog in
           if
             List.is_empty
               (TestGeneration.functions_under_test
                  ~with_warning:true
                  cabs_tunit
                  sigma
                  prog5)
           then (
             print_endline "No testable functions, trivially passing";
             exit 0);
           let _, sigma = ail_prog in
           let output_dir =
             let dir, mk = output_dir in
             mk dir
           in
           Fulminate.Cn_to_ail.augment_record_map (BaseTypes.Record []);
           Fulminate.main
             ~without_ownership_checking
             ~without_loop_invariants:true
             ~with_loop_leak_checks:false
             ~with_test_gen:true
             ~copy_source_dir:false
             filename
             ~use_preproc:false
             ail_prog
             None
             (Some output_dir)
             prog5;
           let config : SeqTests.seq_config =
             { print_steps;
               with_static_hack;
               num_samples;
               max_backtracks = backtrack_attempts;
               num_resets
             }
           in
           SeqTests.set_seq_config config;
           if SeqTests.run_seq ~output_dir ~filename cabs_tunit sigma prog5 <> 0 then
             exit 123)
        ();
      Or_TypeError.return ())


open Cmdliner

module Flags = struct
  let output_test_dir =
    let doc = "Place generated tests in the provided directory" in
    Arg.(
      value
      & opt
          Common.dir_and_mk_if_not_exist
          (`May_not_exist ".", fun (`May_not_exist x) -> x)
      & info [ "output-dir" ] ~docv:"DIR" ~doc)


  let print_steps =
    let doc =
      "Print successful stages, such as directory creation, compilation and linking."
    in
    Arg.(value & flag & info [ "print-steps" ] ~doc)


  let with_static_hack =
    let doc =
      "(HACK) Use an `#include` instead of linking to build testing. Necessary until \
       https://github.com/rems-project/cerberus/issues/784 or equivalent."
    in
    Arg.(value & flag & info [ "with-static-hack" ] ~doc)


  let gen_num_samples =
    let doc = "Set the number of samples to test" in
    Arg.(
      value & opt int SeqTests.default_seq_cfg.num_samples & info [ "num-samples" ] ~doc)


  let gen_backtrack_attempts =
    let doc =
      "Set the maximum attempts to satisfy a constraint before backtracking further, \
       during input generation"
    in
    Arg.(
      value
      & opt int SeqTests.default_seq_cfg.max_backtracks
      & info [ "max-backtrack-attempts" ] ~doc)


  let num_resets =
    let doc = "Number of context resets for sequence testing" in
    Arg.(value & opt int SeqTests.default_seq_cfg.num_resets & info [ "max-resets" ] ~doc)
end

let cmd =
  let open Term in
  let test_t =
    const run_seq_tests
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
    $ Flags.output_test_dir
    $ Flags.print_steps
    $ Flags.with_static_hack
    $ Flags.gen_num_samples
    $ Flags.gen_backtrack_attempts
    $ Flags.num_resets
  in
  let doc =
    "Generates sequences of calls for the API in [FILE].\n\
    \    The tests use randomized inputs or previous calls.\n\
    \    A [.c] file containing the test harnesses will be placed in [output-dir]."
  in
  let info = Cmd.info "seq-test" ~doc in
  Cmd.v info test_t
