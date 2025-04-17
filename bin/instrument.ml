module CF = Cerb_frontend
module CB = Cerb_backend
open Cn

let run_instrumented_file ~filename ~output ~output_dir ~print_steps =
  let instrumented_filename =
    Option.value ~default:(Fulminate.get_instrumented_filename filename) output
  in
  let output_dir = Option.value ~default:"." output_dir in
  let in_folder ?ext fn =
    Filename.concat
      output_dir
      (match ext with Some ext' -> Filename.remove_extension fn ^ ext' | None -> fn)
  in
  let opam_switch_prefix = Sys.getenv "OPAM_SWITCH_PREFIX" in
  let runtime_prefix = opam_switch_prefix ^ "/lib/cn/runtime" in
  let includes = "-I" ^ runtime_prefix ^ "/include/" in
  if not (Sys.file_exists runtime_prefix) then (
    print_endline
      ("Could not find CN's runtime directory (looked at: '" ^ runtime_prefix ^ "')");
    exit 1);
  if
    Sys.command
      ("cc -c "
       ^ includes
       ^ " -o "
       ^ in_folder ~ext:".o" instrumented_filename
       ^ " "
       ^ in_folder instrumented_filename)
    == 0
  then (
    if print_steps then
      print_endline ("Compiled '" ^ instrumented_filename ^ "'"))
  else (
    print_endline ("Failed to compile '" ^ instrumented_filename ^ "'");
    exit 1);
  if
    Sys.command
      ("cc "
       ^ includes
       ^ " -o "
       ^ in_folder ~ext:".out" instrumented_filename
       ^ " "
       ^ in_folder ~ext:".o" instrumented_filename
       ^ " "
       ^ Filename.concat runtime_prefix "libcn_exec.a")
    == 0
  then (
    if print_steps then
      print_endline "Linked C .o files.")
  else (
    print_endline ("Failed to compile '" ^ instrumented_filename ^ "'");
    exit 1);
  Unix.execv (in_folder ~ext:".out" instrumented_filename) (Array.of_list [])


let generate_executable_specs
      filename
      macros
      incl_dirs
      incl_files
      loc_pp
      debug_level
      print_level
      print_sym_nums
      no_timestamps
      only
      skip
      diag
      csv_times
      log_times
      astprints
      dont_use_vip
      no_use_ity
      fail_fast
      no_inherit_loc
      magic_comment_char_dollar
      (* Executable spec *)
        output
      output_dir
      without_ownership_checking
      without_loop_invariants
      with_loop_leak_checks
      with_test_gen
      run
      print_steps
  =
  (*flags *)
  Cerb_debug.debug_level := debug_level;
  Pp.loc_pp := loc_pp;
  Pp.print_level := print_level;
  Sym.print_nums := print_sym_nums;
  Pp.print_timestamps := not no_timestamps;
  Check.skip_and_only := (skip, only);
  IndexTerms.use_vip := not dont_use_vip;
  Check.fail_fast := fail_fast;
  Diagnostics.diag_string := diag;
  WellTyped.use_ity := not no_use_ity;
  Sym.executable_spec_enabled := true;
  let handle_error (e : TypeErrors.t) =
    let report = TypeErrors.pp_message e.msg in
    Pp.error e.loc report.short (Option.to_list report.descr);
    match e.msg with TypeErrors.Unsupported _ -> exit 2 | _ -> exit 1
  in
  let filename = Common.there_can_only_be_one filename in
  let pp_file = Filename.temp_file "cn_" filename in
  let out_file = Fulminate.get_output_filename output_dir output filename in
  Common.with_well_formedness_check (* CLI arguments *)
    ~filename
    ~macros
    ~incl_dirs
    ~incl_files
    ~coq_export_file:None
    ~coq_mucore:false
    ~coq_proof_log:false
    ~coq_check_proof_log:false
    ~csv_times
    ~log_times
    ~astprints
    ~no_inherit_loc
    ~magic_comment_char_dollar (* Callbacks *)
    ~save_cpp:(Some pp_file)
    ~handle_error
    ~f:(fun ~cabs_tunit:_ ~prog5 ~ail_prog ~statement_locs:_ ~paused:_ ->
      Cerb_colour.without_colour
        (fun () ->
           (try
              Fulminate.main
                ~without_ownership_checking
                ~without_loop_invariants
                ~with_loop_leak_checks
                ~with_test_gen
                pp_file
                out_file
                ail_prog
                prog5
            with
            | e -> Common.handle_error_with_user_guidance ~label:"CN-Exec" e);
           ())
        ();
      Or_TypeError.return
        (if run then (
           if with_test_gen then (
             print_endline
               "Tried running instrumented file (`--run`) with `main` function. (Due to \
                use of `--with-test-gen`)";
             exit 1);
           run_instrumented_file ~filename ~output ~output_dir ~print_steps)))


open Cmdliner

module Flags = struct
  let output_dir =
    let doc =
      "output a version of the translation unit decorated with C runtime\n\
      \  translations of the CN annotations to the provided directory"
    in
    Arg.(value & opt (some dir) None & info [ "output-dir" ] ~docv:"DIR" ~doc)


  let output =
    let doc =
      "output a version of the translation unit decorated with C runtime\n\
      \  translations of the CN annotations."
    in
    Arg.(value & opt (some string) None & info [ "output"; "o" ] ~docv:"FILE" ~doc)


  let without_ownership_checking =
    let doc = "Disable ownership checking within CN runtime testing" in
    Arg.(value & flag & info [ "without-ownership-checking" ] ~doc)


  let without_loop_invariants =
    let doc = "Disable checking of loop invariants within CN runtime testing" in
    Arg.(value & flag & info [ "without-loop-invariants" ] ~doc)


  let with_loop_leak_checks =
    let doc = "Enable leak checking across all runtime loop invariants" in
    Arg.(value & flag & info [ "with-loop-leak-checks" ] ~doc)


  let with_test_gen =
    let doc =
      "Generate CN executable specifications in the correct format for feeding into \n\
      \  the CN test generation tool."
    in
    Arg.(value & flag & info [ "with-test-gen" ] ~doc)


  let run =
    let doc = "Run the instrumented program" in
    Arg.(value & flag & info [ "run" ] ~doc)


  let print_steps =
    let doc =
      "Print successful stages, such as instrumentation, compilation and linking."
    in
    Arg.(value & flag & info [ "print-steps" ] ~doc)


  let only =
    let doc = "Only instrument this function (or comma-separated names)" in
    Arg.(value & opt (list string) [] & info [ "only" ] ~doc)


  let skip =
    let doc = "Skip instrumenting this function (or comma-separated names)" in
    Arg.(value & opt (list string) [] & info [ "skip" ] ~doc)
end

let cmd =
  let open Term in
  let instrument_t =
    const generate_executable_specs
    $ Common.Flags.file
    $ Common.Flags.macros
    $ Common.Flags.incl_dirs
    $ Common.Flags.incl_files
    $ Verify.Flags.loc_pp
    $ Common.Flags.debug_level
    $ Common.Flags.print_level
    $ Common.Flags.print_sym_nums
    $ Common.Flags.no_timestamps
    $ Flags.only
    $ Flags.skip
    $ Verify.Flags.diag
    $ Common.Flags.csv_times
    $ Common.Flags.log_times
    $ Common.Flags.astprints
    $ Verify.Flags.dont_use_vip
    $ Common.Flags.no_use_ity
    $ Verify.Flags.fail_fast
    $ Common.Flags.no_inherit_loc
    $ Common.Flags.magic_comment_char_dollar
    $ Flags.output
    $ Flags.output_dir
    $ Flags.without_ownership_checking
    $ Flags.without_loop_invariants
    $ Flags.with_loop_leak_checks
    $ Flags.with_test_gen
    $ Flags.run
    $ Flags.print_steps
  in
  let doc =
    "Instruments [FILE] with runtime C assertions that check the properties provided in \
     CN specifications.\n"
  in
  let info = Cmd.info "instrument" ~doc in
  Cmd.v info instrument_t
