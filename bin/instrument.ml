module CF = Cerb_frontend
module CB = Cerb_backend
open Cn

let pick_cpp_file_name outdir filename =
  let cpp_name = Filename.remove_extension filename ^ "-preproc.c" in
  match outdir with None -> cpp_name | Some d -> Filename.concat d cpp_name


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
      copy_source_dir
  =
  (*flags *)
  Cerb_debug.debug_level := debug_level;
  Pp.loc_pp := loc_pp;
  Pp.print_level := print_level;
  Sym.print_nums := print_sym_nums;
  Pp.print_timestamps := not no_timestamps;
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
  (* XXX temporary: should we inject in the pre-processed file or original one *)
  let filename = Common.there_can_only_be_one filename in
  let use_preproc = false in
  let exec_spec_file, save =
    if use_preproc then (
      let pp_file = pick_cpp_file_name output_dir filename in
      (pp_file, Some pp_file))
    else
      (filename, None)
  in
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
    ~save_cpp:save
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
                ~copy_source_dir
                exec_spec_file
                ~use_preproc
                ail_prog
                output
                output_dir
                prog5
            with
            | e -> Common.handle_error_with_user_guidance ~label:"CN-Exec" e);
           Or_TypeError.return ())
        ())


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


  let copy_source_dir =
    let doc = "Copy non-CN annotated files into output_dir for CN runtime testing" in
    Arg.(value & flag & info [ "copy-source-dir" ] ~doc)
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
    $ Flags.copy_source_dir
  in
  let doc =
    "Instruments [FILE] with runtime C assertions that check the properties provided in \
     CN specifications.\n"
  in
  let info = Cmd.info "instrument" ~doc in
  Cmd.v info instrument_t
