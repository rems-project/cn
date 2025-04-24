module CF = Cerb_frontend
module CB = Cerb_backend
open Cn

let well_formed
      filename
      macros
      incl_dirs
      incl_files
      json
      json_trace
      output_dir
      csv_times
      log_times
      astprints
      no_inherit_loc
      magic_comment_char_dollar
  =
  let filename = Common.there_can_only_be_one filename in
  Common.with_well_formedness_check
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
    ~magic_comment_char_dollar
    ~save_cpp:None
    ~handle_error:(Common.handle_type_error ~json ?output_dir ~serialize_json:json_trace)
    ~f:(fun ~cabs_tunit:_ ~prog5:_ ~ail_prog:_ ~statement_locs:_ ~paused:_ ->
      Or_TypeError.return ())


open Cmdliner

let cmd =
  let open Term in
  let wf_t =
    const well_formed
    $ Common.Flags.file
    $ Common.Flags.macros
    $ Common.Flags.incl_dirs
    $ Common.Flags.incl_files
    $ Verify.Flags.json
    $ Verify.Flags.json_trace
    $ Verify.Flags.output_dir
    $ Common.Flags.csv_times
    $ Common.Flags.log_times
    $ Common.Flags.astprints
    $ Common.Flags.no_inherit_loc
    $ Common.Flags.magic_comment_char_dollar
  in
  let doc =
    "Runs CN's well-formedness check\n\
    \    which finds errors such as\n\
    \    ill-typing CN definitions\n\
    \    (predicates, specifications, lemmas)\n\
    \    and ill-formed recursion in datatypes.\n\
    \    It DOES NOT verify C functions,\n\
    \    which `cn verify` does."
  in
  let info = Cmd.info "wf" ~doc in
  Cmd.v info wf_t
