(* `cn test export`: write the JSON specification module for every function
   definition in [FILE], without generating, building, or running tests. *)

open Cn
open Cmdliner

let run
      filename
      cc
      macros
      permissive
      incl_dirs
      incl_files
      debug_level
      print_level
      csv_times
      stdout_times
      astprints
      no_inherit_loc
      magic_comment_char_dollar
      allow_split_magic_comments
      output
  =
  Cerb_debug.debug_level := debug_level;
  Pp.print_level := print_level;
  Sym.executable_spec_enabled := true;
  let filename = Common.there_can_only_be_one filename in
  Common.with_well_formedness_check (* CLI arguments *)
    ~filename
    ~cc
    ~macros:(Shared.test_macros @ macros)
    ~permissive
    ~incl_dirs
    ~incl_files
    ~csv_times
    ~stdout_times
    ~coq_export_file:None
    ~coq_mucore:false
    ~coq_proof_log:false
    ~coq_check_proof_log:false
    ~astprints
    ~no_inherit_loc
    ~magic_comment_char_dollar
    ~allow_split_magic_comments (* Callbacks *)
    ~save_cpp:None
    ~disable_linemarkers:false
    ~handle_error:Common.print_type_error_and_exit
    ~f:(fun ~cerb_conf:_ ~prog5 ~ail_prog ~statement_locs:_ ~paused ->
      let _, sigma = ail_prog in
      (* Function metadata consults the test-generation config only to choose a
         generator kind, which the export does not use. *)
      TestGeneration.set_config TestGeneration.default_cfg;
      (try TestGeneration.export_specs ~path:output ~filename sigma prog5 paused with
       | e -> Common.handle_error_with_user_guidance ~label:"CN-Export" e);
      Result.ok ())


let output =
  let doc = "Write the exported specification JSON to $(docv)." in
  Arg.(
    required
    & opt (some string) None
    & info ~docs:Shared.s_output [ "o"; "output" ] ~docv:"PATH" ~doc)


let term : unit Term.t =
  let open Term in
  const run
  $ Common.Flags.file
  $ Common.Flags.cc
  $ Common.Flags.macros
  $ Common.Flags.permissive
  $ Common.Flags.incl_dirs
  $ Common.Flags.incl_files
  $ Common.Flags.debug_level
  $ Common.Flags.print_level
  $ Common.Flags.csv_times
  $ Common.Flags.stdout_times
  $ Common.Flags.astprints
  $ Common.Flags.no_inherit_loc
  $ Common.Flags.magic_comment_char_dollar
  $ Common.Flags.allow_split_magic_comments
  $ output


let doc =
  "Exports the CN specifications of every function definition in [FILE] as JSON, without \
   generating or running tests."


let cmd = Cmd.v (Cmd.info "export" ~doc ~man:[ `S Shared.s_output; `S Common.s_cn ]) term
