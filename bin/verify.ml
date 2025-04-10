module CF = Cerb_frontend
module CB = Cerb_backend
open Cn

let verify
      filename
      macros
      incl_dirs
      incl_files
      loc_pp
      debug_level
      print_level
      print_sym_nums
      no_timestamps
      json
      json_trace
      output_dir
      diag
      lemmata
      coq_export_file
      coq_mucore
      coq_proof_log
      coq_check_proof_log
      only
      skip
      csv_times
      log_times
      solver_logging
      solver_flags
      solver_path
      solver_type
      astprints
      dont_use_vip
      no_use_ity
      fail_fast
      quiet
      no_inherit_loc
      magic_comment_char_dollar
      disable_resource_derived_constraints
      try_hard
  =
  if json then (
    if debug_level > 0 then
      CF.Pp_errors.fatal "debug level must be 0 for json output";
    if print_level > 0 then
      CF.Pp_errors.fatal "print level must be 0 for json output");
  (*flags *)
  Cerb_debug.debug_level := debug_level;
  Pp.loc_pp := loc_pp;
  Pp.print_level := print_level;
  Sym.print_nums := print_sym_nums;
  Pp.print_timestamps := not no_timestamps;
  (match solver_logging with
   | Some d ->
     Solver.Logger.to_file := true;
     Solver.Logger.dir := if String.equal d "" then None else Some d
   | _ -> ());
  Solver.solver_path := solver_path;
  Solver.solver_type := solver_type;
  Solver.solver_flags := solver_flags;
  Solver.try_hard := try_hard;
  Check.skip_and_only := (skip, only);
  IndexTerms.use_vip := not dont_use_vip;
  Check.fail_fast := fail_fast;
  Diagnostics.diag_string := diag;
  WellTyped.use_ity := not no_use_ity;
  Resource.disable_resource_derived_constraints := disable_resource_derived_constraints;
  (* Set the prooflog flag based on --coq-proof-log *)
  Prooflog.set_enabled coq_proof_log;
  let filename = Common.there_can_only_be_one filename in
  Common.with_well_formedness_check (* CLI arguments *)
    ~filename
    ~macros
    ~incl_dirs
    ~incl_files
    ~coq_export_file
    ~coq_mucore
    ~coq_proof_log
    ~coq_check_proof_log
    ~csv_times
    ~log_times
    ~astprints
    ~no_inherit_loc
    ~magic_comment_char_dollar
    ~save_cpp:None
    ~handle_error:(Common.handle_type_error ~json ?output_dir ~serialize_json:json_trace)
    ~f:(fun ~cabs_tunit:_ ~prog5:_ ~ail_prog:_ ~statement_locs:_ ~paused ->
      let check (functions, global_var_constraints, lemmas) =
        let open Typing in
        let@ errors = Check.time_check_c_functions (global_var_constraints, functions) in
        if not quiet then
          List.iter
            (fun (fn, err) ->
               Common.report_type_error
                 ~json
                 ?output_dir
                 ~fn_name:fn
                 ~serialize_json:json_trace
                 err)
            errors;
        Option.fold ~none:() ~some:exit (Common.exit_code_of_errors (List.map snd errors));
        Check.generate_lemmas lemmas lemmata
      in
      Typing.run_from_pause check paused)


open Cmdliner

module Flags = struct
  let loc_pp =
    let doc = "Print pointer values as hexadecimal or as decimal values (hex | dec)" in
    Arg.(
      value
      & opt (enum [ ("hex", Pp.Hex); ("dec", Pp.Dec) ]) !Pp.loc_pp
      & info [ "locs" ] ~docv:"HEX" ~doc)


  let fail_fast =
    let doc = "Abort immediately after encountering a verification error" in
    Arg.(value & flag & info [ "fail-fast" ] ~doc)


  let quiet =
    let doc = "Only report success and failure, rather than rich errors" in
    Arg.(value & flag & info [ "quiet" ] ~doc)


  let diag =
    let doc = "explore branching diagnostics with key string" in
    Arg.(value & opt (some string) None & info [ "diag" ] ~doc)


  let solver_logging =
    let doc = "Log solver queries in SMT2 format to a directory." in
    Arg.(value & opt (some string) None & info [ "solver-logging" ] ~docv:"DIR" ~doc)


  let solver_flags =
    let doc =
      "Ovewrite default solver flags. Note that flags should enable at least incremental \
       checking."
    in
    Arg.(
      value & opt (some (list string)) None & info [ "solver-flags" ] ~docv:"X,Y,Z" ~doc)


  let solver_path =
    let doc = "Path to SMT solver executable" in
    Arg.(value & opt (some file) None & info [ "solver-path" ] ~docv:"FILE" ~doc)


  let solver_type =
    let doc = "Specify the SMT solver interface" in
    Arg.(
      value
      & opt (some (enum [ ("z3", Simple_smt.Z3); ("cvc5", Simple_smt.CVC5) ])) None
      & info [ "solver-type" ] ~docv:"z3|cvc5" ~doc)


  let try_hard =
    let doc = "Try undecidable SMT solving using full set of assumptions" in
    Arg.(value & flag & info [ "try-hard" ] ~doc)


  let only =
    let doc = "only type-check this function (or comma-separated names)" in
    Arg.(value & opt (list string) [] & info [ "only" ] ~doc)


  let skip =
    let doc = "skip type-checking of this function (or comma-separated names)" in
    Arg.(value & opt (list string) [] & info [ "skip" ] ~doc)


  (* TODO remove this when VIP impl complete *)
  let dont_use_vip =
    let doc = "(temporary) disable VIP rules" in
    Arg.(value & flag & info [ "no-vip" ] ~doc)


  let json =
    let doc = "output summary in JSON format" in
    Arg.(value & flag & info [ "json" ] ~doc)


  let json_trace =
    let doc = "output state trace files as JSON, in addition to HTML" in
    Arg.(value & flag & info [ "json-trace" ] ~doc)


  let output_dir =
    let doc = "directory in which to output state files" in
    Arg.(value & opt (some dir) None & info [ "output-dir" ] ~docv:"DIR" ~doc)


  let disable_resource_derived_constraints =
    let doc = "disable resource-derived constraints" in
    Arg.(value & flag & info [ "disable-resource-derived-constraints" ] ~doc)
end

module Lemma_flags = struct
  let lemmata =
    let doc = "lemmata generation mode (target filename)" in
    Arg.(value & opt (some string) None & info [ "lemmata" ] ~docv:"FILE" ~doc)
end

module CoqExport_flags = struct
  let coq_export =
    let doc = "File to export to coq defintions" in
    Arg.(value & opt (some string) None & info [ "coq-export-file" ] ~docv:"FILE" ~doc)
end

module CoqMucore_flags = struct
  let coq_mucore =
    let doc = "include mu-core AST in coq export" in
    Arg.(value & flag & info [ "coq-mucore" ] ~doc)
end

module CoqProofLog_flags = struct
  let coq_proof_log =
    let doc = "include proof log in coq export" in
    Arg.(value & flag & info [ "coq-proof-log" ] ~doc)
end

module CoqCheckProofLog_flags = struct
  let coq_check_proof_log =
    let doc = "Include statements to check proof log in coq exported file" in
    Arg.(value & flag & info [ "coq-check-proof-log" ] ~doc)
end

let verify_t : unit Term.t =
  let open Term in
  const verify
  $ Common.Flags.file
  $ Common.Flags.macros
  $ Common.Flags.incl_dirs
  $ Common.Flags.incl_files
  $ Flags.loc_pp
  $ Common.Flags.debug_level
  $ Common.Flags.print_level
  $ Common.Flags.print_sym_nums
  $ Common.Flags.no_timestamps
  $ Flags.json
  $ Flags.json_trace
  $ Flags.output_dir
  $ Flags.diag
  $ Lemma_flags.lemmata
  $ CoqExport_flags.coq_export
  $ CoqMucore_flags.coq_mucore
  $ CoqProofLog_flags.coq_proof_log
  $ CoqCheckProofLog_flags.coq_check_proof_log
  $ Flags.only
  $ Flags.skip
  $ Common.Flags.csv_times
  $ Common.Flags.log_times
  $ Flags.solver_logging
  $ Flags.solver_flags
  $ Flags.solver_path
  $ Flags.solver_type
  $ Common.Flags.astprints
  $ Flags.dont_use_vip
  $ Common.Flags.no_use_ity
  $ Flags.fail_fast
  $ Flags.quiet
  $ Common.Flags.no_inherit_loc
  $ Common.Flags.magic_comment_char_dollar
  $ Flags.disable_resource_derived_constraints
  $ Flags.try_hard


let cmd =
  let doc =
    "Verifies that functions meet\n\
    \    their CN specifications and the\n\
    \    absence of undefined behavior."
  in
  let info = Cmd.info "verify" ~doc in
  Cmd.v info verify_t
