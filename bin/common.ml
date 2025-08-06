module CF = Cerb_frontend
module CB = Cerb_backend
open CB.Pipeline
open Cn
open Setup

let print_log_file =
  let print_count = ref 0 in
  let print_file filename = function
    | `CORE file -> Pp.print_file (filename ^ ".core") (CF.Pp_core.All.pp_file file)
    | `MUCORE file -> Pp.print_file (filename ^ ".mucore") (Pp_mucore.pp_file file)
  in
  fun (filename, file) ->
    if !Cerb_debug.debug_level > 0 then (
      Cerb_colour.do_colour := false;
      let count = !print_count in
      let file_path =
        Filename.concat
          (Filename.get_temp_dir_name ())
          (string_of_int count ^ "__" ^ filename)
      in
      print_file file_path file;
      print_count := 1 + !print_count;
      Cerb_colour.do_colour := true)


let frontend
      ~cc
      ~macros
      ~incl_dirs
      ~incl_files
      ~astprints
      ~filename
      ~magic_comment_char_dollar
      ~allow_split_magic_comments
      ~save_cpp
      ~disable_linemarkers
  =
  Cerb_global.set_cerb_conf
    ~backend_name:"Cn"
    ~exec:false
    (* execution mode *) Random
    ~concurrency:false
    (* error verbosity *) Basic
    ~defacto:false
    ~permissive:false
    ~agnostic:false
    ~ignore_bitfields:false;
  CF.Ocaml_implementation.set CF.Ocaml_implementation.HafniumImpl.impl;
  CF.Switches.set
    ([ "inner_arg_temps"; "at_magic_comments" ]
     (* TODO (DCM, VIP) figure out how to support liveness checks for read-only
        resources and then switch on "strict_pointer_arith" to elaborate array
        shift to the effectful version. "strict_pointer_relationals" is also
        assumed, but this does not affect elaboration. *)
     @ if magic_comment_char_dollar then [ "magic_comment_char_dollar" ] else []);
  if allow_split_magic_comments then Parse.allow_split_magic_comments := true;
  let return = CF.Exception.except_return in
  let ( let@ ) = CF.Exception.except_bind in
  let@ stdlib = load_core_stdlib () in
  let@ impl = load_core_impl stdlib impl_name in
  let conf =
    Setup.conf cc macros incl_dirs incl_files disable_linemarkers astprints save_cpp
  in
  let cn_init_scope : CF.Cn_desugaring.init_scope =
    { predicates = [ Alloc.Predicate.(str, sym, Some loc) ];
      functions = List.map (fun (str, sym) -> (str, sym, None)) Builtins.fun_names;
      idents = [ Alloc.History.(str, sym, None) ]
    }
  in
  let@ cabs_tunit_opt, ail_prog_opt, prog0 =
    c_frontend_and_elaboration ~cn_init_scope (conf, io) (stdlib, impl) ~filename
  in
  let@ () =
    if conf.typecheck_core then
      let@ _ = CF.Core_typing.typecheck_program prog0 in
      return ()
    else
      return ()
  in
  let cabs_tunit = Option.get cabs_tunit_opt in
  let markers_env, ail_prog = Option.get ail_prog_opt in
  CF.Tags.set_tagDefs prog0.CF.Core.tagDefs;
  let prog1 = CF.Remove_unspecs.rewrite_file prog0 in
  let prog2 = CF.Milicore.core_to_micore__file Locations.update prog1 in
  let prog3 =
    if !Sym.executable_spec_enabled then
      prog2
    else
      CF.Milicore_label_inline.rewrite_file prog2
  in
  let statement_locs = CStatements.search (snd ail_prog) in
  print_log_file ("original", `CORE prog0);
  print_log_file ("without_unspec", `CORE prog1);
  return (cabs_tunit, prog3, (markers_env, ail_prog), statement_locs)


let handle_frontend_error = function
  | CF.Exception.Exception ((_, CF.Errors.CORE_TYPING _) as err) ->
    prerr_string (CF.Pp_errors.to_string err);
    prerr_endline
    @@ Cerb_colour.(ansi_format ~err:true [ Bold; Red ] "error: ")
    ^ "this is likely a bug in the Core elaboration.";
    exit 2
  | CF.Exception.Exception err ->
    prerr_endline (CF.Pp_errors.to_string err);
    exit 2
  | CF.Exception.Result result -> result


let there_can_only_be_one =
  let err_if_not_c_h_file filename =
    let ext = String.equal (Filename.extension filename) in
    if not (ext ".c" || ext ".h") then
      CF.Pp_errors.fatal
        ("file \"" ^ filename ^ "\" has wrong file extension (must be .c or .h)")
  in
  function
  | [] -> assert false
  | [ filename ] ->
    err_if_not_c_h_file filename;
    filename
  | filename :: _ :: _ ->
    prerr_endline
    @@ Cerb_colour.(ansi_format ~err:true [ Bold; Yellow ] "warning: ")
    ^ "only checking "
    ^ filename;
    err_if_not_c_h_file filename;
    filename


let with_well_formedness_check
      (* CLI arguments *)
      ~cc
      ~filename
      ~macros
      ~incl_dirs
      ~incl_files
      ~coq_export_file
      ~coq_mucore
      ~coq_proof_log
      ~coq_check_proof_log
      ~csv_times
      ~astprints
      ~no_inherit_loc
      ~magic_comment_char_dollar
      ~allow_split_magic_comments
      ~save_cpp
      ~disable_linemarkers
      ~(* Callbacks *)
       handle_error
      ~(f :
         cabs_tunit:CF.Cabs.translation_unit ->
         prog5:unit Mucore.file ->
         ail_prog:CF.GenTypes.genTypeCategory CF.AilSyntax.ail_program ->
         statement_locs:Cerb_location.t CStatements.LocMap.t ->
         paused:_ Typing.pause ->
         unit Or_TypeError.t)
  =
  let cabs_tunit, prog, (markers_env, ail_prog), statement_locs =
    handle_frontend_error
      (frontend
         ~cc
         ~macros
         ~incl_dirs
         ~incl_files
         ~astprints
         ~filename
         ~magic_comment_char_dollar
         ~allow_split_magic_comments
         ~save_cpp
         ~disable_linemarkers)
  in
  Cerb_debug.maybe_open_csv_timing_file ();
  Pp.maybe_open_times_channel
    (match csv_times with Some times -> Some (times, "csv") | _ -> None);
  try
    let result =
      let open Or_TypeError in
      let@ prog5 =
        Core_to_mucore.normalise_file
          ~inherit_loc:(not no_inherit_loc)
          (markers_env, snd ail_prog)
          prog
      in
      print_log_file ("mucore", `MUCORE prog5);
      let paused =
        Typing.run_to_pause Context.empty (Check.check_decls_lemmata_fun_specs prog5)
      in
      Result.iter_error handle_error (Typing.pause_to_result paused);
      let@ _ = f ~cabs_tunit ~prog5 ~ail_prog ~statement_locs ~paused in
      Option.iter
        (fun path ->
           let prologue = Pp_mucore_coq.pp_prologue () in
           let mucore =
             if coq_mucore then Pp_mucore_coq.pp_unit_file prog5 else PPrint.empty
           in
           let proof =
             if coq_proof_log then (
               let steps = Prooflog.get_proof_log () in
               Pp_mucore_coq.pp_proof_log steps coq_check_proof_log)
             else
               PPrint.empty
           in
           let doc = PPrint.( ^^ ) prologue (PPrint.( ^^ ) mucore proof) in
           Pp.print_file path doc)
        coq_export_file;
      return ()
    in
    Pp.maybe_close_times_channel ();
    Result.fold ~ok:(fun () -> exit 0) ~error:handle_error result
  with
  | exc ->
    Pp.maybe_close_times_channel ();
    Cerb_debug.maybe_close_csv_timing_file_no_err ();
    Printexc.raise_with_backtrace exc (Printexc.get_raw_backtrace ())


(** Report an error on [stderr] in an appropriate format: JSON if [json] is
    true, or human-readable if not. *)
let report_type_error
      ~(json : bool)
      ?(output_dir : string option)
      ?(fn_name : string option)
      ?(serialize_json : bool = false)
      (error : TypeErrors.t)
  : unit
  =
  if json then
    TypeErrors.report_json ?output_dir ?fn_name ~serialize_json error
  else
    TypeErrors.report_pretty ?output_dir ?fn_name ~serialize_json error


(** Generate an appropriate exit code for the provided error. *)
let exit_code_of_error (error : TypeErrors.t) : int =
  match error.msg with TypeErrors.Unsupported _ -> 2 | _ -> 1


(** In the presence of nonempty errors, generate an appropriate exit code for
    them. *)
let exit_code_of_errors (errors : TypeErrors.t list) : int option =
  (* We arbitrarily choose to make any `Unsupported` message dominate all other
     messages. *)
  let is_unsupported (err : TypeErrors.t) =
    match err.msg with TypeErrors.Unsupported _ -> true | _ -> false
  in
  match errors with
  | [] -> None
  | _ -> Some (if List.exists is_unsupported errors then 2 else 1)


(** Report the provided error, then exit. *)
let handle_type_error
      ~(json : bool)
      ?(output_dir : string option)
      ?(serialize_json : bool = false)
      (error : TypeErrors.t)
  =
  report_type_error ~json ?output_dir ~serialize_json error;
  exit (exit_code_of_error error)


let handle_error_with_user_guidance ~(label : string) (e : exn) : unit =
  let msg = Printexc.to_string e in
  let stack = Printexc.get_backtrace () in
  Printf.eprintf "cn: internal error, uncaught exception:\n    %s\n" msg;
  let lines = String.split_on_char '\n' stack in
  List.iter (fun line -> Printf.eprintf "    %s\n" line) lines;
  Printf.eprintf
    "Issues can be made at https://github.com/rems-project/cerberus/issues.\n";
  Printf.eprintf "Prefix your issue with \"[%s]\". " label;
  Printf.eprintf "Check that there isn't already one for this error.\n";
  exit 1


type subcommand =
  | Verify
  | Instrument
  | Test
  | SeqTest

let tool_name cmd =
  match cmd with
  | Verify -> "CN"
  | Instrument -> "Fulminate"
  | Test -> "Bennet"
  | SeqTest -> "CN-Seq-Test"


open Cmdliner

let mk_dir_if_not_exist_maybe_tmp ~mktemp ?(print_steps = false) (cmd : subcommand) dir =
  let dir =
    match dir with
    | Some dir -> dir
    | None ->
      if mktemp then (
        let dir =
          let tmp_dir = Filename.get_temp_dir_name () in
          let rec create_unique_dir () =
            let dir_name =
              Filename.concat
                tmp_dir
                (String.lowercase_ascii (tool_name cmd)
                 ^ "."
                 ^ Printf.sprintf "%04X" (Random.int 65536))
            in
            if Sys.file_exists dir_name then
              create_unique_dir () (* Retry if it exists *)
            else (
              Sys.mkdir dir_name 0o777;
              (* Create the directory with permissions 777 *)
              dir_name)
          in
          create_unique_dir ()
        in
        print_endline ("Using temporary directory: " ^ dir);
        dir)
      else
        "."
  in
  if not (Sys.file_exists dir) then (
    if print_steps then
      print_endline ("Directory \"" ^ dir ^ "\" does not exist.");
    Sys.mkdir dir 0o777;
    if print_steps then
      print_endline ("Created directory \"" ^ dir ^ "\" with full permissions."));
  dir


(* some of these stolen from backend/driver *)
module Flags = struct
  let file =
    let doc = "Source C file" in
    Arg.(non_empty & pos_all non_dir_file [] & info [] ~docv:"FILE" ~doc)


  let cc =
    let doc = "C compiler to use" in
    Arg.(value & opt string "cc" & info ~env:(Cmd.Env.info "CC") [ "cc" ] ~doc)


  (* copied from cerberus' executable (backend/driver/main.ml) *)
  let macros =
    let macro_pair =
      let parser str =
        match String.index_opt str '=' with
        | None -> Result.Ok (str, None)
        | Some i ->
          let macro = String.sub str 0 i in
          let value = String.sub str (i + 1) (String.length str - i - 1) in
          let is_digit n = 48 <= n && n <= 57 in
          if i = 0 || is_digit (Char.code (String.get macro 0)) then
            Result.Error (`Msg "macro name must be a C identifier")
          else
            Result.Ok (macro, Some value)
      in
      let printer ppf = function
        | m, None -> Format.pp_print_string ppf m
        | m, Some v -> Format.fprintf ppf "%s=%s" m v
      in
      Arg.(conv (parser, printer))
    in
    let doc =
      "Adds  an  implicit  #define  into the predefines buffer which is read before the \
       source file is preprocessed."
    in
    Arg.(
      value
      & opt_all macro_pair []
      & info [ "D"; "define-macro" ] ~docv:"NAME[=VALUE]" ~doc)


  let incl_dirs =
    let doc = "Add the specified directory to the search path for theC preprocessor." in
    Arg.(value & opt_all string [] & info [ "I"; "include-directory" ] ~docv:"DIR" ~doc)


  let incl_files =
    let doc =
      "Adds  an  implicit  #include into the predefines buffer which is read before the \
       source file is preprocessed."
    in
    Arg.(value & opt_all string [] & info [ "include" ] ~doc)


  let debug_level =
    let doc =
      "Set the debug message level for cerberus to $(docv) (should range over [0-3])."
    in
    Arg.(value & opt int 0 & info [ "d"; "debug" ] ~docv:"N" ~doc)


  let print_level =
    let doc =
      "Set the debug message level for the type system to $(docv) (should range over \
       [0-15])."
    in
    Arg.(value & opt int 0 & info [ "p"; "print-level" ] ~docv:"N" ~doc)


  let print_sym_nums =
    let doc = "Print numeric IDs of Cerberus symbols (variable names)." in
    Arg.(value & flag & info [ "n"; "print-sym-nums" ] ~doc)


  let no_timestamps =
    let doc = "Disable timestamps in print-level debug messages" in
    Arg.(value & flag & info [ "no_timestamps" ] ~doc)


  let csv_times =
    let doc = "file in which to output csv timing information" in
    Arg.(value & opt (some string) None & info [ "times" ] ~docv:"FILE" ~doc)


  (* copy-pasting from backend/driver/main.ml *)
  let astprints =
    let doc =
      "Pretty print the intermediate syntax tree for the listed languages (ranging over \
       {cabs, ail, core, types})."
    in
    Arg.(
      value
      & opt
          (list (enum [ ("cabs", Cabs); ("ail", Ail); ("core", Core); ("types", Types) ]))
          []
      & info [ "ast" ] ~docv:"LANG1,..." ~doc)


  let no_use_ity =
    let doc =
      "(this switch should go away) in WellTyped.BaseTyping, do not use\n\
      \  integer type annotations placed by the Core elaboration"
    in
    Arg.(value & flag & info [ "no-use-ity" ] ~doc)


  let no_inherit_loc =
    let doc =
      "debugging: stop mucore terms inheriting location information from parents"
    in
    Arg.(value & flag & info [ "no-inherit-loc" ] ~doc)


  let magic_comment_char_dollar =
    let doc = "Override CN's default magic comment syntax to be \"/*\\$ ... \\$*/\"" in
    Arg.(value & flag & info [ "magic-comment-char-dollar" ] ~doc)


  let allow_split_magic_comments =
    let doc =
      "Alloc function specifications and loop invariants to be split across multiple \
       magic comments"
    in
    Arg.(value & flag & info [ "allow-split-magic-comments" ] ~doc)
end
