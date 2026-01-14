module CF = Cerb_frontend
module CB = Cerb_backend
open Cn

let build_lua ~lua_root_dir ~print_steps =
  let src_dir = lua_root_dir ^ "/src" in
  let cn_dir = lua_root_dir ^ "/cn" in

  let src_cmd = Printf.sprintf "make -C %s liblua.a" src_dir in
  if print_steps then Printf.printf "Building Lua source: %s\n%!" src_cmd;
  if Sys.command src_cmd <> 0 then (
    Printf.eprintf "Failed to build Lua source in %s\n%!" src_dir;
    exit 1
  );

  let cn_cmd = Printf.sprintf "make -C %s lua_wrappers.a" cn_dir in
  if print_steps then Printf.printf "Building Lua cn wrappers: %s\n%!" cn_cmd;
  if Sys.command cn_cmd <> 0 then (
    Printf.eprintf "Failed to build Lua cn wrappers in %s\n%!" cn_dir;
    exit 1
  )

let run_instrumented_file ~filename ~cc ~no_debug_info ~output ~output_dir ~print_steps ~experimental_lua_runtime ~is_handwritten =
  let instrumented_filename = if is_handwritten then filename else
    Option.value ~default:(Fulminate.get_instrumented_filename filename) output
  in
  let in_folder ?ext fn =
    Filename.concat
      output_dir
      (match ext with Some ext' -> Filename.remove_extension fn ^ ext' | None -> fn)
  in
  let opam_switch_prefix = Sys.getenv "OPAM_SWITCH_PREFIX" in
  let runtime_prefix = opam_switch_prefix ^ "/lib/cn/runtime" in

  let lua_root_dir, lua_inc_flags, lua_link_flags =
    if not experimental_lua_runtime then ("", "", "") else

    let root_dir = Sys.getcwd() ^ "/runtime/lua" in
    let src_dir = root_dir ^ "/src" in
    let cn_dir = root_dir ^ "/cn" in
    
    let combined_includes = " -I" ^ src_dir ^ " -I" ^ cn_dir in

    let src_link = Printf.sprintf " %s/liblua.a" src_dir in
    let cn_wrapper_link = Printf.sprintf " %s/lua_wrappers.a" cn_dir in
    let combined_links = Printf.sprintf "%s %s -ldl -lm" src_link cn_wrapper_link in

    (root_dir, combined_includes, combined_links)
  in

  if experimental_lua_runtime then build_lua ~lua_root_dir ~print_steps;

  let includes = "-I" ^ runtime_prefix ^ "/include/" ^ lua_inc_flags in
  
  if not (Sys.file_exists runtime_prefix) then (
    print_endline
      ("Could not find CN's runtime directory (looked at: '" ^ runtime_prefix ^ "')");
    exit 1);
  let flags =
    let debug_info_flag = if no_debug_info then "" else " -g " in
    let cflags = Option.value ~default:"" (Sys.getenv_opt "CFLAGS") in
    let cppflags = Option.value ~default:"" (Sys.getenv_opt "CPPFLAGS") in
    String.concat " " [ debug_info_flag; cflags; cppflags ]
  in
  if
    Sys.command
      (cc
       ^ " -c "
       ^ flags
       ^ " "
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
      (cc
       ^ " "
       ^ flags
       ^ " "
       ^ includes
       ^ " -o "
       ^ in_folder ~ext:".out" instrumented_filename
       ^ " "
       ^ in_folder ~ext:".o" instrumented_filename
       ^ " "
       ^ Filename.concat runtime_prefix "libcn_exec.a"
       ^ " "
       ^ lua_link_flags)
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
      cc
      macros
      permissive
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
      astprints
      dont_use_vip
      fail_fast
      no_inherit_loc
      magic_comment_char_dollar
      allow_split_magic_comments
      (* Executable spec *)
        output
      output_dir
      without_ownership_checking
      without_loop_invariants
      with_loop_leak_checks
      without_lemma_checks
      with_testing
      run
      no_debug_info
      exec_c_locs_mode
      experimental_ownership_stack_mode
      experimental_unions
      experimental_curly_braces
      experimental_lua_runtime
      mktemp
      print_steps
      max_bump_blocks
      bump_block_size
  =
  (* flags *)
  Cerb_debug.debug_level := debug_level;
  Pp.loc_pp := loc_pp;
  Pp.print_level := print_level;
  Sym.print_nums := print_sym_nums;
  Pp.print_timestamps := not no_timestamps;
  IndexTerms.use_vip := not dont_use_vip;
  Check.fail_fast := fail_fast;
  Diagnostics.diag_string := diag;
  Sym.executable_spec_enabled := true;
  Sym.experimental_unions := experimental_unions;
  let handle_error (e : TypeErrors.t) =
    let report = TypeErrors.pp_message e.msg in
    Pp.error e.loc report.short (Option.to_list report.descr);
    match e.msg with TypeErrors.Unsupported _ -> exit 2 | _ -> exit 1
  in
  let filename = Common.there_can_only_be_one filename in
  let basefile = Filename.basename filename in
  let pp_file = Filename.temp_file "cn_" basefile in
  let out_file =
    Option.value ~default:(Fulminate.get_instrumented_filename basefile) output
  in
  Common.with_well_formedness_check (* CLI arguments *)
    ~filename
    ~cc
    ~macros:(("__CN_INSTRUMENT", None) :: macros)
    ~permissive
    ~incl_dirs
    ~incl_files
    ~coq_export_file:None
    ~coq_mucore:false
    ~coq_proof_log:false
    ~coq_check_proof_log:false
    ~csv_times
    ~astprints
    ~no_inherit_loc
    ~magic_comment_char_dollar
    ~allow_split_magic_comments (* Callbacks *)
    ~save_cpp:(Some pp_file)
    ~disable_linemarkers:exec_c_locs_mode
      (* If output locations requested, disable linemarkers in preproc step *)
    ~skip_label_inlining:true
    ~handle_error
    ~f:(fun ~cabs_tunit ~prog5 ~ail_prog ~statement_locs:_ ~paused:_ ->
      if run && Option.is_none prog5.main then (
        print_endline "Tried running instrumented file (`--run`) without `main` function.";
        exit 1);
      if mktemp && Option.is_some output_dir then (
        print_endline "Cannot use '--tmp' and '--output-dir' together.";
        exit 1);
      let output_dir =
        Common.mk_dir_if_not_exist_maybe_tmp ~mktemp Instrument output_dir
      in
      Cerb_colour.without_colour
        (fun () ->
           (try
              Fulminate.main
                ~without_ownership_checking
                ~without_loop_invariants
                ~with_loop_leak_checks
                ~without_lemma_checks
                ~exec_c_locs_mode
                ~experimental_ownership_stack_mode
                ~experimental_curly_braces
                ~experimental_lua_runtime
                ~with_testing
                ~skip_and_only:(skip, only)
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
           ())
        ();
      Or_TypeError.return
        (if run then
           let is_handwritten = false in

           run_instrumented_file
             ~filename
             ~cc
             ~no_debug_info
             ~output
             ~output_dir
             ~print_steps
             ~experimental_lua_runtime
             ~is_handwritten))

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


  let without_lemma_checks =
    let doc = "Disable runtime checking of lemmas" in
    Arg.(value & flag & info [ "without-lemma-checks" ] ~doc)


  let with_test_gen =
    let doc =
      "Generate CN executable specifications in the correct format for feeding into \n\
      \  the CN test generation tool."
    in
    let deprecated = "Use `--with-testing` instead." in
    Arg.(value & flag & info [ "with-test-gen" ] ~deprecated ~doc)


  let with_testing =
    let doc =
      "Generate intrumentation in a format more amenable to testing. Ignores an existing \
       `main` function."
    in
    Arg.(value & flag & info [ "with-testing" ] ~doc)


  let run =
    let doc = "Run the instrumented program" in
    Arg.(value & flag & info [ "run" ] ~doc)


  let mktemp =
    let doc = "Use a temporary directory" in
    Arg.(value & flag & info [ "tmp" ] ~doc)


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


  let no_debug_info =
    let doc = "Run the instrumented program without collecting debug information" in
    Arg.(value & flag & info [ "no-debug-info" ] ~doc)


  let exec_c_locs_mode =
    let doc =
      "At errors, report the location in the Fulminated output, not the source. This \
       flag needs to be enabled for lldb to be usable on the instrumented binary."
    in
    Arg.(value & flag & info [ "exec-c-locs-mode" ] ~doc)


  let experimental_ownership_stack_mode =
    let doc =
      "(experimental) Record (and report, in case of ownership error) the source \
       locations where ownership was taken for Fulminate-tracked memory"
    in
    Arg.(value & flag & info [ "ownership-stack-mode" ] ~doc)


  let experimental_unions =
    let doc = "(experimental) Handle unions from source" in
    Arg.(value & flag & info [ "experimental-unions" ] ~doc)


  let max_bump_blocks =
    let doc = "Maximum number of bump allocator blocks (default: 256)" in
    Arg.(value & opt (some int) None & info [ "max-bump-blocks" ] ~doc)


  let bump_block_size =
    let doc =
      "Size of each bump allocator block in bytes (default: 8388608 = 8m). Supports \
       suffixes: k/K for kilobytes, m/M for megabytes, g/G for gigabytes. Examples: 8m, \
       8192k, 8388608"
    in
    Arg.(value & opt (some size_converter) None & info [ "bump-block-size" ] ~doc)


  let experimental_curly_braces =
    let doc = "(experimental) Insert curly braces for single-statement control flow" in
    Arg.(value & flag & info [ "insert-curly-braces" ] ~doc)

  let experimental_lua_runtime =
    let doc = "(experimental) Use Lua as the runtime environment for Fulminate" in
    Arg.(value & flag & info [ "experimental-lua-runtime" ] ~doc)
end

let run_existing
    cc
    print_steps
    experimental_lua_runtime
    filename
  =
  let is_handwritten = true in
  let output_dir = "" in

  run_instrumented_file
    ~filename
    ~cc
    ~no_debug_info:false
    ~output:None
    ~output_dir
    ~print_steps
    ~experimental_lua_runtime
    ~is_handwritten

let run_existing_term =
  let one_file =
    Term.map Common.there_can_only_be_one Common.Flags.file in

  Term.(
    const run_existing
    $ Common.Flags.cc
    $ Flags.print_steps
    $ Flags.experimental_lua_runtime
    $ one_file
  )

let run_existing_cmd =
  let doc =
    "Run an already-instrumented CN executable C file"
  in
  let info =
    Cmd.info "run-existing" ~doc
  in
  Cmd.v info run_existing_term

let instrument_term =
  let open Term in
  const generate_executable_specs
  $ Common.Flags.file
  $ Common.Flags.cc
  $ Common.Flags.macros
  $ Common.Flags.permissive
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
  $ Common.Flags.astprints
  $ Verify.Flags.dont_use_vip
  $ Verify.Flags.fail_fast
  $ Common.Flags.no_inherit_loc
  $ Common.Flags.magic_comment_char_dollar
  $ Common.Flags.allow_split_magic_comments
  $ Flags.output
  $ Flags.output_dir
  $ Flags.without_ownership_checking
  $ Flags.without_loop_invariants
  $ Flags.with_loop_leak_checks
  $ Flags.without_lemma_checks
  $ Term.map
      (fun (x, y) -> x || y)
      (Term.product Flags.with_test_gen Flags.with_testing)
  $ Flags.run
  $ Flags.no_debug_info
  $ Flags.exec_c_locs_mode
  $ Flags.experimental_ownership_stack_mode
  $ Flags.experimental_unions
  $ Flags.experimental_curly_braces
  $ Flags.experimental_lua_runtime
  $ Flags.mktemp
  $ Flags.print_steps
  $ Flags.max_bump_blocks
  $ Flags.bump_block_size

let instrument_cmd =
  let doc =
    "Instruments [FILE] with runtime C assertions that check the properties provided in \
     CN specifications.\n"
  in
  let info = Cmd.info "instrument" ~doc in
  Cmd.v info instrument_term

let cmds = [ instrument_cmd; run_existing_cmd ]