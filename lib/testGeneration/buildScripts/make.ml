module Config = TestGenConfig
open Pp

let header ~output_dir =
  !^"# Auto-generated Makefile for CN test generation"
  ^^ twice hardline
  ^^ !^"# Basic variables"
  ^^ hardline
  ^^ !^"RUNTIME_PREFIX := $(OPAM_SWITCH_PREFIX)/lib/cn/runtime"
  ^^ hardline
  ^^ !^"TEST_DIR := "
  ^^ string (Filename.dirname (Filename.concat output_dir "junk"))
  ^^ twice hardline
  ^^ !^"# Check if runtime exists"
  ^^ hardline
  ^^ !^"ifeq ($(wildcard $(RUNTIME_PREFIX)),)"
  ^^ hardline
  ^^ !^"\t$(error Could not find CN's runtime directory (looked at: '$(RUNTIME_PREFIX)'))"
  ^^ hardline
  ^^ !^"endif"
  ^^ twice hardline


let define_cc_flags () =
  let base_flags = [ "-g"; "-I$(RUNTIME_PREFIX)/include/"; "$(CFLAGS)"; "$(CPPFLAGS)" ] in
  let sanitize_flags =
    let sanitize, no_sanitize = Config.has_sanitizers () in
    (match sanitize with Some sanitize -> [ "-fsanitize=" ^ sanitize ] | None -> [])
    @
    match no_sanitize with
    | Some no_sanitize -> [ "-fno-sanitize=" ^ no_sanitize ]
    | None -> []
  in
  let coverage_flags = if Config.is_coverage () then [ "--coverage" ] else [] in
  let all_flags = base_flags @ sanitize_flags @ coverage_flags in
  !^"CFLAGS_TEST := " ^^ separate_map space string all_flags ^^ hardline


let define_test_flags () =
  let flags =
    []
    @ (if Config.is_print_seed () then [ "--print-seed" ] else [])
    @ (Config.has_input_timeout ()
       |> Option.map (fun input_timeout ->
         [ "--input-timeout"; string_of_int input_timeout ])
       |> Option.to_list
       |> List.flatten)
    @ (Config.has_null_in_every ()
       |> Option.map (fun null_in_every ->
         [ "--null-in-every"; string_of_int null_in_every ])
       |> Option.to_list
       |> List.flatten)
    @ (Config.has_seed ()
       |> Option.map (fun seed -> [ "--seed"; seed ])
       |> Option.to_list
       |> List.flatten)
    @ (Config.has_logging_level_str ()
       |> Option.map (fun level -> [ "--logging-level"; level ])
       |> Option.to_list
       |> List.flatten)
    @ (Config.has_trace_granularity_str ()
       |> Option.map (fun granularity -> [ "--trace-granularity"; granularity ])
       |> Option.to_list
       |> List.flatten)
    @ (Config.has_progress_level_str ()
       |> Option.map (fun level -> [ "--progress-level"; level ])
       |> Option.to_list
       |> List.flatten)
    @ (match Config.is_until_timeout () with
       | Some timeout -> [ "--until-timeout"; string_of_int timeout ]
       | None -> [])
    @ (if Config.is_exit_fast () then [ "--exit-fast" ] else [])
    @ (Config.has_max_stack_depth ()
       |> Option.map (fun max_stack_depth ->
         [ "--max-stack-depth"; string_of_int max_stack_depth ])
       |> Option.to_list
       |> List.flatten)
    @ (Config.has_max_generator_size ()
       |> Option.map (fun max_generator_size ->
         [ "--max-generator-size"; string_of_int max_generator_size ])
       |> Option.to_list
       |> List.flatten)
    @ (Config.has_sizing_strategy_str ()
       |> Option.map (fun sizing_strategy -> [ "--sizing-strategy"; sizing_strategy ])
       |> Option.to_list
       |> List.flatten)
    @ (Config.has_allowed_size_split_backtracks ()
       |> Option.map (fun allowed_size_split_backtracks ->
         [ "--allowed-size-split-backtracks";
           string_of_int allowed_size_split_backtracks
         ])
       |> Option.to_list
       |> List.flatten)
    @ (if Config.is_trap () then [ "--trap" ] else [])
    @ (if Config.has_no_replays () then [ "--no-replays" ] else [])
    @ (if Config.has_no_replicas () then [ "--no-replicas" ] else [])
    @ (match Config.get_output_tyche () with
       | Some file -> [ "--output-tyche"; file ]
       | None -> [])
    @ (if Config.will_print_size_info () then [ "--print-size-info" ] else [])
    @ (if Config.will_print_backtrack_info () then
         [ "--print-backtrack-info" ]
       else
         [])
    @
    if Config.will_print_satisfaction_info () then
      [ "--print-satisfaction-info" ]
    else
      []
  in
  !^"TEST_FLAGS := " ^^ separate_map space string flags ^^ hardline


let coverage_rules ~filename_base =
  !^"# Coverage rules"
  ^^ hardline
  ^^ !^"\t@echo \"Generating coverage report\""
  ^^ hardline
  ^^ !^"\tlcov -b . --capture "
  (* TODO: Related to working around line markers *)
  (* ^^ !^"--substitute" *)
  (* ^^ !^("\"s/.*" ^ filename_base ^ ".c" ^ "/" ^ filename_base ^ ".exec.c/\"") *)
  ^^ !^"--directory . --output-file coverage.info --gcov-tool gcov"
  ^^ hardline
  ^^ !^"\tlcov --ignore-errors unused --directory . --remove coverage.info -o \
        coverage_filtered.info "
  ^^ !^filename_base
  ^^ !^".test.c "
  ^^ !^filename_base
  ^^ !^".gen.h"
  ^^ hardline
  ^^ !^"\tgenhtml --output-directory html coverage_filtered.info"
  ^^ hardline
  ^^ !^"\t@echo \"Coverage report generated at '$(TEST_DIR)/html/'\""
  ^^ twice hardline


let rules ~filename_base =
  !^".PHONY: all compile test clean"
  ^^ twice hardline
  ^^ !^"# Run tests"
  ^^ hardline
  ^^ !^"test: tests.out"
  ^^ hardline
  ^^ !^"\t@"
  ^^ !^(if Config.is_print_steps () then "echo \"Running tests\"" else ":")
  ^^ hardline
  ^^ !^"\t./$< $(TEST_FLAGS)"
  ^^ (if Config.is_coverage () then
        coverage_rules ~filename_base
      else
        twice hardline)
  ^^ !^"# Compilation rules"
  ^^ hardline
  ^^ !^(filename_base ^ ".test.o: " ^ filename_base ^ ".test.c")
  ^^ hardline
  ^^ !^"\t@"
  ^^ !^(if Config.is_print_steps () then
          "echo \"Compiling " ^ filename_base ^ ".test.c\""
        else
          ":")
  ^^ hardline
  ^^ !^"\t$(CC) -c -o $@ $< $(CFLAGS_TEST)"
  ^^ twice hardline
  ^^ !^(filename_base ^ ".exec.o: " ^ filename_base ^ ".exec.c")
  ^^ hardline
  ^^ !^"\t@"
  ^^ !^(if Config.is_print_steps () then
          "echo \"Compiling " ^ filename_base ^ ".exec.c\""
        else
          ":")
  ^^ hardline
  ^^ !^"\t$(CC) -c -o $@ $< $(CFLAGS_TEST)"
  ^^ twice hardline
  ^^ !^"# Linking rule"
  ^^ hardline
  ^^ !^"tests.out: "
  ^^ string (filename_base ^ ".test.o ")
  ^^ string (filename_base ^ ".exec.o")
  ^^ hardline
  ^^ !^"\t@"
  ^^ !^(if Config.is_print_steps () then "echo \"Linking object files\"" else ":")
  ^^ hardline
  ^^ !^(String.concat
          " "
          [ "\t$(CC)";
            "-o";
            "$@";
            "$^";
            "$(RUNTIME_PREFIX)/libcn_test.a";
            "$(RUNTIME_PREFIX)/libbennet.a";
            "\"${RUNTIME_PREFIX}/libcn_smt.a\"";
            "$(RUNTIME_PREFIX)/libcn_replica.a";
            "$(RUNTIME_PREFIX)/libcn_exec.a";
            "$(CFLAGS_TEST)"
          ])
  ^^ twice hardline


let clean_rule =
  !^"# Clean rule"
  ^^ hardline
  ^^ !^"clean:"
  ^^ hardline
  ^^ !^"\t$(RM) *.o tests.out *.gcno *.gcda coverage*.info"
  ^^ hardline
  ^^ !^"\t$(RM) -r html"
  ^^ hardline


let generate ~(output_dir : string) ~(filename_base : string) : Pp.document =
  header ~output_dir
  ^^ define_cc_flags ()
  ^^ define_test_flags ()
  ^^ twice hardline
  ^^ rules ~filename_base
  ^^ clean_rule


let filename_base fn = fn |> Filename.basename |> Filename.remove_extension

let save ?(perm = 0o666) (output_dir : string) (filename : string) (doc : Pp.document)
  : unit
  =
  let oc =
    Stdlib.open_out_gen
      [ Open_wronly; Open_creat; Open_trunc; Open_text ]
      perm
      (Filename.concat output_dir filename)
  in
  output_string oc (Pp.plain ~width:80 doc);
  close_out oc


let generate_and_save ~(output_dir : string) ~(filename : string) : unit =
  let script_doc = generate ~output_dir ~filename_base:(filename_base filename) in
  save ~perm:0o777 output_dir "Makefile" script_doc
