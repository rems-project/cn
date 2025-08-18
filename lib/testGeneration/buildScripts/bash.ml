module Config = TestGenConfig
open Pp

let setup ~output_dir =
  !^"#!/bin/bash"
  ^^ twice hardline
  ^^ !^"# Auto-generated bash script for CN test generation"
  ^^ hardline
  ^^ !^"RUNTIME_PREFIX=\"$OPAM_SWITCH_PREFIX/lib/cn/runtime\""
  ^^ hardline
  ^^ !^"[ -d \"${RUNTIME_PREFIX}\" ]"
  ^^^ twice bar
  ^^^ parens
        (nest
           4
           (hardline
            ^^ !^"printf \"Could not find CN's runtime directory (looked at: \
                  '${RUNTIME_PREFIX}')\""
            ^^ hardline
            ^^ !^"exit 1")
         ^^ hardline)
  ^^ twice hardline
  ^^ !^("TEST_DIR=" ^ Filename.dirname (Filename.concat output_dir "junk"))
  ^^ hardline
  ^^ !^"pushd $TEST_DIR > /dev/null"
  ^^ hardline


let attempt cmd success failure =
  separate_map space string [ "if"; cmd; ";"; "then" ]
  ^^ nest
       4
       (hardline
        ^^ if Config.is_print_steps () then string ("echo \"" ^ success ^ "\"") else colon
       )
  ^^ hardline
  ^^ !^"else"
  ^^ nest 4 (hardline ^^ !^("printf \"" ^ failure ^ "\"") ^^ hardline ^^ !^"exit 1")
  ^^ hardline
  ^^ !^"fi"


let cc_flags () =
  [ "-g"; "\"-I${RUNTIME_PREFIX}/include/\""; "${CFLAGS}"; "${CPPFLAGS}" ]
  @ (let sanitize, no_sanitize = Config.has_sanitizers () in
     (match sanitize with Some sanitize -> [ "-fsanitize=" ^ sanitize ] | None -> [])
     @
     match no_sanitize with
     | Some no_sanitize -> [ "-fno-sanitize=" ^ no_sanitize ]
     | None -> [])
  @
  if Config.is_coverage () then
    [ "--coverage" ]
  else
    []


let compile ~filename_base =
  !^"# Compile"
  ^^ hardline
  ^^ attempt
       (String.concat
          " "
          ([ Config.get_cc ();
             "-c";
             "-o";
             "\"./" ^ filename_base ^ ".test.o\"";
             "\"./" ^ filename_base ^ ".test.c\""
           ]
           @ cc_flags ()))
       ("Compiled '" ^ filename_base ^ ".test.c'.")
       ("Failed to compile '" ^ filename_base ^ ".test.c' in ${TEST_DIR}.")
  ^^ (twice hardline
      ^^ attempt
           (String.concat
              " "
              ([ Config.get_cc ();
                 "-c";
                 "-o";
                 "\"./" ^ filename_base ^ ".exec.o\"";
                 "\"./" ^ filename_base ^ ".exec.c\""
               ]
               @ cc_flags ()))
           ("Compiled '" ^ filename_base ^ ".exec.c'.")
           ("Failed to compile '" ^ filename_base ^ ".exec.c' in ${TEST_DIR}."))
  ^^ hardline


let link ~filename_base =
  !^"# Link"
  ^^ hardline
  ^^ (if Config.is_print_steps () then
        !^"echo" ^^ twice hardline
      else
        empty)
  ^^ attempt
       (String.concat
          " "
          ([ Config.get_cc ();
             "-o";
             "\"./tests.out\"";
             filename_base ^ ".test.o " ^ filename_base ^ ".exec.o";
             "\"${RUNTIME_PREFIX}/libcn_test.a\"";
             "\"${RUNTIME_PREFIX}/libbennet.a\"";
             "\"${RUNTIME_PREFIX}/libcn_smt.a\"";
             "\"${RUNTIME_PREFIX}/libcn_replica.a\"";
             "\"${RUNTIME_PREFIX}/libcn_exec.a\""
           ]
           @ cc_flags ()))
       "Linked C *.o files."
       "Failed to link *.o files in ${TEST_DIR}."
  ^^ hardline


let run () =
  let cmd =
    separate_map
      space
      string
      ([ "./tests.out" ]
       @ (if Config.is_print_seed () then
            [ "--print-seed" ]
          else
            [])
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
       @ (if Config.is_exit_fast () then
            [ "--exit-fast" ]
          else
            [])
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
       @ (if Config.is_trap () then
            [ "--trap" ]
          else
            [])
       @ (if Config.has_no_replays () then
            [ "--no-replays" ]
          else
            [])
       @ (if Config.has_no_replicas () then
            [ "--no-replicas" ]
          else
            [])
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
         [])
  in
  !^"# Run"
  ^^ hardline
  ^^ (if Config.is_print_steps () then
        !^"echo" ^^ twice hardline
      else
        empty)
  ^^ cmd
  ^^ hardline
  ^^ !^"test_exit_code=$? # Save tests exit code for later"
  ^^ hardline


let coverage ~filename_base =
  !^"# Coverage"
  ^^ hardline
  ^^ !^"echo"
  ^^ hardline
  ^^ attempt
       (String.concat
          " "
          [ "lcov";
            "-b";
            ".";
            "--capture";
            (* TODO: Related to working around line markers *)
            (* "--substitute"; *)
            (* "\"s/.*" ^ filename_base ^ ".c" ^ "/" ^ filename_base ^ ".exec.c/\""; *)
            "--directory";
            ".";
            "--output-file";
            "coverage.info";
            "--gcov-tool";
            "gcov"
          ])
       "Collected coverage via lcov."
       "Failed to collect coverage."
  ^^ twice hardline
  ^^ attempt
       (String.concat
          " "
          [ "lcov";
            "--ignore-errors unused";
            "--directory .";
            "--remove coverage.info";
            "-o coverage_filtered.info";
            filename_base ^ ".test.c";
            filename_base ^ ".gen.h"
          ])
       "Exclude test harnesses from coverage via lcov."
       "Failed to exclude test harnesses from coverage."
  ^^ twice hardline
  ^^ attempt
       "genhtml --output-directory html \"coverage_filtered.info\""
       "Generated HTML report at '${TEST_DIR}/html/'."
       "Failed to generate HTML report."
  ^^ hardline


let generate ~(output_dir : string) ~(filename_base : string) : Pp.document =
  setup ~output_dir
  ^^ hardline
  ^^ compile ~filename_base
  ^^ hardline
  ^^ link ~filename_base
  ^^ hardline
  ^^ run ()
  ^^ hardline
  ^^ (if Config.is_coverage () then
        coverage ~filename_base ^^ hardline
      else
        empty)
  ^^ !^"popd > /dev/null"
  ^^ hardline
  ^^ !^"exit $test_exit_code"
  ^^ hardline


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
  save ~perm:0o777 output_dir "run_tests.sh" script_doc
