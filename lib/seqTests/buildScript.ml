module Config = SeqTestGenConfig
open Pp

let setup ~output_dir =
  string "#!/bin/bash"
  ^^ twice hardline
  ^^ string "# copied from cn-runtime-single-file.sh"
  ^^ hardline
  ^^ string "RUNTIME_PREFIX=\"$OPAM_SWITCH_PREFIX/lib/cn/runtime\""
  ^^ hardline
  ^^ string "[ -d \"${RUNTIME_PREFIX}\" ]"
  ^^ space
  ^^ twice bar
  ^^ space
  ^^ parens
       (nest
          4
          (hardline
           ^^ string
                "printf \"Could not find CN's runtime directory (looked at: \
                 '${RUNTIME_PREFIX}')\""
           ^^ hardline
           ^^ string "exit 1")
        ^^ hardline)
  ^^ twice hardline
  ^^ string ("TEST_DIR=" ^ Filename.dirname (Filename.concat output_dir "junk"))
  ^^ hardline
  ^^ string "pushd $TEST_DIR > /dev/null"
  ^^ hardline


let attempt cmd success failure =
  separate_map space string [ "if"; cmd; ";"; "then" ]
  ^^ nest
       4
       (hardline
        ^^ if Config.is_print_steps () then string ("echo \"" ^ success ^ "\"") else colon
       )
  ^^ hardline
  ^^ string "else"
  ^^ nest
       4
       (hardline ^^ string ("printf \"" ^ failure ^ "\"") ^^ hardline ^^ string "exit 1")
  ^^ hardline
  ^^ string "fi"


let cc_flags () = [ "-g"; "\"-I${RUNTIME_PREFIX}/include/\"" ]

let compile ~filename_base =
  string "# Compile"
  ^^ hardline
  ^^ attempt
       (String.concat
          " "
          ([ "cc";
             "-c";
             "-o";
             "\"./" ^ filename_base ^ ".test.o\"";
             "\"./" ^ filename_base ^ ".test.c\""
           ]
           @ cc_flags ()))
       ("Compiled '" ^ filename_base ^ ".test.c'.")
       ("Failed to compile '" ^ filename_base ^ ".test.c' in ${TEST_DIR}.")
  ^^ (if Config.with_static_hack () then
        empty
      else
        twice hardline
        ^^ attempt
             (String.concat
                " "
                ([ "cc";
                   "-c";
                   "-o";
                   "\"./" ^ filename_base ^ ".exec.o\"";
                   "\"./" ^ filename_base ^ ".exec.c\""
                 ]
                 @ cc_flags ()))
             ("Compiled '" ^ filename_base ^ ".exec.c'.")
             ("Failed to compile '" ^ filename_base ^ ".exec.c' in ${TEST_DIR}.")
        ^^ twice hardline
        ^^ attempt
             (String.concat
                " "
                ([ "cc";
                   "-c";
                   "-o";
                   "\"./" ^ filename_base ^ ".cn.o\"";
                   "\"./" ^ filename_base ^ ".cn.c\""
                 ]
                 @ cc_flags ()))
             ("Compiled '" ^ filename_base ^ ".cn.c'.")
             ("Failed to compile '" ^ filename_base ^ ".cn.c' in ${TEST_DIR}."))
  ^^ hardline


let link ~filename_base =
  string "# Link"
  ^^ hardline
  ^^ (if Config.is_print_steps () then
        string "echo" ^^ twice hardline
      else
        empty)
  ^^ attempt
       (String.concat
          " "
          ([ "cc";
             "-o";
             "\"./tests.out\"";
             (filename_base
              ^ ".test.o"
              ^
              if Config.with_static_hack () then
                ""
              else
                " " ^ filename_base ^ ".exec.o " ^ filename_base ^ ".cn.o");
             "\"${RUNTIME_PREFIX}/libcn_exec.a\"";
             "\"${RUNTIME_PREFIX}/libcn_test.a\"";
             "\"${RUNTIME_PREFIX}/libcn_replica.a\""
           ]
           @ cc_flags ()))
       "Linked C *.o files."
       "Failed to link *.o files in ${TEST_DIR}."
  ^^ hardline


let run (num_tests : int) =
  let create_run_string (i : int) =
    (* string (Printf.sprintf "echo \"Test %d\"" i)
    ^^ hardline
    ^^  *)
    separate_map space string [ "./tests.out"; string_of_int i ]
  in
  let cmds = List.map create_run_string (List.init num_tests Fun.id) in
  string "# Run"
  ^^ hardline
  ^^ (if Config.is_print_steps () then
        string "echo" ^^ twice hardline
      else
        empty)
  ^^ separate hardline cmds
  ^^ hardline
  ^^ string "test_exit_code=$? # Save tests exit code for later"
  ^^ hardline


let[@warning "-32" (* unused-value-declaration *)] coverage ~filename_base =
  string "# Coverage"
  ^^ hardline
  ^^ string "echo"
  ^^ hardline
  ^^ attempt
       ("gcov \"" ^ filename_base ^ ".test.c\"")
       "Recorded coverage via gcov."
       "Failed to record coverage."
  ^^ twice hardline
  ^^ attempt
       "lcov --capture --directory . --output-file coverage.info"
       "Collected coverage via lcov."
       "Failed to collect coverage."
  ^^ twice hardline
  ^^ attempt
       "genhtml --output-directory html \"coverage.info\""
       "Generated HTML report at '${TEST_DIR}/html/'."
       "Failed to generate HTML report."
  ^^ hardline


let generate ~(output_dir : string) ~(filename_base : string) (num_tests : int)
  : Pp.document
  =
  setup ~output_dir
  ^^ hardline
  ^^ compile ~filename_base
  ^^ hardline
  ^^ link ~filename_base
  ^^ hardline
  ^^ run num_tests
  ^^ hardline
  ^^ string "popd > /dev/null"
  ^^ hardline
  ^^ string "exit $test_exit_code"
  ^^ hardline


let generate_intermediate
      ~(output_dir : string)
      ~(filename_base : string)
      (* (num_tests : int) *)
  : Pp.document
  =
  setup ~output_dir
  ^^ hardline
  ^^ compile ~filename_base
  ^^ hardline
  ^^ link ~filename_base
  ^^ hardline
  (* ^^ run_intermediate num_tests
  ^^ hardline *)
  ^^ string "popd > /dev/null"
  ^^ hardline
  ^^ string "exit $test_exit_code"
  ^^ hardline
