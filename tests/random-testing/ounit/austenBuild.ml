(* `cn test bennet|darcy --no-run` against a fake `austen`, which records its
   arguments and exits with a chosen status. Every path that CN passes to
   Austen contains a space. *)

open OUnit2

let fixture () = Sys.getenv "CN_AUSTEN_SCALAR_FIXTURE"

let include_dir () = Filename.dirname (Sys.getenv "CN_AUSTEN_FORCED_INCLUDE")

(* A fresh directory whose path contains a space, removed after [f].
   ([Filename.temp_dir] needs OCaml 5.1; CI builds with 4.14.) *)
let with_temp_dir f =
  let dir = Filename.temp_file "cn austen " "" in
  Sys.remove dir;
  Sys.mkdir dir 0o700;
  let rec remove path =
    if Sys.is_directory path then (
      Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
      Sys.rmdir path)
    else
      Sys.remove path
  in
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> f dir)


let write_file path contents =
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc contents)


let copy_file ~src ~dst =
  write_file dst (In_channel.with_open_bin src In_channel.input_all)


(* A fake `austen` in [dir] that writes its NUL-terminated arguments to
   [args] and exits with [exit_code]. *)
let install_fake_austen ?signal ~dir ~args ~exit_code () =
  let path = Filename.concat dir "austen" in
  Out_channel.with_open_bin path (fun oc ->
    Printf.fprintf
      oc
      "#!/bin/sh\nfor arg in \"$@\"; do printf '%%s\\0' \"$arg\"; done > %s\n%s\n"
      (Filename.quote args)
      (match signal with
       | Some signal -> "kill -" ^ signal ^ " $$"
       | None -> "exit " ^ string_of_int exit_code));
  Unix.chmod path 0o755


let recorded_args path =
  let recorded = In_channel.with_open_bin path In_channel.input_all in
  String.split_on_char '\000' (String.sub recorded 0 (String.length recorded - 1))


(* Run `cn test ARGS` with [path_prefix] (if any) before a [PATH] that
   provides no other `austen`, returning its status and standard error. *)
let run_cn_test ?path_prefix args =
  let stderr_path = Filename.temp_file "cn-austen-stderr" ".txt" in
  Fun.protect
    ~finally:(fun () -> Sys.remove stderr_path)
    (fun () ->
       let status =
         SpecExport.with_null_fd (fun null_fd ->
           let err_fd = Unix.openfile stderr_path [ O_WRONLY; O_TRUNC ] 0 in
           Fun.protect
             ~finally:(fun () -> Unix.close err_fd)
             (fun () ->
                SpecExport.run_cn
                  ~env:(SpecExport.environment_without_austen ?path_prefix ())
                  ~stdout_fd:null_fd
                  ~stderr_fd:err_fd
                  ("test" :: args)))
       in
       (status, In_channel.with_open_bin stderr_path In_channel.input_all))


let assert_status expected (status, stderr) =
  assert_equal
    ~printer:SpecExport.string_of_process_status
    ~msg:stderr
    (Unix.WEXITED expected)
    status


let assert_contains ~needle haystack =
  let n = String.length needle in
  let rec go i =
    i + n <= String.length haystack
    && (String.equal (String.sub haystack i n) needle || go (i + 1))
  in
  assert_bool (Printf.sprintf "expected %S in %S" needle haystack) (go 0)


(* The fixture copied to [dir], the fake Austen's directory and recording,
   and the output directory, all below [dir]. *)
let setup ?signal ?(exit_code = 0) dir =
  let source = Filename.concat dir "scalar fixture.c" in
  copy_file ~src:(fixture ()) ~dst:source;
  let bin = Filename.concat dir "fake bin" in
  Sys.mkdir bin 0o755;
  let args = Filename.concat dir "austen args" in
  install_fake_austen ?signal ~dir:bin ~args ~exit_code ();
  (source, bin, args, Filename.concat dir "out dir")


let source_flags ~source =
  [ source;
    "-D";
    "STEP=1";
    "-I";
    include_dir ();
    "--include";
    Filename.concat (include_dir ()) "forced.h"
  ]


(* The arguments CN passes to Austen to build [fn] from [source]. *)
let expected_argv ?(ownership = true) ~engine ~fn ~out ~spec ~source () =
  [ "test";
    "--engine=" ^ engine;
    "--function=" ^ fn;
    "--work=" ^ Filename.concat (Filename.concat out "work") fn;
    "--build-only"
  ]
  @ (if ownership then [ "--check-heap-ownership" ] else [])
  @ [ "--cflag=-D";
      "--cflag=__CN_TEST";
      "--cflag=-D";
      "--cflag=__AUSTEN_TEST";
      "--cflag=-D";
      "--cflag=STEP=1";
      "--cflag=-I";
      "--cflag=" ^ include_dir ();
      "--cflag=-include";
      "--cflag=" ^ Filename.concat (include_dir ()) "forced.h";
      "--";
      spec;
      source
    ]


let builds_one_function_with_engine engine _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  run_cn_test
    ~path_prefix:bin
    ([ engine; "--no-run"; "--only=decrement"; "--output-dir"; out ]
     @ source_flags ~source)
  |> assert_status 0;
  let spec = Filename.concat out "spec.json" in
  assert_equal
    ~printer:(String.concat "\n")
    (expected_argv ~engine ~fn:"decrement" ~out ~spec ~source ())
    (recorded_args args);
  assert_bool "the specification was not exported" (Sys.file_exists spec)


let builds_with_exported_spec_path _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  let spec = Filename.concat dir "my spec.json" in
  run_cn_test
    ~path_prefix:bin
    ([ "bennet";
       "--no-run";
       "--only=increment";
       "--output-dir";
       out;
       "--export-spec-json";
       spec
     ]
     @ source_flags ~source)
  |> assert_status 0;
  assert_bool "the specification was not exported" (Sys.file_exists spec);
  assert_equal
    ~printer:(String.concat "\n")
    (expected_argv ~engine:"bennet" ~fn:"increment" ~out ~spec ~source ())
    (recorded_args args)


let builds_without_ownership_checking _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  run_cn_test
    ~path_prefix:bin
    ([ "bennet";
       "--no-run";
       "--only=decrement";
       "--without-ownership-checking";
       "--output-dir";
       out
     ]
     @ source_flags ~source)
  |> assert_status 0;
  assert_equal
    ~printer:(String.concat "\n")
    (expected_argv
       ~ownership:false
       ~engine:"bennet"
       ~fn:"decrement"
       ~out
       ~spec:(Filename.concat out "spec.json")
       ~source
       ())
    (recorded_args args)


(* A static root is selected by the name the exporter gives it. *)
let builds_a_static_function_by_its_exported_name _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  run_cn_test
    ~path_prefix:bin
    ([ "bennet"; "--no-run"; "--only=twice"; "--output-dir"; out ] @ source_flags ~source)
  |> assert_status 0;
  let spec = Filename.concat out "spec.json" in
  let fn = "twice" in
  assert_equal
    ~printer:(String.concat "\n")
    (expected_argv ~engine:"bennet" ~fn ~out ~spec ~source ())
    (recorded_args args);
  let names =
    match Yojson.Safe.from_file spec with
    | `Assoc fields ->
      (match List.assoc "functions" fields with
       | `List functions -> List.map (SpecExport.json_field "name") functions
       | _ -> [])
    | _ -> []
  in
  assert_bool
    "the exported module does not name the static root"
    (List.mem (`String fn) names)


let assert_no_build ~args ~out =
  assert_bool "the output directory was created" (not (Sys.file_exists out));
  assert_bool "the fake Austen ran" (not (Sys.file_exists args))


let missing_austen_is_reported _ =
  with_temp_dir
  @@ fun dir ->
  let source, _, args, out = setup dir in
  let ((_, stderr) as result) =
    run_cn_test
      ([ "darcy"; "--no-run"; "--only=decrement"; "--output-dir"; out ]
       @ source_flags ~source)
  in
  assert_status 2 result;
  assert_contains ~needle:"could not find 'austen' on PATH" stderr;
  assert_contains ~needle:"cargo install austen" stderr;
  assert_no_build ~args ~out


let austen_failure_is_reported exit_code _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup ~exit_code dir in
  let ((_, stderr) as result) =
    run_cn_test
      ~path_prefix:bin
      ([ "bennet"; "--no-run"; "--only=decrement"; "--output-dir"; out ]
       @ source_flags ~source)
  in
  assert_status 2 result;
  assert_bool "the fake Austen did not run" (Sys.file_exists args);
  assert_contains
    ~needle:
      (Printf.sprintf
         "exited with status %d while building the harness for decrement"
         exit_code)
    stderr


let interrupted_austen_is_reported _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup ~exit_code:130 dir in
  run_cn_test
    ~path_prefix:bin
    ([ "darcy"; "--no-run"; "--only=decrement"; "--output-dir"; out ]
     @ source_flags ~source)
  |> assert_status 130;
  assert_bool "the fake Austen did not run" (Sys.file_exists args)


let signaled_austen_is_reported _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup ~signal:"TERM" dir in
  let ((_, stderr) as result) =
    run_cn_test
      ~path_prefix:bin
      ([ "bennet"; "--no-run"; "--only=decrement"; "--output-dir"; out ]
       @ source_flags ~source)
  in
  assert_status 2 result;
  assert_bool "the fake Austen did not run" (Sys.file_exists args);
  assert_contains ~needle:"was killed by signal" stderr


let unspawnable_austen_is_reported _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  (* Executable and discoverable, but its interpreter is absent. *)
  write_file (Filename.concat bin "austen") "#!/cn-test-nonexistent-interpreter\n";
  let ((_, stderr) as result) =
    run_cn_test
      ~path_prefix:bin
      ([ "darcy"; "--no-run"; "--only=decrement"; "--output-dir"; out ]
       @ source_flags ~source)
  in
  assert_status 2 result;
  assert_contains ~needle:"could not run '" stderr;
  assert_bool "the fake Austen ran" (not (Sys.file_exists args))


let invalid_selection_is_refused ~status ~message selection _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  let ((_, stderr) as result) =
    run_cn_test
      ~path_prefix:bin
      ([ "bennet"; "--no-run"; "--output-dir"; out ] @ selection @ source_flags ~source)
  in
  assert_status status result;
  assert_contains ~needle:message stderr;
  assert_no_build ~args ~out


let campaign_option_with_no_run_is_refused engine (flag, value) _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  (* Invalid C proves option validation happens before the frontend. *)
  write_file source "#error frontend must not run\n";
  let ((_, stderr) as result) =
    run_cn_test
      ~path_prefix:bin
      ([ engine; "--no-run"; "--only=decrement"; "--output-dir"; out; flag ]
       @ value
       @ source_flags ~source)
  in
  assert_status 124 result;
  assert_contains ~needle:(flag ^ " cannot be used with --no-run") stderr;
  assert_no_build ~args ~out


let frontend_failure_does_not_run_austen _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  write_file source "#error deliberate preprocessing failure\n";
  let status, stderr =
    run_cn_test
      ~path_prefix:bin
      ([ "bennet"; "--no-run"; "--only=decrement"; "--output-dir"; out ]
       @ source_flags ~source)
  in
  assert_bool
    ("frontend failure was reported as success: " ^ stderr)
    (match status with Unix.WEXITED code -> code <> 0 | _ -> false);
  assert_no_build ~args ~out


let suite =
  "austen build-only"
  >::: [ "builds one function with bennet" >:: builds_one_function_with_engine "bennet";
         "builds one function with darcy" >:: builds_one_function_with_engine "darcy";
         "builds from --export-spec-json" >:: builds_with_exported_spec_path;
         "builds without heap ownership checking" >:: builds_without_ownership_checking;
         "builds a static function by its exported name"
         >:: builds_a_static_function_by_its_exported_name;
         "reports a missing austen" >:: missing_austen_is_reported;
         "reports an austen finding status as a build failure"
         >:: austen_failure_is_reported 1;
         "reports an austen infrastructure failure" >:: austen_failure_is_reported 2;
         "reports an unexpected austen failure" >:: austen_failure_is_reported 3;
         "preserves an austen interruption" >:: interrupted_austen_is_reported;
         "reports a signaled austen" >:: signaled_austen_is_reported;
         "reports an unspawnable austen" >:: unspawnable_austen_is_reported;
         "refuses an unmatched --only"
         >:: invalid_selection_is_refused
               ~status:2
               ~message:"functions specified in --only not found: absent"
               [ "--only=absent" ];
         "refuses a selection emptied by --skip"
         >:: invalid_selection_is_refused
               ~status:2
               ~message:"no functions were selected for testing"
               [ "--only=decrement"; "--skip=decrement" ];
         "does not build after a frontend failure"
         >:: frontend_failure_does_not_run_austen;
         "refuses several selected functions"
         >:: invalid_selection_is_refused
               ~status:2
               ~message:"3 were selected (increment, decrement, twice)"
               []
       ]
       @ List.map
           (fun engine ->
              engine ^ " campaign options"
              >::: List.map
                     (fun ((flag, _) as option) ->
                        flag >:: campaign_option_with_no_run_is_refused engine option)
                     [ ("--until-timeout", [ "0" ]);
                       ("--seed", [ "0" ]);
                       ("--output-tyche", [ "tyche.jsonl" ]);
                       ("--exit-fast", [])
                     ])
           [ "bennet"; "darcy" ]
