(* Campaign results must be backed by a fresh, internally consistent Austen
   report. These fakes run no generator or native target. *)

open OUnit2
open AustenBuild

let report_path out = Filename.concat out "report.json"

let fields = function
  | `Assoc fields -> fields
  | json -> assert_failure ("expected a JSON object: " ^ Yojson.Safe.to_string json)


let replace key value json = `Assoc ((key, value) :: List.remove_assoc key (fields json))

let remove key json = `Assoc (List.remove_assoc key (fields json))

let replace_oracle key value json =
  replace "oracle" (replace key value (List.assoc "oracle" (fields json))) json


(* The process whose pid [pid_file] records has exited and been reaped. *)
let assert_reaped ~msg pid_file =
  let pid = In_channel.with_open_bin pid_file In_channel.input_all |> int_of_string in
  assert_bool
    msg
    (try
       Unix.kill pid 0;
       false
     with
     | Unix.Unix_error (ESRCH, _, _) -> true)


(* Includes ordinary parse/precondition rejections: these are allowed when
   inputs also reach the target and the oracle can check them. *)
let report
      ?(engine = "bennet")
      ?(duration = 10)
      ?(ownership = true)
      ?(confirmed = 0)
      ?(post_failed = 0)
      ?(seed = `Null)
      ()
  =
  `Assoc
    [ ("schema_version", `Int 1);
      ("function", `String "decrement");
      ("engine", `String engine);
      ("budget", `String "seconds");
      ("duration", `Int duration);
      ("seed", seed);
      ("check_heap_ownership", `Bool ownership);
      ("effective_check_heap_ownership", `Bool ownership);
      ("status", `String "ok");
      ("stage", `String "complete");
      ("error", `Null);
      ("exit", `Int (if confirmed = 0 then 0 else 1));
      ("seen", `Int 5);
      ("selected", `Int 5);
      ("dispatched", `Int 3);
      ( "oracle",
        `Assoc
          [ ("parse_rejected", `Int 1);
            ("pre_passed", `Int 3);
            ("pre_failed", `Int 1);
            ("pre_unsupported", `Int 0);
            ("pre_internal_error", `Int 0);
            ("post_unsupported", `Int 0)
          ] );
      ("crashes", `Int confirmed);
      ( "findings",
        `Assoc
          [ ("unit", `String "crashing-executions");
            ("confirmed", `Int confirmed);
            ("capped", `Bool false);
            ("post_failed", `Int post_failed)
          ] )
    ]


(* Record the actual argument array, find Austen's requested report path, and
   optionally write the chosen report there before returning the chosen status. *)
let install_campaign_fake ?signal ?report ~bin ~args ~exit_code () =
  let write_report =
    match report with
    | None -> ""
    | Some contents ->
      Printf.sprintf "printf '%%s' %s > \"$report\"\n" (Filename.quote contents)
  in
  let finish =
    match signal with
    | Some signal -> "kill -" ^ signal ^ " $$\n"
    | None -> "exit " ^ string_of_int exit_code ^ "\n"
  in
  let script =
    Printf.sprintf
      "#!/bin/sh\n\
       for arg in \"$@\"; do printf '%%s\\0' \"$arg\"; done > %s\n\
       report=\n\
       for arg in \"$@\"; do\n\
      \  case \"$arg\" in\n\
      \    --report=*) report=${arg#--report=} ;;\n\
      \  esac\n\
       done\n\
       test -n \"$report\" || exit 99\n\
       %s%s"
      (Filename.quote args)
      write_report
      finish
  in
  let exe = Filename.concat bin "austen" in
  write_file exe script;
  Unix.chmod exe 0o755


let run_campaign ?(engine = "bennet") ?(extra = []) ~source ~bin ~out () =
  run_cn_test
    ~path_prefix:bin
    ([ engine; "--only=decrement"; "--output-dir"; out ] @ extra @ source_flags ~source)


let assert_campaign_arguments ?seed ~engine ~duration ~ownership ~source ~out ~spec args =
  let build = expected_argv ~engine ~fn:"decrement" ~source ~out ~spec () in
  let cflags = List.filter (String.starts_with ~prefix:"--cflag=") build in
  let expected_options =
    [ "test";
      "--engine=" ^ engine;
      "--function=decrement";
      "--work=" ^ Filename.concat (Filename.concat out "work") "decrement";
      "--duration=" ^ string_of_int duration;
      "--strict-duration";
      "--fail-on-crash";
      "--report=" ^ report_path out;
      "--crashes=" ^ Filename.concat (Filename.concat out "findings") "decrement"
    ]
    @ (match seed with None -> [] | Some seed -> [ "--seed=" ^ seed ])
    @ if ownership then [ "--check-heap-ownership" ] else []
  in
  let rec split_options options = function
    | "--" :: positional -> (List.rev options, positional)
    | option :: rest -> split_options (option :: options) rest
    | [] -> assert_failure "Austen arguments have no positional-argument separator"
  in
  let options, positional = split_options [] args in
  assert_equal ~printer:(String.concat "\n") [ spec; source ] positional;
  assert_equal
    ~msg:"compiler arguments changed order"
    ~printer:(String.concat "\n")
    cflags
    (List.filter (String.starts_with ~prefix:"--cflag=") options);
  assert_equal
    ~printer:(String.concat "\n")
    (List.sort String.compare expected_options)
    (List.filter (fun arg -> not (String.starts_with ~prefix:"--cflag=" arg)) options
     |> List.sort String.compare)


let successful_campaign engine _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  install_campaign_fake
    ~report:(Yojson.Safe.to_string (report ~engine ()))
    ~bin
    ~args
    ~exit_code:0
    ();
  run_campaign ~engine ~source ~bin ~out () |> assert_status 0;
  let spec = Filename.concat out "spec.json" in
  assert_bool "the specification was not exported" (Sys.file_exists spec);
  assert_bool "the campaign report was not retained" (Sys.file_exists (report_path out));
  assert_campaign_arguments
    ~engine
    ~duration:10
    ~ownership:true
    ~source
    ~out
    ~spec
    (recorded_args args)


let duration_and_ownership_are_forwarded _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  install_campaign_fake
    ~report:(Yojson.Safe.to_string (report ~duration:3 ~ownership:false ()))
    ~bin
    ~args
    ~exit_code:0
    ();
  run_campaign
    ~extra:[ "--until-timeout=3"; "--without-ownership-checking" ]
    ~source
    ~bin
    ~out
    ()
  |> assert_status 0;
  assert_campaign_arguments
    ~engine:"bennet"
    ~duration:3
    ~ownership:false
    ~source
    ~out
    ~spec:(Filename.concat out "spec.json")
    (recorded_args args)


let uses_export_spec_json _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  let spec = Filename.concat dir "chosen spec.json" in
  install_campaign_fake
    ~report:(Yojson.Safe.to_string (report ()))
    ~bin
    ~args
    ~exit_code:0
    ();
  run_campaign ~extra:[ "--export-spec-json"; spec ] ~source ~bin ~out ()
  |> assert_status 0;
  assert_bool "the chosen specification was not exported" (Sys.file_exists spec);
  assert_bool
    "an extra specification was exported"
    (not (Sys.file_exists (Filename.concat out "spec.json")));
  assert_campaign_arguments
    ~engine:"bennet"
    ~duration:10
    ~ownership:true
    ~source
    ~out
    ~spec
    (recorded_args args)


let seed_is_forwarded seed _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  install_campaign_fake
    ~report:(Yojson.Safe.to_string (report ~seed:(`Intlit seed) ()))
    ~bin
    ~args
    ~exit_code:0
    ();
  run_campaign ~extra:[ "--seed=" ^ seed ] ~source ~bin ~out () |> assert_status 0;
  assert_campaign_arguments
    ~seed
    ~engine:"bennet"
    ~duration:10
    ~ownership:true
    ~source
    ~out
    ~spec:(Filename.concat out "spec.json")
    (recorded_args args)


let incorrect_reported_seed_is_refused _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  install_campaign_fake
    ~report:(Yojson.Safe.to_string (report ~seed:(`Int 1) ()))
    ~bin
    ~args
    ~exit_code:0
    ();
  run_campaign ~extra:[ "--seed=0" ] ~source ~bin ~out () |> assert_status 2


let invalid_seed_is_refused seed _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  write_file source "#error seed must be checked before preprocessing\n";
  run_campaign ~extra:[ "--seed=" ^ seed ] ~source ~bin ~out () |> assert_status 124;
  assert_no_build ~args ~out


let finding_is_reported post_failed _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  install_campaign_fake
    ~report:(Yojson.Safe.to_string (report ~confirmed:1 ~post_failed ()))
    ~bin
    ~args
    ~exit_code:1
    ();
  run_campaign ~source ~bin ~out () |> assert_status 1


let rejected_report ?(exit_code = 0) contents _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  install_campaign_fake ~report:contents ~bin ~args ~exit_code ();
  let ((_, stderr) as result) = run_campaign ~source ~bin ~out () in
  assert_status 2 result;
  assert_bool "report failure had no diagnostic" (String.length stderr > 0)


let missing_report exit_code _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  install_campaign_fake ~bin ~args ~exit_code ();
  run_campaign ~source ~bin ~out () |> assert_status 2


let stale_report_is_not_reused _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  install_campaign_fake
    ~report:(Yojson.Safe.to_string (report ()))
    ~bin
    ~args
    ~exit_code:0
    ();
  run_campaign ~source ~bin ~out () |> assert_status 0;
  assert_bool "the first report was not written" (Sys.file_exists (report_path out));
  install_campaign_fake ~bin ~args ~exit_code:0 ();
  run_campaign ~source ~bin ~out () |> assert_status 2;
  assert_bool "the old report remains" (not (Sys.file_exists (report_path out)))


let child_status ?signal exit_code expected _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  install_campaign_fake ?signal ~bin ~args ~exit_code ();
  run_campaign ~source ~bin ~out () |> assert_status expected


(* The fake interrupts CN itself, then records the forwarded interrupt. Its
   fallback exits promptly even if CN fails to forward the signal. *)
let parent_interruption_is_forwarded_and_reaped _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, _, out = setup dir in
  let marker = Filename.concat dir "interrupt received" in
  let child_pid = Filename.concat dir "child pid" in
  let handler =
    Printf.sprintf "printf interrupted > %s; exit 130" (Filename.quote marker)
  in
  write_file
    (Filename.concat bin "austen")
    (Printf.sprintf
       "#!/bin/sh\n\
        printf '%%s' \"$$\" > %s\n\
        trap %s INT\n\
        kill -INT \"$PPID\"\n\
        sleep 2\n\
        exit 7\n"
       (Filename.quote child_pid)
       (Filename.quote handler));
  run_campaign ~source ~bin ~out () |> assert_status 130;
  assert_bool "CN did not forward SIGINT to Austen" (Sys.file_exists marker);
  assert_reaped ~msg:"the interrupted Austen process was not reaped" child_pid


(* Austen's compiler and harness processes share its isolated group. A child
   that interrupts CN must receive the forwarded signal itself, before its
   fallback returns, and be reaped by its supervising fake Austen. *)
let interruption_reaches_descendants _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, _, out = setup dir in
  let marker = Filename.concat dir "descendant interrupted" in
  let child_pid = Filename.concat dir "descendant pid" in
  let handler =
    Printf.sprintf "printf interrupted > %s; exit 130" (Filename.quote marker)
  in
  let descendant =
    Printf.sprintf
      "printf '%%s' \"$$\" > %s\ntrap %s INT\nkill -INT \"$1\"\nsleep 2\nexit 7\n"
      (Filename.quote child_pid)
      (Filename.quote handler)
  in
  write_file
    (Filename.concat bin "austen")
    (Printf.sprintf
       "#!/bin/sh\ntrap 'exit 130' INT\n/bin/sh -c %s ignored \"$PPID\"\nexit 7\n"
       (Filename.quote descendant));
  run_campaign ~source ~bin ~out () |> assert_status 130;
  assert_bool "CN did not forward SIGINT to Austen's descendant" (Sys.file_exists marker);
  assert_reaped ~msg:"Austen's descendant remains after interruption" child_pid


let build_interruption_is_preserved _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, _, out = setup ~signal:"INT" dir in
  run_cn_test
    ~path_prefix:bin
    ([ "bennet"; "--no-run"; "--only=decrement"; "--output-dir"; out ]
     @ source_flags ~source)
  |> assert_status 130


let invalid_duration duration _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  write_file source "#error duration must be checked before preprocessing\n";
  run_campaign ~extra:[ "--until-timeout=" ^ duration ] ~source ~bin ~out ()
  |> assert_status 124;
  assert_no_build ~args ~out


let preparation_failure contents _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  write_file source contents;
  run_campaign ~source ~bin ~out () |> assert_status 2;
  assert_no_build ~args ~out


let invalid_source_extension _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  let renamed = Filename.concat dir "source.txt" in
  Sys.rename source renamed;
  run_campaign ~source:renamed ~bin ~out () |> assert_status 2;
  assert_no_build ~args ~out


let report_cannot_replace_spec _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  run_campaign ~extra:[ "--export-spec-json"; report_path out ] ~source ~bin ~out ()
  |> assert_status 2;
  assert_bool "Austen ran with a conflicting report path" (not (Sys.file_exists args));
  ignore (Yojson.Safe.from_file (report_path out))


let missing_austen _ =
  with_temp_dir
  @@ fun dir ->
  let source, _, args, out = setup dir in
  run_cn_test
    ([ "bennet"; "--only=decrement"; "--output-dir"; out ] @ source_flags ~source)
  |> assert_status 2;
  assert_no_build ~args ~out


let unspawnable_austen _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  write_file (Filename.concat bin "austen") "#!/cn-test-nonexistent-interpreter\n";
  run_campaign ~source ~bin ~out () |> assert_status 2;
  assert_bool "the fake Austen ran" (not (Sys.file_exists args))


let invalid_selection selection _ =
  with_temp_dir
  @@ fun dir ->
  let source, bin, args, out = setup dir in
  run_cn_test
    ~path_prefix:bin
    ([ "bennet"; "--output-dir"; out ] @ selection @ source_flags ~source)
  |> assert_status 2;
  assert_no_build ~args ~out


let malformed_reports =
  let base = report () in
  [ ("malformed JSON", "{broken"); ("non-object JSON", "[]") ]
  @ List.map
      (fun (name, json) -> (name, Yojson.Safe.to_string json))
      [ ("missing schema version", remove "schema_version" base);
        ("unknown schema version", replace "schema_version" (`Int 2) base);
        ("wrong function", replace "function" (`String "increment") base);
        ("wrong engine", replace "engine" (`String "darcy") base);
        ("unbounded budget", replace "budget" (`String "unbounded") base);
        ("wrong duration", replace "duration" (`Int 1) base);
        ("disabled requested ownership", replace "check_heap_ownership" (`Bool false) base);
        ( "disabled effective ownership",
          replace "effective_check_heap_ownership" (`Bool false) base );
        ("error status", replace "status" (`String "error") base);
        ("unfinished stage", replace "stage" (`String "compile-target") base);
        ("non-null error", replace "error" (`Assoc []) base);
        ("wrong reported exit", replace "exit" (`Int 1) base);
        ("zero dispatched inputs", replace "dispatched" (`Int 0) base);
        ("negative dispatched count", replace "dispatched" (`Int (-1)) base);
        ("fractional dispatched count", replace "dispatched" (`Float 3.) base);
        ("missing oracle", remove "oracle" base);
        ("zero successful preconditions", replace_oracle "pre_passed" (`Int 0) base);
        ("unsupported preconditions", replace_oracle "pre_unsupported" (`Int 1) base);
        ("internal oracle error", replace_oracle "pre_internal_error" (`Int 1) base);
        ("unsupported postconditions", replace_oracle "post_unsupported" (`Int 1) base);
        ("inconsistent dispatch tally", replace "selected" (`Int 2) base);
        ("inconsistent oracle tally", replace_oracle "pre_failed" (`Int 4) base);
        ("inconsistent crash tally", replace "crashes" (`Int 1) base);
        ("missing findings", remove "findings" base);
        ( "invalid findings unit",
          replace
            "findings"
            (replace "unit" (`String "unknown") (List.assoc "findings" (fields base)))
            base )
      ]


let suite =
  "austen campaigns"
  >::: [ "runs bennet with a ten-second budget" >:: successful_campaign "bennet";
         "runs darcy with a ten-second budget" >:: successful_campaign "darcy";
         "forwards duration and ownership opt-out"
         >:: duration_and_ownership_are_forwarded;
         "uses --export-spec-json" >:: uses_export_spec_json;
         "forwards seed zero" >:: seed_is_forwarded "0";
         "forwards the largest seed" >:: seed_is_forwarded "18446744073709551615";
         "rejects an incorrect reported seed" >:: incorrect_reported_seed_is_refused;
         "rejects a negative seed" >:: invalid_seed_is_refused "-1";
         "rejects an overflowing seed" >:: invalid_seed_is_refused "18446744073709551616";
         "rejects a noninteger seed" >:: invalid_seed_is_refused "not-a-seed";
         "reports a confirmed target crash" >:: finding_is_reported 0;
         "reports a confirmed postcondition failure" >:: finding_is_reported 1;
         "requires a report after success" >:: missing_report 0;
         "requires a report after a finding status" >:: missing_report 1;
         "reports a build failure without a report" >:: missing_report 2;
         "never reuses an earlier report" >:: stale_report_is_not_reused;
         "preserves interruption status" >:: child_status 130 130;
         "preserves SIGINT" >:: child_status ~signal:"INT" 0 130;
         "forwards parent SIGINT and reaps Austen"
         >:: parent_interruption_is_forwarded_and_reaped;
         "forwards SIGINT to Austen descendants" >:: interruption_reaches_descendants;
         "preserves build-only SIGINT" >:: build_interruption_is_preserved;
         "reports unexpected child signals" >:: child_status ~signal:"TERM" 0 2;
         "reports an unexpected child exit" >:: child_status 3 2;
         "rejects a zero duration" >:: invalid_duration "0";
         "rejects a negative duration" >:: invalid_duration "-1";
         "reports a preprocessing failure"
         >:: preparation_failure "#error deliberate preprocessing failure\n";
         "reports a specification type error"
         >:: preparation_failure
               "int decrement(int x)\n/*@ ensures return == true; @*/\n{ return x; }\n";
         "reports an invalid source extension" >:: invalid_source_extension;
         "preserves specifications at a conflicting report path"
         >:: report_cannot_replace_spec;
         "reports missing Austen" >:: missing_austen;
         "reports unspawnable Austen" >:: unspawnable_austen;
         "rejects an unknown root" >:: invalid_selection [ "--only=absent" ];
         "rejects an empty selection"
         >:: invalid_selection [ "--only=decrement"; "--skip=decrement" ];
         "rejects several selected roots" >:: invalid_selection [];
         "rejects a finding report with exit zero"
         >:: rejected_report (Yojson.Safe.to_string (report ~confirmed:1 ()));
         "rejects a clean report with exit one"
         >:: rejected_report ~exit_code:1 (Yojson.Safe.to_string (report ()));
         "rejects a completed report with failure exit"
         >:: rejected_report ~exit_code:2 (Yojson.Safe.to_string (report ()))
       ]
       @ List.map
           (fun (name, contents) -> name >:: rejected_report contents)
           malformed_reports
