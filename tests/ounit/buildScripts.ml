open OUnit2

let read_file path =
  let channel = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))


let rec remove_tree path =
  if Sys.is_directory path then (
    Sys.readdir path |> Array.iter (fun entry -> remove_tree (Filename.concat path entry));
    Unix.rmdir path)
  else
    Sys.remove path


let with_temp_dir f =
  let path = Filename.temp_file "cn-build-script-" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> f path)


let create_runtime_dir prefix =
  let lib_dir = Filename.concat prefix "lib" in
  let cn_dir = Filename.concat lib_dir "cn" in
  Unix.mkdir lib_dir 0o700;
  Unix.mkdir cn_dir 0o700;
  Unix.mkdir (Filename.concat cn_dir "runtime") 0o700


let write_executable path contents =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel contents);
  Unix.chmod path 0o700


let count_substring ~needle haystack =
  let rec loop count offset =
    match String.index_from_opt haystack offset needle.[0] with
    | None -> count
    | Some index ->
      let matches =
        index + String.length needle <= String.length haystack
        && String.equal (String.sub haystack index (String.length needle)) needle
      in
      if matches then
        loop (count + 1) (index + String.length needle)
      else
        loop count (index + 1)
  in
  loop 0 0


let generate_script ~cc ~coverage output_dir =
  let config = { Cn.TestGeneration.default_cfg with cc; coverage } in
  Cn.TestGeneration.set_config config;
  Fun.protect
    ~finally:(fun () -> Cn.TestGeneration.set_config Cn.TestGeneration.default_cfg)
    (fun () ->
       Cn.TestGeneration.Private.BuildScripts.generate_and_save
         ~output_dir
         ~filename:"fixture.c"
         Cn.TestGeneration.Config.Bash);
  Filename.concat output_dir "run_tests.sh"


let command_status ~opam_prefix script =
  let command =
    Printf.sprintf
      "OPAM_SWITCH_PREFIX=%s bash %s >/dev/null 2>&1"
      (Filename.quote opam_prefix)
      (Filename.quote script)
  in
  Sys.command command


let test_infrastructure_failures_use_distinct_status _ =
  with_temp_dir (fun output_dir ->
    create_runtime_dir output_dir;
    let script = generate_script ~cc:"false" ~coverage:true output_dir in
    let contents = read_file script in
    assert_equal ~printer:string_of_int 7 (count_substring ~needle:"exit 2" contents);
    assert_equal 0 (count_substring ~needle:"exit 1" contents);
    assert_bool
      "the generated test result remains the script's final status"
      (count_substring ~needle:"exit $test_exit_code" contents = 1);
    assert_equal ~printer:string_of_int 2 (command_status ~opam_prefix:output_dir script))


let test_expected_test_failure_preserves_status_one _ =
  with_temp_dir (fun output_dir ->
    create_runtime_dir output_dir;
    let fake_cc = Filename.concat output_dir "fake-cc" in
    write_executable
      fake_cc
      {bash|#!/bin/bash
output=
while (( $# )); do
  if [[ "$1" == "-o" ]]; then
    output=$2
    shift 2
  else
    shift
  fi
done
if [[ "$output" == *.o ]]; then
  : > "$output"
  exit 0
fi
if [[ "$output" == "./tests.out" ]]; then
  printf '#!/bin/bash\nexit 1\n' > "$output"
  chmod +x "$output"
  exit 0
fi
exit 2
|bash};
    let script = generate_script ~cc:fake_cc ~coverage:false output_dir in
    assert_equal ~printer:string_of_int 1 (command_status ~opam_prefix:output_dir script))


let test_missing_runtime_stops_parent_script _ =
  with_temp_dir (fun output_dir ->
    let script = generate_script ~cc:"true" ~coverage:false output_dir in
    assert_equal ~printer:string_of_int 2 (command_status ~opam_prefix:output_dir script))


let suite =
  "Generated Bash build scripts"
  >::: [ "infrastructure failures use status 2"
         >:: test_infrastructure_failures_use_distinct_status;
         "expected test failures preserve status 1"
         >:: test_expected_test_failure_preserves_status_one;
         "missing runtime exits the parent script"
         >:: test_missing_runtime_stops_parent_script
       ]
