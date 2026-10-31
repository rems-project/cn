open OUnit2
module Export = Cn.TestGeneration.Private.SpecExport
module Sym = Cn.Sym

let referenced_symbol_is_found _ =
  let target = Sym.fresh "target" in
  let other = Sym.fresh "other" in
  let json =
    `Assoc
      [ ( "outer",
          `List
            [ Export.json_of_sym other; `Assoc [ ("term", Export.json_of_sym target) ] ]
        )
      ]
  in
  assert_bool
    "nested target symbol should be found"
    (Export.json_mentions_sym target json);
  assert_bool
    "different symbol should not match"
    (not (Export.json_mentions_sym (Sym.fresh "absent") json))


let qualifiers_match_austen_serde _ =
  let qualifiers : Cn.Sctypes.Qualifiers.t =
    { const = false; restrict = true; volatile = false }
  in
  assert_equal
    ~printer:Yojson.Safe.to_string
    (`Assoc
        [ ("const_", `Bool false); ("restrict", `Bool true); ("volatile", `Bool false) ])
    (Export.json_of_quals qualifiers)


let exact_storage_types_match_austen_serde _ =
  let open Cn.Sctypes in
  assert_equal
    ~printer:Yojson.Safe.to_string
    (`Assoc
        [ ( "Array",
            `List
              [ `Assoc [ ("Integer", `Assoc [ ("Unsigned", `String "LongLong") ]) ];
                `Int 3
              ] )
        ])
    (Export.json_of_sct (Array (Integer (Unsigned LongLong), Some 3)))


let linkage_and_owner_selection_are_deterministic _ =
  assert_equal
    ~printer:Yojson.Safe.to_string
    (`Assoc [ ("kind", `String "external") ])
    (Export.json_of_global_linkage ~is_external:true ~owner:None ~c_name:"shared");
  assert_equal
    ~printer:Yojson.Safe.to_string
    (`Assoc [ ("kind", `String "internal"); ("owner", `String "first") ])
    (Export.json_of_global_linkage
       ~is_external:false
       ~owner:(Some "first")
       ~c_name:"local");
  assert_equal
    (Some "first")
    (Export.first_non_static_owner
       [ (true, "static_one"); (false, "first"); (false, "second") ]);
  assert_raises
    (Export.Unrepresentable
       "internal global local has no exported non-static owner function")
    (fun () ->
       ignore
         (Export.json_of_global_linkage ~is_external:false ~owner:None ~c_name:"local"))


let unsupported_storage_objects_are_refused _ =
  let open Cn.Sctypes in
  let no_tag _ = None in
  List.iter
    (fun (ct, reason) ->
       assert_raises (Export.Unrepresentable reason) (fun () ->
         Export.validate_global_type_with no_tag ct))
    [ (Void, "a void global");
      (Array (Integer (Signed Int_), None), "an incomplete global array");
      (Array (Integer (Signed Int_), Some 0), "a zero-sized global array")
    ]


let string_of_process_status = function
  | Unix.WEXITED code -> "exited " ^ string_of_int code
  | Unix.WSIGNALED signal -> "signaled " ^ string_of_int signal
  | Unix.WSTOPPED signal -> "stopped " ^ string_of_int signal


(* The environment with every `PATH` entry that provides `austen` removed, so
   the export is shown not to depend on Austen being installed, and with
   [path_prefix] (if any) put first on `PATH`. *)
let environment_without_austen ?path_prefix () =
  let without_austen path =
    String.split_on_char ':' path
    |> List.filter (fun dir ->
      (not (String.equal dir "")) && not (Sys.file_exists (Filename.concat dir "austen")))
    |> String.concat ":"
  in
  let prefix = "PATH=" in
  Array.map
    (fun binding ->
       let n = String.length prefix in
       if String.starts_with ~prefix binding then
         prefix
         ^ Option.fold ~none:"" ~some:(fun dir -> dir ^ ":") path_prefix
         ^ without_austen (String.sub binding n (String.length binding - n))
       else
         binding)
    (Unix.environment ())


(* Run `cn ARGS` in [env] (by default, without Austen) and wait for it. *)
let run_cn ?env ?(stdout_fd = Unix.stdout) ?(stderr_fd = Unix.stderr) args =
  let cn = Sys.getenv "CN_TEST_EXE" in
  let env = match env with Some env -> env | None -> environment_without_austen () in
  let pid =
    Unix.create_process_env
      cn
      (Array.of_list (cn :: args))
      env
      Unix.stdin
      stdout_fd
      stderr_fd
  in
  let _, status = Unix.waitpid [] pid in
  status


let run_cn_test_export ?stdout_fd ?stderr_fd args =
  run_cn ?stdout_fd ?stderr_fd ("test" :: "export" :: args)


let with_null_fd f =
  let null_fd = Unix.openfile Filename.null [ Unix.O_WRONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close null_fd) (fun () -> f null_fd)


let short_output path = [ "-o"; path ]

let run_export_process
      ?stdout_fd
      ?stderr_fd
      ?(output_args = short_output)
      fixture_env
      path
  =
  run_cn_test_export ?stdout_fd ?stderr_fd (Sys.getenv fixture_env :: output_args path)


let run_export ?output_args fixture_env =
  let path = Filename.temp_file "cn-spec-export" ".json" in
  Sys.remove path;
  let cleanup () = if Sys.file_exists path then Sys.remove path in
  Fun.protect ~finally:cleanup (fun () ->
    let status = run_export_process ?output_args fixture_env path in
    assert_equal ~printer:string_of_process_status (Unix.WEXITED 0) status;
    assert_bool "the exporter did not write its JSON file" (Sys.file_exists path);
    Yojson.Safe.from_file path)


let json_field name = function
  | `Assoc fields -> List.assoc name fields
  | json -> assert_failure ("expected an object, got " ^ Yojson.Safe.to_string json)


let exported_functions ?output_args fixture_env =
  match json_field "functions" (run_export ?output_args fixture_env) with
  | `List functions -> functions
  | json ->
    assert_failure ("expected exported functions, got " ^ Yojson.Safe.to_string json)


let one_exported_function fixture_env =
  match exported_functions fixture_env with
  | [ fn ] -> fn
  | functions ->
    assert_failure
      ("expected exactly one exported function, got "
       ^ Yojson.Safe.to_string (`List functions))


let assert_integer_ctype json = assert_equal (`String "int") (json_field "kind" json)

let enum_argument_function_is_exported _ =
  let fn = one_exported_function "CN_ENUM_ARGUMENT_FIXTURE" in
  assert_equal (`String "identity") (json_field "name" fn);
  (match json_field "params" fn with
   | `List [ param ] -> assert_integer_ctype (json_field "type" param)
   | json ->
     assert_failure ("expected one exported parameter, got " ^ Yojson.Safe.to_string json));
  assert_integer_ctype (json_field "return" fn)


let enum_return_function_is_exported _ =
  let fn = one_exported_function "CN_ENUM_RETURN_FIXTURE" in
  assert_equal (`String "identity") (json_field "name" fn);
  assert_equal (`List []) (json_field "params" fn);
  assert_integer_ctype (json_field "return" fn)


let untestable_functions_join_testable_ones _ =
  let functions =
    exported_functions
      ~output_args:(fun path -> [ "--output=" ^ path ])
      "CN_MIXED_EXPORT_FIXTURE"
  in
  let names = List.map (fun fn -> json_field "name" fn) functions in
  assert_equal
    [ `String "helper"; `String "specified"; `String "identity"; `String "main" ]
    names;
  let helper = List.hd functions in
  assert_equal
    ~printer:Yojson.Safe.to_string
    (`String "internal")
    (json_field "linkage" helper);
  let main = List.nth functions 3 in
  assert_equal
    ~printer:Yojson.Safe.to_string
    (`String "external")
    (json_field "linkage" main)


let early_export_failure_uses_export_error_handler _ =
  let invalid_parent = Filename.temp_file "cn-spec-export-parent" "" in
  let cleanup () = if Sys.file_exists invalid_parent then Sys.remove invalid_parent in
  Fun.protect ~finally:cleanup (fun () ->
    with_null_fd
    @@ fun null_fd ->
    let status =
      run_export_process
        ~stdout_fd:null_fd
        ~stderr_fd:null_fd
        "CN_ENUM_ARGUMENT_FIXTURE"
        (Filename.concat invalid_parent "spec.json")
    in
    assert_equal ~printer:string_of_process_status (Unix.WEXITED 1) status)


let output_path_is_required _ =
  with_null_fd
  @@ fun null_fd ->
  let status =
    run_cn_test_export
      ~stdout_fd:null_fd
      ~stderr_fd:null_fd
      [ Sys.getenv "CN_MIXED_EXPORT_FIXTURE" ]
  in
  assert_equal ~printer:string_of_process_status (Unix.WEXITED 124) status


let suite =
  "spec export"
  >::: [ "collects referenced symbols" >:: referenced_symbol_is_found;
         "serializes qualifiers" >:: qualifiers_match_austen_serde;
         "serializes exact storage types" >:: exact_storage_types_match_austen_serde;
         "selects linkage owners" >:: linkage_and_owner_selection_are_deterministic;
         "rejects unsupported objects" >:: unsupported_storage_objects_are_refused;
         "exports a spec-less enum-argument function"
         >:: enum_argument_function_is_exported;
         "exports a spec-less enum-return function" >:: enum_return_function_is_exported;
         "exports untestable functions beside testable functions"
         >:: untestable_functions_join_testable_ones;
         "reports early export failures through the export command"
         >:: early_export_failure_uses_export_error_handler;
         "requires an output path" >:: output_path_is_required
       ]
