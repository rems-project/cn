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


let run_export_process
      ?(stdout_fd = Unix.stdout)
      ?(stderr_fd = Unix.stderr)
      fixture_env
      path
  =
  let cn = Sys.getenv "CN_TEST_EXE" in
  let source = Sys.getenv fixture_env in
  let argv =
    [| cn; "test"; "bennet"; "--no-run"; "--export-spec-json=" ^ path; source |]
  in
  let pid = Unix.create_process cn argv Unix.stdin stdout_fd stderr_fd in
  let _, status = Unix.waitpid [] pid in
  status


let run_export fixture_env =
  let path = Filename.temp_file "cn-spec-export" ".json" in
  Sys.remove path;
  let cleanup () = if Sys.file_exists path then Sys.remove path in
  Fun.protect ~finally:cleanup (fun () ->
    let status = run_export_process fixture_env path in
    assert_equal ~printer:string_of_process_status (Unix.WEXITED 0) status;
    assert_bool "the exporter did not write its JSON file" (Sys.file_exists path);
    Yojson.Safe.from_file path)


let json_field name = function
  | `Assoc fields -> List.assoc name fields
  | json -> assert_failure ("expected an object, got " ^ Yojson.Safe.to_string json)


let exported_functions fixture_env =
  match json_field "functions" (run_export fixture_env) with
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
  let names =
    exported_functions "CN_MIXED_EXPORT_FIXTURE"
    |> List.map (fun fn -> json_field "name" fn)
  in
  assert_equal
    [ `String "static_mixed_helper";
      `String "specified";
      `String "identity";
      `String "main"
    ]
    names


let early_export_failure_uses_test_error_handler _ =
  let invalid_parent = Filename.temp_file "cn-spec-export-parent" "" in
  let null_fd = Unix.openfile Filename.null [ Unix.O_WRONLY ] 0 in
  let cleanup () =
    Unix.close null_fd;
    if Sys.file_exists invalid_parent then Sys.remove invalid_parent
  in
  Fun.protect ~finally:cleanup (fun () ->
    let status =
      run_export_process
        ~stdout_fd:null_fd
        ~stderr_fd:null_fd
        "CN_ENUM_ARGUMENT_FIXTURE"
        (Filename.concat invalid_parent "spec.json")
    in
    assert_equal ~printer:string_of_process_status (Unix.WEXITED 1) status)


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
         "reports early export failures through the test driver"
         >:: early_export_failure_uses_test_error_handler
       ]
