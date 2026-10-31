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


let json_list = function
  | `List items -> items
  | json -> assert_failure ("expected a list, got " ^ Yojson.Safe.to_string json)


let rec json_nodes json =
  json
  ::
  (match json with
   | `Assoc fields -> List.concat_map (fun (_, value) -> json_nodes value) fields
   | `List items -> List.concat_map json_nodes items
   | _ -> [])


let has_variant name = function
  | `Assoc [ (tag, _) ] -> String.equal name tag
  | _ -> false


let named_export kind name exported =
  json_field kind exported
  |> json_list
  |> List.find (fun item ->
    let candidate = json_field "name" item in
    Yojson.Safe.equal candidate (`String name)
    ||
    match candidate with
    | `Assoc fields ->
      Option.equal Yojson.Safe.equal (List.assoc_opt "name" fields) (Some (`String name))
    | _ -> false)


let bits sign width = `Assoc [ ("Bits", `List [ `String sign; `Int width ]) ]

let computational_binding fn =
  match json_field "internal" fn with
  | `Assoc [ ("Computational", `List [ `List [ symbol; bt ]; _ ]) ] -> (symbol, bt)
  | json ->
    assert_failure ("expected a computational argument, got " ^ Yojson.Safe.to_string json)


let integer_alias raw chain =
  List.find_map
    (function
      | `Assoc [ ("Define", `List [ `List [ symbol; value ]; _ ]) ] ->
        (match json_field "term" value with
         | `Assoc [ ("Cast", `List [ `String "Integer"; operand ]) ]
           when Yojson.Safe.equal (json_field "term" operand) (`Assoc [ ("Sym", raw) ]) ->
           Some symbol
         | _ -> None)
      | _ -> None)
    (json_nodes chain)


(* Every widths test only inspects the export, so run it once. *)
let widths_export = lazy (run_export "CN_EXPORT_WIDTHS_FIXTURE")

let scalar_boundaries_keep_integer_logic _ =
  let exported = Lazy.force widths_export in
  List.iter
    (fun (name, expected) ->
       let fn = named_export "functions" name exported in
       let raw, actual = computational_binding fn in
       assert_equal ~msg:name ~printer:Yojson.Safe.to_string expected actual;
       let logical =
         match integer_alias raw (json_field "internal" fn) with
         | Some symbol -> symbol
         | None -> assert_failure (name ^ " has no integer alias of its raw argument")
       in
       assert_bool
         "the raw and logical binders must be distinct"
         (not (Yojson.Safe.equal raw logical));
       assert_bool
         "the original logical argument must remain available to the postcondition"
         (List.mem (`Assoc [ ("Sym", logical) ]) (json_nodes (json_field "ensures" fn)));
       List.iter
         (function
           | `Assoc fields
             when Option.equal
                    Yojson.Safe.equal
                    (List.assoc_opt "term" fields)
                    (Some (`Assoc [ ("Sym", raw) ])) ->
             assert_equal
               ~msg:"every raw-symbol occurrence must retain its finite-width type"
               ~printer:Yojson.Safe.to_string
               expected
               (List.assoc "bt" fields)
           | _ -> ())
         (json_nodes fn))
    [ ("signed_echo", bits "Signed" 8);
      ("unsigned_echo", bits "Unsigned" 64);
      ("boolean_echo", bits "Unsigned" 8)
    ];
  let boolean = named_export "functions" "boolean_echo" exported in
  let internal = json_field "internal" boolean in
  let raw, _ = computational_binding boolean in
  let logical = Option.get (integer_alias raw internal) in
  let logical_term =
    `Assoc [ ("term", `Assoc [ ("Sym", logical) ]); ("bt", `String "Integer") ]
  in
  let integer sign limbs =
    `Assoc
      [ ("term", `Assoc [ ("Const", `Assoc [ ("Z", `List [ `Int sign; `List limbs ]) ]) ]);
        ("bt", `String "Integer")
      ]
  in
  let le left right = `Assoc [ ("Binop", `List [ `String "Le"; left; right ]) ] in
  assert_bool
    "C Boolean inputs must remain restricted to zero or one"
    (List.mem (le (integer 0 []) logical_term) (json_nodes internal)
     && List.mem (le logical_term (integer 1 [ `Int 1 ])) (json_nodes internal))


let owned_boundaries_keep_predicate_values_integer _ =
  let exported = Lazy.force widths_export in
  let predicate = named_export "predicates" "WidthCell" exported in
  assert_equal (`String "Integer") (json_field "oarg" predicate);
  let used_values =
    `List [ predicate; named_export "functions" "owned_increment" exported ]
  in
  let bindings =
    List.filter_map
      (function
        | `Assoc
            [ ( "Resource",
                `List
                  [ `List [ symbol; `List [ `Assoc [ ("P", `Assoc request) ]; bt ] ];
                    rest
                  ] )
            ] ->
          (match List.assoc "name" request with
           | `Assoc
               [ ( "Owned",
                   `List
                     [ `Assoc [ ("Integer", `Assoc [ ("Signed", `String "Int") ]) ];
                       init
                     ] )
               ] ->
             Some (symbol, bt, init, rest)
           | _ -> None)
        | _ -> None)
      (json_nodes used_values)
  in
  assert_bool
    "the regression must exercise scalar owned storage"
    (List.length bindings > 0);
  List.iter
    (fun (raw, bt, init, rest) ->
       assert_equal ~printer:Yojson.Safe.to_string (bits "Signed" 32) bt;
       if Yojson.Safe.equal init (`String "Init") then
         assert_bool
           "logically used initialized owned values must be converted to mathematical \
            integers"
           (List.exists
              (function
                | `Assoc [ ("Cast", `List [ `String "Integer"; operand ]) ] ->
                  Yojson.Safe.equal (json_field "term" operand) (`Assoc [ ("Sym", raw) ])
                | _ -> false)
              (json_nodes rest)))
    bindings


let aggregate_logic_uses_records_and_finite_maps _ =
  let exported = Lazy.force widths_export in
  assert_equal 16 (List.length (json_list (json_field "functions" exported)));
  let echo = named_export "functions" "aggregate_echo" exported in
  assert_bool
    "the ABI argument must retain its C struct type"
    (has_variant "Struct" (snd (computational_binding echo)));
  let predicate = named_export "predicates" "WidthBlock" exported in
  assert_bool
    "the predicate result must use the logical record representation"
    (has_variant "Record" (json_field "oarg" predicate));
  let update = named_export "logical_functions" "logical_update" exported in
  (match json_list (json_field "args" update) with
   | [ `List [ _; bt ] ] ->
     assert_bool "logical function inputs must use records" (has_variant "Record" bt)
   | _ -> assert_failure "expected the logical struct update's single argument");
  assert_equal (`String "Integer") (json_field "return_bt" update);
  assert_bool
    "logical struct updates must become record updates"
    (List.exists (has_variant "RecordUpdate") (json_nodes update));
  assert_bool
    "aggregate conversion must not introduce unsupported map comprehensions"
    (not (List.exists (has_variant "MapDef") (json_nodes exported)));
  assert_bool
    "array conversions must produce mathematical integer keys and values"
    (List.mem
       (`Assoc [ ("Map", `List [ `String "Integer"; `String "Integer" ]) ])
       (json_nodes exported));
  let types = json_field "types" exported in
  assert_bool
    "the original fixed-size C array layout must remain available to Austen"
    (List.exists
       (function
         | `Assoc fields ->
           Option.equal
             Yojson.Safe.equal
             (List.assoc_opt "kind" fields)
             (Some (`String "array"))
           && Option.equal
                Yojson.Safe.equal
                (List.assoc_opt "length" fields)
                (Some (`Int 2))
         | _ -> false)
       (json_nodes types))


let uninitialized_outputs_do_not_read_an_absent_value _ =
  let exported = Lazy.force widths_export in
  let fn = named_export "functions" "uninitialized_output" exported in
  let body = json_field "body" (json_field "ensures" fn) in
  assert_bool
    "the postcondition still carries its uninitialized ownership request"
    (List.exists
       (function
         | `Assoc [ ("Owned", `List [ _; `String "Uninit" ]) ] -> true | _ -> false)
       (json_nodes body));
  assert_bool
    "unused uninitialized values must not acquire an eager conversion"
    (not (List.exists (has_variant "Define") (json_nodes body)))


let unused_aggregate_keeps_storage_checks _ =
  let exported = Lazy.force widths_export in
  let fn = named_export "functions" "unused_aggregate" exported in
  let nodes = json_nodes fn in
  assert_bool
    "unused aggregate values must not introduce logical array converters"
    (not (List.exists (has_variant "Q") nodes));
  assert_bool
    "unused aggregate values need no logical alias"
    (not (List.exists (has_variant "Define") nodes));
  assert_bool
    "raw representability must project the original C struct"
    (List.exists (has_variant "StructMember") nodes);
  assert_bool
    "storage checks must not use logical record projections"
    (not (List.exists (has_variant "RecordMember") nodes));
  let arrays =
    List.filter_map
      (function
        | `Assoc [ ("EachI", `Assoc fields) ] -> Some (List.assoc "var" fields)
        | _ -> None)
      nodes
  in
  assert_bool "the regression must check array element values" (List.length arrays > 0);
  List.iter
    (function
      | `List [ _; typ ] ->
        assert_equal
          ~msg:"raw arrays retain finite storage indices"
          ~printer:Yojson.Safe.to_string
          (bits "Unsigned" 64)
          typ
      | _ -> assert_failure "expected a typed array-check binder")
    arrays;
  let is_raw_one term =
    Yojson.Safe.equal (json_field "bt" term) (bits "Unsigned" 8)
    && Yojson.Safe.equal
         (json_field "term" term)
         (`Assoc
             [ ( "Const",
                 `Assoc
                   [ ( "Bits",
                       `List
                         [ `List [ `String "Unsigned"; `Int 8 ];
                           `List [ `Int 1; `List [ `Int 1 ] ]
                         ] )
                   ] )
             ])
  in
  assert_bool
    "unused nested Boolean values must still be restricted to zero or one"
    (List.exists
       (function
         | `Assoc [ ("Binop", `List [ `String "Le"; _; right ]) ] -> is_raw_one right
         | _ -> false)
       nodes)


let boundary_exports_are_deterministic _ =
  let first = Lazy.force widths_export in
  let second = run_export "CN_EXPORT_WIDTHS_FIXTURE" in
  assert_equal ~printer:Yojson.Safe.to_string first second;
  let next =
    match json_field "next_sym" first with
    | `Int next -> next
    | _ -> assert_failure "next_sym must be an integer"
  in
  List.iter
    (function
      | `Assoc fields ->
        (match List.assoc_opt "num" fields with
         | Some (`Int number) ->
           assert_bool "next_sym must follow every generated symbol" (number < next)
         | _ -> ())
      | _ -> ())
    (json_nodes first)


let assert_contains ~needle haystack =
  let n = String.length needle in
  let rec go i =
    i + n <= String.length haystack
    && (String.equal (String.sub haystack i n) needle || go (i + 1))
  in
  assert_bool (Printf.sprintf "expected %S in %S" needle haystack) (go 0)


let reading_uninitialized_outputs_is_diagnosed _ =
  let output = Filename.temp_file "cn-uninit-export" ".json" in
  let diagnostics = Filename.temp_file "cn-uninit-diagnostic" ".txt" in
  let cleanup () =
    List.iter
      (fun path -> if Sys.file_exists path then Sys.remove path)
      [ output; diagnostics ]
  in
  Fun.protect ~finally:cleanup (fun () ->
    let stderr_fd = Unix.openfile diagnostics [ Unix.O_WRONLY; Unix.O_TRUNC ] 0 in
    let status =
      Fun.protect
        ~finally:(fun () -> Unix.close stderr_fd)
        (fun () -> run_export_process ~stderr_fd "CN_UNINIT_VALUE_FIXTURE" output)
    in
    assert_equal ~printer:string_of_process_status (Unix.WEXITED 0) status;
    let exported = Yojson.Safe.from_file output in
    assert_equal
      ~msg:"only the unsupported function should be omitted"
      [ `String "unaffected" ]
      (List.map (json_field "name") (json_list (json_field "functions" exported)));
    assert_contains
      ~needle:"reading an uninitialized Owned value"
      (In_channel.with_open_bin diagnostics In_channel.input_all))


module L = Export.Lower
module BT = Cn.BaseTypes
module T = Cn.Terms
module LC = Cn.LogicalConstraints
module LAT = Cn.LogicalArgumentTypes
module RT = Cn.ReturnTypes
module LRT = Cn.LogicalReturnTypes

let cast bt x = Cn.MakeTerm.cast_ bt x L.loc

let bin op a b = L.term BT.Bool (T.Binop (op, a, b))

let integer n = Cn.MakeTerm.int_ n L.loc

let equality_projection_preserves_scalar_ranges _ =
  let x = Sym.fresh "x" in
  let raw = L.sym (Sym.fresh "raw") (BT.Bits (BT.Signed, 8)) in
  let views = L.bind_view Sym.Map.empty x (cast BT.Integer raw) in
  let value = L.sym x BT.Integer in
  let equality left right = LC.T (bin T.EQ left right) in
  List.iter
    (fun n ->
       match L.equality_projection views (equality value (integer n)) with
       | Some
           (LC.T
              (T.IT
                 ( T.Binop
                     (T.EQ, _, T.IT (T.Const (T.Bits ((BT.Signed, 8), literal)), _, _)),
                   _,
                   _ ))) ->
         assert_bool
           "the projected literal must preserve its integer value"
           (Z.equal (Z.of_int n) literal)
       | _ -> assert_failure "expected a signed byte equality projection")
    [ -128; -1; 0; 127 ];
  List.iter
    (fun n ->
       assert_equal None (L.equality_projection views (equality value (integer n))))
    [ -129; 128 ];
  assert_bool
    "constant equality is projected in either operand order"
    (Option.is_some (L.equality_projection views (equality (integer 10) value)));
  List.iter
    (fun (typ, supported) ->
       let y = Sym.fresh "y" in
       let raw_y = L.sym (Sym.fresh "raw_y") typ in
       let views = L.bind_view views y (cast BT.Integer raw_y) in
       assert_equal
         ~msg:"only the same storage signedness and width may share a raw equality"
         supported
         (Option.is_some
            (L.equality_projection views (equality value (L.sym y BT.Integer)))))
    [ (BT.Bits (BT.Signed, 8), true);
      (BT.Bits (BT.Unsigned, 8), false);
      (BT.Bits (BT.Signed, 16), false);
      (BT.Bool, false)
    ]


let equality_projection_respects_scope_and_logic _ =
  let x = Sym.fresh "x" in
  let raw_symbol = Sym.fresh "raw" in
  let raw = L.sym raw_symbol (BT.Bits (BT.Signed, 32)) in
  let converted = cast BT.Integer raw in
  let views = L.bind_view Sym.Map.empty x converted in
  let value = L.sym x BT.Integer in
  let equal = bin T.EQ value (integer 10) in
  let constraint_ = LC.T equal in
  let arithmetic = L.term BT.Integer (T.Binop (T.Add, value, integer 1)) in
  assert_equal
    None
    (L.equality_projection views (LC.T (bin T.EQ arithmetic (integer 11))));
  assert_equal
    None
    (L.equality_projection views (LC.T (bin T.Or equal (Cn.MakeTerm.bool_ true L.loc))));
  assert_equal None (L.equality_projection views (LC.T (bin T.LT value (integer 10))));
  assert_equal None (L.equality_projection (L.bind_view views x (integer 10)) constraint_);
  assert_equal None (L.equality_projection (L.forget_view raw_symbol views) constraint_);
  let alias = Sym.fresh "alias" in
  let aliases = L.bind_view views alias value in
  assert_bool
    "an unchanged scalar alias retains its raw view"
    (Option.is_some
       (L.equality_projection aliases (LC.T (bin T.EQ (L.sym alias BT.Integer) value))));
  let chain =
    LAT.Define
      ( (x, converted),
        L.info,
        LAT.Constraint
          ( constraint_,
            L.info,
            LAT.Define
              ((x, integer 10), L.info, LAT.Constraint (constraint_, L.info, LAT.I ())) )
      )
  in
  match L.project_lat (fun _ () -> ()) Sym.Map.empty chain with
  | LAT.Define
      ( _,
        _,
        LAT.Constraint
          ( _,
            _,
            LAT.Constraint
              (original, _, LAT.Define (_, _, LAT.Constraint (shadowed, _, LAT.I ()))) )
      ) ->
    assert_bool "the original integer equality remains" (LC.equal original constraint_);
    assert_bool
      "a shadowed binding gets no stale raw equality"
      (LC.equal shadowed constraint_)
  | _ ->
    assert_failure "expected one extra equality before the original, with lexical scope"


let equality_projection_reaches_postconditions _ =
  let x = Sym.fresh "x" in
  let raw = L.sym (Sym.fresh "raw") (BT.Bits (BT.Signed, 32)) in
  let views = L.bind_view Sym.Map.empty x (cast BT.Integer raw) in
  let equality = LC.T (bin T.EQ (L.sym x BT.Integer) (integer 10)) in
  let body = LRT.Constraint (equality, L.info, LRT.I) in
  let result = RT.Computational ((Sym.fresh "return", BT.Integer), L.info, body) in
  (match L.project_return views result with
   | RT.Computational (_, _, LRT.Constraint (_, _, LRT.Constraint (original, _, LRT.I)))
     ->
     assert_bool
       "postconditions retain their integer equality"
       (LC.equal original equality)
   | _ -> assert_failure "an entry alias must remain available in the postcondition");
  let shadowed = RT.Computational ((x, BT.Integer), L.info, body) in
  match L.project_return views shadowed with
  | RT.Computational (_, _, LRT.Constraint (_, _, LRT.I)) -> ()
  | _ -> assert_failure "the return binder must shadow an entry view with the same symbol"


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
         "requires an output path" >:: output_path_is_required;
         "keeps finite scalar boundaries and mathematical logic"
         >:: scalar_boundaries_keep_integer_logic;
         "keeps owned values finite and predicate results mathematical"
         >:: owned_boundaries_keep_predicate_values_integer;
         "lowers aggregate logic without changing C layouts"
         >:: aggregate_logic_uses_records_and_finite_maps;
         "does not read unused uninitialized outputs"
         >:: uninitialized_outputs_do_not_read_an_absent_value;
         "keeps unused aggregate checks on C storage values"
         >:: unused_aggregate_keeps_storage_checks;
         "exports boundary adapters deterministically"
         >:: boundary_exports_are_deterministic;
         "diagnoses reads of uninitialized outputs"
         >:: reading_uninitialized_outputs_is_diagnosed;
         "projects equalities only within the same scalar range"
         >:: equality_projection_preserves_scalar_ranges;
         "keeps equality projections scoped and preserves integer logic"
         >:: equality_projection_respects_scope_and_logic;
         "preserves entry scalar views for postcondition equalities"
         >:: equality_projection_reaches_postconditions
       ]
