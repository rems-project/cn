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


let suite =
  "spec export"
  >::: [ "collects referenced symbols" >:: referenced_symbol_is_found;
         "serializes qualifiers" >:: qualifiers_match_austen_serde;
         "serializes exact storage types" >:: exact_storage_types_match_austen_serde;
         "selects linkage owners" >:: linkage_and_owner_selection_are_deterministic;
         "rejects unsupported objects" >:: unsupported_storage_objects_are_refused
       ]
