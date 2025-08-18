module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module CtA = Fulminate.Cn_to_ail
module ESpecInternal = Fulminate.Internal
module Records = Fulminate.Records
module FExtract = Fulminate.Extract
module Config = TestGenConfig
module Options = Config.Options
module Cn_to_ail = Fulminate.Cn_to_ail

module Private = struct
  module Bennet = Bennet
end

type config = Config.t

let default_cfg : config = Config.default

let set_config = Config.initialize

let filename_base fn = fn |> Filename.basename |> Filename.remove_extension

let compile_assumes
      ~(without_ownership_checking : bool)
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (insts : (bool * FExtract.instrumentation) list)
  : Pp.document
  =
  let declarations, function_definitions =
    List.split
      (List.map
         (CtA.generate_assume_ownership_function ~without_ownership_checking)
         (let module CtypeSet =
            Set.Make (struct
              type t = C.ctype

              let compare a b = compare (Hashtbl.hash a) (Hashtbl.hash b)
            end)
          in
         !CtA.ownership_ctypes |> CtypeSet.of_list |> CtypeSet.to_seq |> List.of_seq)
       @ CtA.cn_to_ail_assume_predicates
           filename
           prog5.resource_predicates
           sigma.cn_datatypes
           (CtA.extract_global_variables prog5.globs)
           prog5.resource_predicates
       @ ESpecInternal.generate_c_assume_pres_internal filename insts sigma prog5)
  in
  let open Pp in
  CF.Pp_ail.(
    with_executable_spec
      (separate_map (twice hardline) (fun (tag, (_, _, decl)) ->
         pp_function_prototype tag decl))
      declarations)
  ^^ twice hardline
  ^^ CF.Pp_ail.(
       with_executable_spec
         (pp_program ~show_include:true)
         (None, { A.empty_sigma with declarations; function_definitions }))
  ^^ hardline


let compile_shape_analyzers
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (insts : (bool * Fulminate.Extract.instrumentation) list)
  : Pp.document
  =
  let declarations, function_definitions =
    BugExplanation.synthesize_shape_analyzers filename sigma prog5 insts |> List.split
  in
  let open Pp in
  CF.Pp_ail.(
    with_executable_spec
      (separate_map (twice hardline) (fun (tag, (_, _, decl)) ->
         pp_function_prototype tag decl))
      declarations)
  ^^ twice hardline
  ^^ CF.Pp_ail.(
       with_executable_spec
         (pp_program ~show_include:true)
         (None, { A.empty_sigma with declarations; function_definitions }))
  ^^ hardline


let compile_replicators
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (insts : (bool * Fulminate.Extract.instrumentation) list)
  : Pp.document
  =
  let declarations, function_definitions =
    BugExplanation.synthesize_replicators filename sigma prog5 insts |> List.split
  in
  let open Pp in
  CF.Pp_ail.(
    with_executable_spec
      (separate_map (twice hardline) (fun (tag, (_, _, decl)) ->
         pp_function_prototype tag decl))
      declarations)
  ^^ twice hardline
  ^^ CF.Pp_ail.(
       with_executable_spec
         (pp_program ~show_include:true)
         (None, { A.empty_sigma with declarations; function_definitions }))
  ^^ hardline


let pp_label ?(width : int = 30) (label : string) (doc : Pp.document) : Pp.document =
  let padding = max 2 ((width - (String.length label + 2)) / 2) in
  let width = max width (String.length label + 6) in
  let open Pp in
  if PPrint.requirement doc = 0 then
    empty
  else
    repeat width slash
    ^^ hardline
    ^^ repeat
         (if String.length label mod 2 = 1 then
            padding + 1
          else
            padding)
         slash
    ^^^ !^label
    ^^^ repeat padding slash
    ^^ hardline
    ^^ repeat width slash
    ^^ twice hardline
    ^^ doc


let compile_includes ~filename ~generators =
  let open Pp in
  !^"#include "
  ^^ angles !^"bennet/prelude.h"
  ^^ hardline
  ^^ !^"#include "
  ^^ angles !^"cn-testing/prelude.h"
  ^^ hardline
  ^^ !^"#include "
  ^^ angles !^"cn-replicate/shape.h"
  ^^ hardline
  ^^ !^"#include "
  ^^ angles !^"cn-replicate/lines.h"
  ^^ hardline
  ^^
  if generators then
    !^"#include " ^^ dquotes (string (filename_base filename ^ ".gen.h")) ^^ hardline
  else (
    let record_defs = Records.generate_all_record_strs () in
    let open Pp in
    hardline ^^ !^"/* TAG DEFINITIONS */" ^^ hardline ^^ !^record_defs ^^ twice hardline)


let compile_test (test : Test.t) =
  let open Pp in
  (match test.kind with
   | Test.Constant ->
     if test.is_static then
       !^"CN_REGISTER_STATIC_UNIT_TEST_CASE"
     else
       !^"CN_REGISTER_EXTERN_UNIT_TEST_CASE"
   | Test.RandomGenerator ->
     if test.is_static then
       !^"CN_REGISTER_STATIC_RANDOM_TEST_CASE"
     else
       !^"CN_REGISTER_EXTERN_RANDOM_TEST_CASE"
   | Test.SymbolicGenerator ->
     if test.is_static then
       !^"CN_REGISTER_STATIC_SYMBOLIC_TEST_CASE"
     else
       !^"CN_REGISTER_EXTERN_SYMBOLIC_TEST_CASE")
  ^^ parens
       (string test.suite
        ^^ comma
        ^^^ !^(test.test)
        ^^
        if test.is_static then
          comma ^^^ !^(Fulminate.Utils.static_prefix test.filename)
        else
          empty)
  ^^ semi


let compile_test_file
      ~(without_ownership_checking : bool)
      ~(filename : string)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (tests : Test.t list)
  =
  let constant_tests, generator_tests =
    List.partition (fun (test : Test.t) -> Test.equal_kind test.kind Test.Constant) tests
  in
  let constant_tests, constant_tests_defs =
    SpecTests.compile_constant_tests filename sigma constant_tests
  in
  let generator_tests, generator_tests_defs =
    SpecTests.compile_generator_tests filename sigma prog5 generator_tests
  in
  let all_tests = constant_tests @ generator_tests in
  (* Convert tests to (bool * instrumentation) format for remaining functions *)
  let insts =
    List.map (fun (test : Test.t) -> (test.is_static, Test.to_instrumentation test)) tests
  in
  (* TODO copied from fulminate.ml, put somewhere shared *)
  let open ESpecInternal in
  let c_datatype_defs = generate_c_datatypes sigma in
  let c_function_defs, c_function_decls, _c_function_locs =
    generate_c_functions filename prog5 sigma
  in
  let c_predicate_defs, c_predicate_decls, _c_predicate_locs =
    generate_c_predicates filename prog5 sigma
  in
  let conversion_function_defs, conversion_function_decls =
    generate_conversion_and_equality_functions filename sigma
  in
  let ownership_function_defs, ownership_function_decls =
    generate_ownership_functions without_ownership_checking !Cn_to_ail.ownership_ctypes
  in
  let c_struct_decls = generate_c_struct_strs sigma.tag_definitions in
  let cn_converted_struct_defs = generate_cn_versions_of_structs sigma.tag_definitions in
  let record_fun_defs, record_fun_decls = Records.generate_c_record_funs sigma in
  (* let record_defs = Records.generate_all_record_strs () in *)
  let cn_header_decls_list =
    List.concat
      [ [ "#ifndef NULL\n";
          "#include <stdlib.h>\n";
          "#endif\n";
          "#include <stdint.h>\n";
          "#include <cn-executable/utils.h>\n";
          "#include <cn-executable/cerb_types.h>\n"
        ];
        [ c_struct_decls ];
        [ (* (if not (String.equal record_defs "") then "\n/* CN RECORDS */\n\n" else ""); *)
          (*  record_defs; *)
          cn_converted_struct_defs
        ];
        (if List.is_empty c_datatype_defs then [] else [ "/* CN DATATYPES */" ]);
        List.map snd c_datatype_defs;
        [ "\n\n/* OWNERSHIP FUNCTIONS */\n\n";
          ownership_function_decls;
          conversion_function_decls;
          record_fun_decls;
          c_function_decls;
          "\n";
          c_predicate_decls
        ]
      ]
  in
  let static_wrappers_defs =
    tests
    |> List.filter (fun (test : Test.t) -> test.is_static)
    |> List.map (fun (test : Test.t) ->
      let fsym =
        Sym.fresh (Fulminate.Utils.static_prefix filename ^ "_" ^ Sym.pp_string test.fn)
      in
      let declarations =
        [ ( fsym,
            match List.assoc Sym.equal test.fn sigma.A.declarations with
            | _, _, A.Decl_function (_, ret_ct, args_ct, _, _, _) ->
              ( Locations.other __LOC__,
                CF.Annot.Attrs [],
                A.Decl_function (false, ret_ct, args_ct, false, false, false) )
            | _ -> failwith __LOC__ )
        ]
      in
      CF.Pp_ail.pp_program
        ~show_include:true
        (None, { CF.AilSyntax.empty_sigma with declarations }))
    |> Pp.(separate hardline)
  in
  let open Pp in
  !^(String.concat " " cn_header_decls_list)
  ^^ compile_includes ~filename ~generators:(List.non_empty generator_tests)
  ^^ twice hardline
  ^^ pp_label
       "Assume Ownership Functions"
       (compile_assumes ~without_ownership_checking filename sigma prog5 insts)
  ^^ pp_label "Shape Analyzers" (compile_shape_analyzers filename sigma prog5 insts)
  ^^ pp_label "Replicators" (compile_replicators filename sigma prog5 insts)
  ^^ pp_label "Static Wrappers" static_wrappers_defs
  ^^ pp_label "Constant function tests" constant_tests_defs
  ^^ pp_label "Generator-based tests" generator_tests_defs
  ^^ pp_label
       "Main function"
       (!^"int main"
        ^^ parens !^"int argc, char* argv[]"
        ^/^ braces
              (nest
                 2
                 (hardline
                  ^^ separate_map hardline compile_test all_tests
                  ^^ twice hardline
                  ^^ !^"return cn_test_main(argc, argv);")
               ^^ hardline))
  ^^ hardline
  ^^ !^(String.concat
          " "
          [ "/* RECORD */\n";
            record_fun_defs;
            "/* CONVERSION */\n";
            conversion_function_defs;
            ownership_function_defs;
            "/* CN FUNCTIONS */\n";
            c_function_defs;
            "\n";
            c_predicate_defs
          ])


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


let save_generators
      ~output_dir
      ~filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (paused : _ Typing.pause)
      (tests : Test.t list)
  : unit
  =
  let filename_base = filename |> Filename.basename |> Filename.remove_extension in
  let tests_for_generators =
    List.filter
      (fun (test : Test.t) ->
         Test.equal_kind test.kind Test.RandomGenerator
         || Test.equal_kind test.kind Test.SymbolicGenerator)
      tests
  in
  if List.non_empty tests_for_generators then (
    let generators_doc =
      Pp.(
        separate
          hardline
          ([ string (Fulminate.Globals.accessors_prototypes filename prog5) ]
           @ [ Bennet.synthesize filename sigma prog5 paused tests_for_generators ]))
    in
    let generators_fn = filename_base ^ ".gen.h" in
    save output_dir generators_fn generators_doc)


let save_tests
      ~output_dir
      ~filename
      ~without_ownership_checking
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (tests : Test.t list)
  : unit
  =
  let tests_doc =
    let open Pp in
    Bennet.test_setup ()
    ^/^ hardline
    ^^ compile_test_file ~without_ownership_checking ~filename sigma prog5 tests
  in
  save output_dir (filename_base filename ^ ".test.c") tests_doc


(** Workaround for https://github.com/rems-project/cerberus/issues/765 *)
let needs_enum_hack
      ~(with_warning : bool)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (inst : FExtract.instrumentation)
  =
  match List.assoc Sym.equal inst.fn sigma.declarations with
  | loc, _, Decl_function (_, (_, ret_ct), cts, _, _, _) ->
    if
      List.exists
        (fun (_, ct, _) ->
           match ct with C.Ctype (_, Basic (Integer (Enum _))) -> true | _ -> false)
        cts
    then (
      if with_warning then
        Cerb_colour.with_colour
          (fun () ->
             Pp.(
               warn
                 loc
                 (!^"Function"
                  ^^^ squotes (Sym.pp inst.fn)
                  ^^^ !^"has enum arguments and so could not be tested."
                  ^/^ !^"See https://github.com/rems-project/cerberus/issues/765.")))
          ();
      true)
    else if match ret_ct with C.Ctype (_, Basic (Integer (Enum _))) -> true | _ -> false
    then (
      if with_warning then
        Cerb_colour.with_colour
          (fun () ->
             Pp.(
               warn
                 loc
                 (!^"Function"
                  ^^^ squotes (Sym.pp inst.fn)
                  ^^^ !^"has an enum return type and so could not be tested."
                  ^/^ !^"See https://github.com/rems-project/cerberus/issues/765.")))
          ();
      true)
    else
      false
  | _ -> false


let functions_under_test
      ~(with_warning : bool)
      (cabs_tunit : CF.Cabs.translation_unit)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (paused : _ Typing.pause)
  : Test.t list
  =
  let insts = fst (FExtract.collect_instrumentation cabs_tunit prog5) in
  let selected_fsyms =
    Check.select_functions
      (Sym.Set.of_list
         (List.map (fun (inst : FExtract.instrumentation) -> inst.fn) insts))
  in
  insts
  |> List.filter (fun (inst : FExtract.instrumentation) ->
    CtA.has_cn_spec inst
    && (match prog5.main with
        | Some main_fn -> not (Sym.equal main_fn inst.fn)
        | None -> true)
    && Option.is_some inst.internal
    && Sym.Set.mem inst.fn selected_fsyms
    && not (needs_enum_hack ~with_warning sigma inst))
  |> List.map (Test.of_instrumentation cabs_tunit sigma paused)


let run
      ~output_dir
      ~filename
      ~without_ownership_checking
      (build_tool : TestGenConfig.build_tool)
      (cabs_tunit : CF.Cabs.translation_unit)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (paused : _ Typing.pause)
  : unit
  =
  Cerb_debug.begin_csv_timing ();
  let insts = functions_under_test ~with_warning:false cabs_tunit sigma prog5 paused in
  save_generators ~output_dir ~filename sigma prog5 paused insts;
  save_tests ~output_dir ~filename ~without_ownership_checking sigma prog5 insts;
  BuildScripts.generate_and_save ~output_dir ~filename build_tool;
  Cerb_debug.end_csv_timing "specification test generation"
