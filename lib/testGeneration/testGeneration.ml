module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module AT = ArgumentTypes
module LAT = LogicalArgumentTypes
module CtA = Fulminate.Cn_to_ail
module ESpecInternal = Fulminate.Internal
module Records = Fulminate.Records
module FExtract = Fulminate.Extract
module Config = TestGenConfig
module Options = Config.Options
module Cn_to_ail = Fulminate.Cn_to_ail

type config = Config.t

let default_cfg : config = Config.default

let set_config = Config.initialize

let filename_base fn = fn |> Filename.basename |> Filename.remove_extension

let is_constant_function
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (inst : FExtract.instrumentation)
  =
  let _, _, decl = List.assoc Sym.equal inst.fn sigma.declarations in
  match decl with
  | Decl_function (_, _, args, _, _, _) ->
    List.is_empty args
    && Sym.Set.is_empty
         (LAT.free_vars (fun _ -> Sym.Set.empty) (AT.get_lat (Option.get inst.internal)))
  | Decl_object _ -> failwith __LOC__


let compile_assumes
      ~(without_ownership_checking : bool)
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (insts : FExtract.instrumentation list)
  : Pp.document
  =
  let declarations, function_definitions =
    List.split
      (List.map
         (fun ctype ->
            CtA.generate_assume_ownership_function ~without_ownership_checking ctype)
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
           []
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
      (insts : Fulminate.Extract.instrumentation list)
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
      (insts : Fulminate.Extract.instrumentation list)
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
    ^^ space
    ^^ string label
    ^^ space
    ^^ repeat padding slash
    ^^ hardline
    ^^ repeat width slash
    ^^ twice hardline
    ^^ doc


let compile_includes ~filename =
  let open Pp in
  string "#include "
  ^^ angles (string "cn-replicate/shape.h")
  ^^ hardline
  (* TODO the static hack has been removed from the testing files, and the hack
     should change from including the whole exec file to creating wrappers for
     every static function and calling those instead *)
  ^^ (if Config.with_static_hack () then
        string "#include "
        ^^ dquotes (string (filename_base filename ^ ".exec.c"))
        ^^ hardline
      else
        empty)
  ^^ string "#include "
  ^^ dquotes (string (filename_base filename ^ ".gen.h"))
  ^^ hardline


let compile_test test =
  let open Pp in
  let macro = Test.registration_macro test in
  string macro ^^ parens (string test.suite ^^ comma ^^ space ^^ string test.test) ^^ semi


let compile_test_file
      ~(without_ownership_checking : bool)
      ~(filename : string)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (insts : FExtract.instrumentation list)
  =
  let for_constant, for_generator = List.partition (is_constant_function sigma) insts in
  let constant_tests, constant_tests_defs =
    SpecTests.compile_constant_tests sigma for_constant
  in
  let generator_tests, generator_tests_defs =
    SpecTests.compile_generator_tests filename sigma prog5 for_generator
  in
  let tests = [ constant_tests; generator_tests ] in
  (* TODO copied from fulminate.ml, put somewhere shared *)
  let open ESpecInternal in
  let c_datatype_defs = generate_c_datatypes sigma in
  let c_function_defs, c_function_decls, _c_function_locs =
    generate_c_functions filename sigma prog5.logical_predicates
  in
  let c_predicate_defs, c_predicate_decls, _c_predicate_locs =
    generate_c_predicates filename sigma prog5.resource_predicates
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
  let cn_defs_list =
    [ (* record_equality_fun_strs; *)
      (* record_equality_fun_strs'; *)
      "/* RECORD */\n";
      record_fun_defs;
      "/* CONVERSION */\n";
      conversion_function_defs;
      ownership_function_defs;
      "/* CN FUNCTIONS */\n";
      c_function_defs;
      "\n";
      c_predicate_defs
    ]
  in
  let open Pp in
  !^(String.concat " " cn_header_decls_list)
  ^^ compile_includes ~filename
  ^^ twice hardline
  ^^ pp_label
       "Assume Ownership Functions"
       (compile_assumes ~without_ownership_checking filename sigma prog5 insts)
  ^^ pp_label "Shape Analyzers" (compile_shape_analyzers filename sigma prog5 insts)
  ^^ pp_label "Replicators" (compile_replicators filename sigma prog5 insts)
  ^^ pp_label "Constant function tests" constant_tests_defs
  ^^ pp_label "Generator-based tests" generator_tests_defs
  ^^ pp_label
       "Main function"
       (string "int main"
        ^^ parens (string "int argc, char* argv[]")
        ^^ break 1
        ^^ braces
             (nest
                2
                (hardline
                 ^^ separate_map
                      (twice hardline)
                      (separate_map hardline compile_test)
                      tests
                 ^^ twice hardline
                 ^^ string "return cn_test_main(argc, argv);")
              ^^ hardline))
  ^^ hardline
  ^^ !^(String.concat " " cn_defs_list)


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
      (insts : FExtract.instrumentation list)
  : unit
  =
  let filename_base = filename |> Filename.basename |> Filename.remove_extension in
  let generators_doc =
    Pp.(
      separate
        hardline
        [ string (Fulminate.Globals.accessors_prototypes filename prog5);
          SpecTests.compile_generators
            sigma
            prog5
            (List.filter (fun inst -> not (is_constant_function sigma inst)) insts)
        ])
  in
  let generators_fn = filename_base ^ ".gen.h" in
  save output_dir generators_fn generators_doc


let save_tests
      ~output_dir
      ~filename
      ~without_ownership_checking
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (insts : FExtract.instrumentation list)
  : unit
  =
  let tests_doc =
    compile_test_file ~without_ownership_checking ~filename sigma prog5 insts
  in
  save output_dir (filename_base filename ^ ".test.c") tests_doc


let save_build_script ~output_dir ~filename =
  let script_doc =
    BuildScript.generate ~output_dir ~filename_base:(filename_base filename)
  in
  save ~perm:0o777 output_dir "run_tests.sh" script_doc


(** Workaround for https://github.com/rems-project/cerberus/issues/784 *)
let needs_static_hack
      ~(with_warning : bool)
      (cabs_tunit : CF.Cabs.translation_unit)
      (inst : FExtract.instrumentation)
  =
  let (TUnit decls) = cabs_tunit in
  let is_static_func () =
    List.exists
      (fun decl ->
         match decl with
         | CF.Cabs.EDecl_func
             (FunDef
                ( loc,
                  _,
                  { storage_classes; _ },
                  Declarator
                    (_, DDecl_function (DDecl_identifier (_, Identifier (_, fn')), _)),
                  _ ))
           when String.equal (Sym.pp_string inst.fn) fn'
                && List.exists
                     (fun scs -> match scs with CF.Cabs.SC_static -> true | _ -> false)
                     storage_classes ->
           if with_warning then
             Cerb_colour.with_colour
               (fun () ->
                  Pp.(
                    warn
                      loc
                      (string "Static function"
                       ^^^ squotes (Sym.pp inst.fn)
                       ^^^ string "could not be tested.")))
               ();
           true
         | _ -> false)
      decls
  in
  is_static_func ()


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
                 (string "Function"
                  ^^^ squotes (Sym.pp inst.fn)
                  ^^^ string "has enum arguments and so could not be tested."
                  ^/^ string "See https://github.com/rems-project/cerberus/issues/765.")))
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
                 (string "Function"
                  ^^^ squotes (Sym.pp inst.fn)
                  ^^^ string "has an enum return type and so could not be tested."
                  ^/^ string "See https://github.com/rems-project/cerberus/issues/765.")))
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
  : FExtract.instrumentation list
  =
  let insts = prog5 |> FExtract.collect_instrumentation |> fst in
  let selected_fsyms =
    Check.select_functions
      (Sym.Set.of_list
         (List.map (fun (inst : FExtract.instrumentation) -> inst.fn) insts))
  in
  insts
  |> List.filter (fun (inst : FExtract.instrumentation) ->
    (match prog5.main with
     | Some main_fn -> not (Sym.equal main_fn inst.fn)
     | None -> true)
    && Option.is_some inst.internal
    && Sym.Set.mem inst.fn selected_fsyms
    && not
         (needs_static_hack ~with_warning cabs_tunit inst
          || needs_enum_hack ~with_warning sigma inst))


let run
      ~output_dir
      ~filename
      ~without_ownership_checking
      (cabs_tunit : CF.Cabs.translation_unit)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
  : unit
  =
  Cerb_debug.begin_csv_timing ();
  let insts = functions_under_test ~with_warning:false cabs_tunit sigma prog5 in
  save_generators ~output_dir ~filename sigma prog5 insts;
  save_tests ~output_dir ~filename ~without_ownership_checking sigma prog5 insts;
  save_build_script ~output_dir ~filename;
  Cerb_debug.end_csv_timing "specification test generation"
