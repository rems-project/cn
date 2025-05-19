module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module AT = ArgumentTypes
module LAT = LogicalArgumentTypes
module CtA = Fulminate.Cn_to_ail
module Utils = Fulminate.Utils
module FExtract = Fulminate.Extract
module Config = TestGenConfig

let debug_log_file : out_channel option ref = ref None

let init_debug () =
  if Option.is_none !debug_log_file && !Cerb_debug.debug_level > 0 then
    debug_log_file
    := Some
         (let open Stdlib in
          open_out "generatorCompilation.log")


let debug_log (str : string) : unit =
  init_debug ();
  match !debug_log_file with
  | Some oc ->
    output_string oc str;
    flush oc
  | None -> ()


let debug_stage (stage : string) (str : string) : unit =
  debug_log (stage ^ ":\n");
  debug_log (str ^ "\n\n")


let compile_constant_tests
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (insts : (bool * FExtract.instrumentation) list)
  : Test.t list * Pp.document
  =
  let tests, docs =
    List.map_split
      (fun ((is_static, inst) : bool * FExtract.instrumentation) ->
         ( Test.of_instrumentation is_static Constant inst,
           let open Pp in
           (CF.Pp_ail.(
              with_executable_spec
                (fun () ->
                   pp_function_prototype
                     inst.fn
                     (let _, _, decl = List.assoc Sym.equal inst.fn sigma.declarations in
                      decl))
                ())
            ^^ hardline)
           ^^ CF.Pp_ail.pp_statement
                A.(
                  Utils.mk_stmt
                    (AilSexpr
                       (Utils.mk_expr
                          (AilEcall
                             ( Utils.mk_expr
                                 (AilEident
                                    (Sym.fresh
                                       (if is_static then
                                          "CN_STATIC_UNIT_TEST_CASE"
                                        else
                                          "CN_EXTERN_UNIT_TEST_CASE"))),
                               [ Utils.mk_expr (AilEident inst.fn) ]
                               @
                               if is_static then
                                 [ Utils.mk_expr
                                     (AilEident (Sym.fresh (Utils.static_prefix filename)))
                                 ]
                               else
                                 [] ))))) ))
      insts
  in
  let open Pp in
  (tests, separate (twice hardline) docs ^^ twice hardline)


let compile_generators
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (insts : (bool * FExtract.instrumentation) list)
  : Pp.document
  =
  let ctx = GenCompile.compile filename prog5.resource_predicates insts in
  debug_stage "Compile" (ctx |> GenDefinitions.pp_context |> Pp.plain ~width:80);
  let ctx = ctx |> GenInline.inline in
  debug_stage "Inline" (ctx |> GenDefinitions.pp_context |> Pp.plain ~width:80);
  let ctx = ctx |> GenNormalize.normalize prog5 in
  debug_stage "Normalize" (ctx |> GenDefinitions.pp_context |> Pp.plain ~width:80);
  let ctx = ctx |> GenDistribute.distribute in
  debug_stage "Distribute" (ctx |> GenDefinitions.pp_context |> Pp.plain ~width:80);
  let ctx = ctx |> GenOptimize.optimize prog5 in
  debug_stage "Optimize" (ctx |> GenDefinitions.pp_context |> Pp.plain ~width:80);
  let ctx = ctx |> GenRuntime.elaborate in
  debug_stage "Elaborated" (ctx |> GenRuntime.pp |> Pp.plain ~width:80);
  ctx |> GenCodeGen.compile sigma


let convert_from ((x, ct) : Sym.t * C.ctype) =
  CF.Pp_ail.pp_expression
    (Utils.mk_expr
       (CtA.wrap_with_convert_from
          A.(
            AilEmemberofptr
              ( Utils.mk_expr (AilEident (Sym.fresh "res")),
                CF.Symbol.Identifier (Locations.other __LOC__, Sym.pp_string x) ))
          (Memory.bt_of_sct (Sctypes.of_ctype_unsafe (Locations.other __LOC__) ct))))


let compile_random_test_case
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (Test.{ is_static; suite; test; fn; internal; _ } : Test.t)
  : Pp.document
  =
  let open Pp in
  let args =
    let _, _, _, xs, _ = List.assoc Sym.equal fn sigma.function_definitions in
    match List.assoc Sym.equal fn sigma.declarations with
    | _, _, Decl_function (_, _, cts, _, _, _) ->
      List.combine xs (List.map (fun (_, ct, _) -> ct) cts)
    | _ ->
      failwith
        (String.concat
           " "
           [ "Function declaration not found for"; Sym.pp_string fn; "@"; __LOC__ ])
  in
  let globals =
    let global_syms =
      let args = args |> List.map fst in
      internal
      |> AT.get_lat
      |> LAT.free_vars (fun _ -> Sym.Set.empty)
      |> Sym.Set.to_seq
      |> List.of_seq
      |> List.filter (fun x ->
        not
          (List.mem (fun x y -> String.equal (Sym.pp_string x) (Sym.pp_string y)) x args))
    in
    List.map
      (fun sym ->
         match List.assoc Sym.equal sym prog5.globs with
         | GlobalDecl sct -> (sym, sct)
         | GlobalDef (sct, _) -> (sym, sct))
      global_syms
  in
  (CF.Pp_ail.(
     with_executable_spec
       (fun () ->
          pp_function_prototype
            fn
            (let _, _, decl = List.assoc Sym.equal fn sigma.declarations in
             decl))
       ())
   ^^ hardline)
  ^^ (if List.is_empty globals then
        string
          (if is_static then
             "CN_STATIC_RANDOM_TEST_CASE"
           else
             "CN_EXTERN_RANDOM_TEST_CASE")
      else (
        let init_name =
          string "cn_test_gen_"
          ^^ (if is_static then
                string (Fulminate.Utils.static_prefix filename) ^^ underscore ^^ Sym.pp fn
              else
                Sym.pp fn)
          ^^ string "_init"
        in
        string "void"
        ^^ space
        ^^ init_name
        ^^ parens
             (string
                (String.concat
                   "_"
                   [ "cn_gen";
                     (if is_static then
                        Fulminate.Utils.static_prefix filename ^ "_" ^ Sym.pp_string fn
                      else
                        Sym.pp_string fn);
                     "record"
                   ])
              ^^ star
              ^^ space
              ^^ string "res")
        ^^ space
        ^^ braces
             (nest
                2
                (hardline
                 ^^ separate_map
                      hardline
                      (fun (sym, sct) ->
                         let ty =
                           CF.Pp_ail.(
                             with_executable_spec
                               (pp_ctype ~is_human:false C.no_qualifiers)
                               (Sctypes.to_ctype sct))
                         in
                         let tmp_doc = Sym.pp sym ^^ string "_tmp" in
                         (ty ^^ star)
                         ^^^ tmp_doc
                         ^^^ equals
                         ^^^ (string "convert_from_cn_pointer"
                              ^^ parens (string "res->" ^^ Sym.pp sym)
                              ^^ semi
                              ^^ hardline
                              ^^ string "cn_assume_ownership"
                              ^^ parens
                                   (separate
                                      (comma ^^ space)
                                      [ string (Fulminate.Globals.getter_str filename sym);
                                        string "sizeof" ^^ parens ty;
                                        string "(char*)" ^^ dquotes init_name
                                      ]))
                         ^^ semi
                         ^^ hardline
                         ^^ string (Fulminate.Globals.setter_str filename sym)
                         ^^ parens tmp_doc
                         ^^ semi)
                      globals)
              ^^ hardline)
        ^^ twice hardline
        ^^ string
             (if is_static then
                "CN_STATIC_RANDOM_TEST_CASE_WITH_INIT"
              else
                "CN_EXTERN_RANDOM_TEST_CASE_WITH_INIT")))
  ^^ parens
       (separate
          (comma ^^ space)
          ([ string suite; string test ]
           @ (if is_static then
                [ string (Fulminate.Utils.static_prefix filename) ]
              else
                [])
           @ [ int (Config.get_num_samples ());
               separate_map (comma ^^ space) convert_from args
             ]))
  ^^ semi
  ^^ twice hardline


let compile_generator_tests
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (insts : (bool * FExtract.instrumentation) list)
  : Test.t list * Pp.document
  =
  let tests =
    List.map
      (fun ((is_static, inst) : bool * FExtract.instrumentation) ->
         Test.of_instrumentation is_static Generator inst)
      insts
  in
  let open Pp in
  (tests, concat_map (compile_random_test_case filename sigma prog5) tests)
