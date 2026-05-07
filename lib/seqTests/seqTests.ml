module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module Config = SeqTestGenConfig
module SymSet = Set.Make (Sym)
module FExtract = Fulminate.Extract
module T = Types
module SUtils = Utils
module Shrink = Shrink

let callable
      (ctx : T.context)
      ((_, _, (_, args)) :
        bool * SymSet.elt * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list))
  : bool
  =
  List.for_all
    (fun (_, (ty : C.ctype)) ->
       (match ty with
        | Ctype (_, ty) ->
          (match ty with
           | Basic (Integer Char)
           | Basic (Integer Bool)
           | Basic (Integer (Signed _))
           | Basic (Integer (Unsigned _))
           | Basic (Floating _) ->
             true
           | Pointer (_, ty) ->
             List.exists (fun (_, _, ct, _, _) -> SUtils.ty_eq ty ct) ctx
           | _ -> false))
       || List.exists (fun (_, _, ct, _, _) -> SUtils.ty_eq ty ct) ctx)
    args


let rec update_tests
          (filename : string)
          (test_states : (int * Pp.document * T.context * T.test_stats) list)
          (results :
            [ `OtherFailure
            | `PreConditionViolation
            | `PostConditionViolation of T.call
            | `Success of T.call * int
            ]
              option
              list)
  =
  let open Pp in
  match (test_states, results) with
  | [], [] -> []
  | _, [ Some `OtherFailure ] -> [ `OtherFailure (-1, empty, [], T.empty_stats) ]
  | (prev, test_so_far, ctx, stats) :: test_states, result :: results ->
    let updated_state =
      match result with
      | None ->
        `Success (prev, test_so_far, ctx, { stats with skipped = stats.skipped + 1 })
      | Some `OtherFailure -> `OtherFailure (-1, empty, ctx, stats)
      | Some `PreConditionViolation ->
        `PreConditionViolation (prev, test_so_far, ctx, stats)
      | Some (`Success (((name, is_static, ret_ty, f, args) as call), prev')) ->
        let distrib =
          let name = Sym.pp_string f in
          match List.assoc_opt String.equal name stats.distrib with
          | None -> (name, 1) :: stats.distrib
          | Some n -> (name, n + 1) :: List.remove_assoc name stats.distrib
        in
        let used_args =
          List.fold_left
            (fun acc (ty, arg) ->
               if
                 String.starts_with ~prefix:"&x" arg
                 || String.starts_with ~prefix:"x" arg
                 || List.mem
                      (fun arg (name2, _, _, _, _) ->
                         match name2 with
                         | None -> false
                         | Some name2 -> (String.equal arg) (Sym.pp_string name2))
                      arg
                      ctx
               then
                 acc
               else
                 (Some (Sym.fresh arg), false, ty, f, []) :: acc)
            []
            args
        in
        (* print_string ("used args: " ^ SUtils.ctx_to_string used_args ^ "\n"); *)
        `Success
          ( prev',
            test_so_far ^^ SUtils.test_to_doc filename name is_static ret_ty f args,
            List.append used_args (call :: ctx),
            { stats with successes = stats.successes + 1; distrib } )
      | Some (`PostConditionViolation ((_, _, _, f, _) as call)) ->
        let distrib =
          let name = Sym.pp_string f in
          match List.assoc_opt String.equal name stats.distrib with
          | None -> (name, 1) :: stats.distrib
          | Some n -> (name, n + 1) :: List.remove_assoc name stats.distrib
        in
        `PostConditionViolation
          ( prev,
            empty,
            (call :: ctx : T.context),
            { stats with failures = stats.failures + 1; distrib } )
    in
    updated_state :: update_tests filename test_states results
  | _, _ -> failwith "impossible"


(* needs way more complexity here *)
let calc_score (ctx : T.context) (args : (SymSet.elt * C.ctype) list) : int =
  let calc_score_typebased = function
    | C.Ctype (_, Basic (Integer itype)) ->
      (match itype with
       | Char | Bool -> 20
       | Signed _ | Unsigned _ | Wchar_t | Wint_t -> 40
       | Enum _ | Size_t | Ptrdiff_t | Ptraddr_t -> 30)
    | C.Ctype (_, Basic (Floating _)) -> 40
    | C.Ctype (_, Pointer (qual, ty)) ->
      let const_modifier = if qual.const then -40 else 0 in
      const_modifier
      +
        (match ty with
        | C.Ctype (_, Pointer (_, _)) -> 120
        | C.Ctype (_, Basic _) -> 60
        | C.Ctype (_, Array _) -> 70
        | C.Ctype (_, Function _) -> 70
        | C.Ctype (_, Struct _) -> 50
        | C.Ctype (_, Union _) -> 60
        | C.Ctype (_, Void) -> 100
        | _ -> 30)
    | C.Ctype (_, Array _) -> 25
    | C.Ctype (_, Struct _) -> 25
    | C.Ctype (_, Union _) -> 30
    | _ -> 20
  in
  let calc_score_reusable ty =
    match List.find_opt (fun (_, _, ct, _, _) -> SUtils.ty_eq ty ct) ctx with
    | Some _ -> 25
    | None -> 0
  in
  List.fold_left
    (fun acc (_, ty) -> acc + calc_score_typebased ty + calc_score_reusable ty)
    1
    args


let gen_arg (ctx : T.context) ((name, ty) : SymSet.elt * C.ctype) : string =
  let generated_base_ty = SUtils.gen_val ty in
  let prev_call =
    let prev_calls =
      List.filter
        (fun (name, _, ct, _, _) ->
           Option.is_some name
           && (SUtils.ty_eq ty ct
               ||
               match ty with
               | Ctype (_, Pointer (_, ty)) -> SUtils.ty_eq ty ct
               | _ -> false))
        ctx
    in
    match List.length prev_calls with
    | 0 -> []
    | n ->
      (match List.nth prev_calls (Random.int n) with
       | Some name, _, ty', _, _ ->
         if not (SUtils.ty_eq ty' ty) then
           (* only way they're not directly equal is if pointer*)
           [ "&" ^ Sym.pp_string name ]
         else
           [ Sym.pp_string name ]
       | None, _, _, _, _ -> failwith "impossible case")
  in
  let options = List.append generated_base_ty prev_call in
  match List.length options with
  | 0 ->
    failwith
      ("unable to generate arg or reuse T.context for "
       ^ Sym.pp_string name
       ^ " of type "
       ^ CF.String_core_ctype.string_of_ctype ty
       ^ " in T.context "
       ^ SUtils.ctx_to_string ctx)
  | n -> List.nth options (Random.int n)


let create_test_file (sequence : Pp.document) (fun_decls : Pp.document) : Pp.document =
  let open Pp in
  string "#include <stddef.h>"
  ^^ hardline
  ^^ fun_decls
  ^^ twice hardline
  ^^ string "#include <cn-executable/utils.h>"
  ^^ twice hardline
  ^^ string "int main"
  ^^ parens (string "int argc, char* argv[]")
  ^^ break 1
  ^^ braces
       (nest
          2
          (hardline
           ^^
           let init_ghost = Fulminate.Ownership.get_ownership_global_init_stats () in
           separate_map hardline SUtils.stmt_to_doc init_ghost ^^ hardline ^^ sequence)
        ^^ hardline)


let rec gen_sequence
          (funcs :
            (bool * SymSet.elt * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list))
              list)
          (fuel : int)
          (test_states : (int * Pp.document * T.context * T.test_stats) list)
          (output_dir : string)
          (filename : string)
          (fun_decls : Pp.document)
  : [ `PostConditionViolation of Pp.document * T.test_stats
    | `Success of Pp.document * T.test_stats
    | `OtherFailure of Pp.document
    ]
  =
  let open Pp in
  if fuel = 0 then (
    let test_strs =
      List.map
        (fun (_, seq_so_far, ctx, _) ->
           let unmap_stmt =
             List.filter_map
               (fun (name, _, ret, _, _) ->
                  match name with
                  | Some name ->
                    Some (Fulminate.Ownership.generate_c_local_ownership_exit (name, ret))
                  | None -> None)
               (SUtils.extract_actual_ctx ctx)
           in
           let unmap_str =
             hardline ^^ separate_map hardline SUtils.stmt_to_doc unmap_stmt
           in
           seq_so_far ^^ unmap_str ^^ hardline ^^ string "printf(\"S\");")
        test_states
    in
    `Success
      ( SUtils.create_intermediate_test_file
          test_strs
          (List.init (Config.get_num_tests ()) (fun _ -> empty))
          fun_decls,
        SUtils.combine_stats
          (List.map (fun (_, _, _, stats) -> stats) test_states)
          T.empty_stats ))
  else (
    let callables =
      List.map
        (fun (_, _, ctx, _) ->
           List.map
             (fun ((_, _, (_, args)) as f) -> (calc_score ctx args, f))
             (List.filter (callable ctx) funcs))
        test_states
    in
    (* SUtils.print_scores (List.nth callables 0); *)
    let gen_test ()
      : [ `OtherFailure of int * document * T.context * T.test_stats
        | `PreConditionViolation of int * document * T.context * T.test_stats
        | `PostConditionViolation of int * document * T.context * T.test_stats
        | `Success of int * document * T.context * T.test_stats
        ]
          list
      =
      let gen_test_h (is_static, f, ((_, ret_ty), params)) (ctx : T.context) (prev : int)
        : int * T.call
        =
        let name, prev =
          match ret_ty with
          | C.Ctype (_, Void) ->
            (None, prev) (* attempted to use fresh_cn but did not work for some reason?*)
          | _ -> (Some (Sym.fresh ("x" ^ string_of_int prev)), prev + 1)
        in
        let args = List.map (fun ((_, ty) as param) -> (ty, gen_arg ctx param)) params in
        (prev, (name, is_static, ret_ty, f, args))
      in
      let prev_and_tests =
        List.map
          (fun ((fs, (prev, _, ctx, _)) :
                 (int
                 * (bool
                   * SymSet.elt
                   * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list)))
                   list
                 * 'a) ->
             if List.length fs = 0 then
               None
             else
               Some
                 (gen_test_h
                    (SUtils.pick
                       fs
                       (Random.int (List.fold_left ( + ) 1 (List.map fst fs))))
                    ctx
                    prev))
          (List.combine callables test_states)
      in
      let results =
        SUtils.analyze_results
          prev_and_tests
          (List.map (fun (_, test_str, _, _) -> test_str) test_states)
          filename
          output_dir
          fun_decls
      in
      update_tests filename test_states results
    in
    let test_states = gen_test () in
    match List.hd test_states with
    | `OtherFailure (_, _, ctx, _) ->
      `OtherFailure
        (create_test_file
           (SUtils.ctx_to_tests filename ctx ^^ hardline ^^ string "return 139;")
           fun_decls)
    | _ ->
      let postcond_violations =
        List.filter_map
          (fun result ->
             match result with `PostConditionViolation x -> Some x | _ -> None)
          test_states
      in
      let test_states' =
        List.map
          (fun result ->
             match result with
             | `Success info -> info
             | `OtherFailure info -> info
             | `PreConditionViolation info -> info
             | `PostConditionViolation info -> info)
          test_states
      in
      if List.length postcond_violations <> 0 then (
        let _, _, ctx, _ =
          List.hd
            (List.sort
               (fun (_, _, ctx1, _) (_, _, ctx2, _) ->
                  Int.compare (SUtils.ctx_length ctx1) (SUtils.ctx_length ctx2))
               postcond_violations)
        in
        let num_left, seq =
          if Config.is_disable_shrink () then (
            let actual_ctx = SUtils.extract_actual_ctx ctx in
            (List.length actual_ctx, SUtils.ctx_to_tests filename (List.rev actual_ctx)))
          else
            Shrink.shrink (SUtils.extract_actual_ctx ctx) output_dir filename fun_decls
        in
        (* print_string (SUtils.ctx_to_string ctx ^ "\n"); *)
        let combined_stats =
          SUtils.combine_stats
            (List.map (fun (_, _, _, stats) -> stats) test_states')
            T.empty_stats
        in
        `PostConditionViolation
          ( create_test_file (seq ^^ hardline ^^ string "return 2;") fun_decls,
            { combined_stats with discarded = combined_stats.successes + 1 - num_left } ))
      else
        gen_sequence funcs (fuel - 1) test_states' output_dir filename fun_decls)


let compile_sequence
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (insts : FExtract.instrumentation list)
      (num_samples : int)
      (output_dir : string)
      (filename : string)
      (fun_decls : Pp.document)
  : [ `PostConditionViolation of Pp.document * T.test_stats
    | `Success of Pp.document * T.test_stats
    | `OtherFailure of Pp.document
    ]
  =
  let fuel = num_samples in
  let declarations : A.sigma_declaration list =
    insts
    |> List.map (fun (inst : FExtract.instrumentation) ->
      (inst.fn, List.assoc Sym.equal inst.fn sigma.declarations))
  in
  let args_map
    : (bool * SymSet.elt * ((C.qualifiers * C.ctype) * (SymSet.elt * C.ctype) list)) list
    =
    List.map
      (fun (inst : FExtract.instrumentation) ->
         ( inst.is_static,
           inst.fn,
           let _, _, _, xs, _ = List.assoc Sym.equal inst.fn sigma.function_definitions in
           match List.assoc Sym.equal inst.fn declarations with
           | _, _, Decl_function (_, (qual, ret), cts, _, _, _) ->
             ((qual, ret), List.combine xs (List.map (fun (_, ct, _) -> ct) cts))
           | _ ->
             failwith
               (String.concat
                  " "
                  [ "Function declaration not found for";
                    Sym.pp_string inst.fn;
                    "@";
                    __LOC__
                  ]) ))
      insts
  in
  gen_sequence
    args_map
    fuel
    (List.map
       (fun _ ->
          ( 1,
            Pp.empty,
            [],
            { T.successes = 0; failures = 0; skipped = 0; discarded = 0; distrib = [] } ))
       (List.init (Config.get_num_tests ()) Fun.id))
    output_dir
    filename
    fun_decls


let generate
      ~(output_dir : string)
      ~(filename : string)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (insts : FExtract.instrumentation list)
  : int
  =
  if List.is_empty insts then failwith "No testable functions";
  let filename_base = filename |> Filename.basename |> Filename.chop_extension in
  let test_file = filename_base ^ ".test.c" in
  let script_doc' = BuildScript.generate_intermediate ~output_dir ~filename_base in
  SUtils.save ~perm:0o777 output_dir "run_tests_intermediate.sh" script_doc';
  let fun_to_decl (inst : FExtract.instrumentation) =
    CF.Pp_ail.(
      with_executable_spec
        (fun () ->
           pp_function_prototype
             (match inst.fn with
              | Symbol (s, n, SD_Id str) ->
                Symbol
                  ( s,
                    n,
                    SD_Id
                      (if inst.is_static then
                         Fulminate.Utils.static_prefix filename ^ "_" ^ str
                       else
                         str) )
              | s -> s)
             (let _, _, decl = List.assoc Sym.equal inst.fn sigma.declarations in
              decl))
        ())
  in
  let open Pp in
  let ordered_ail_tag_defs =
    Fulminate.Internal.order_ail_tag_definitions sigma.tag_definitions
  in
  let struct_decls = Fulminate.Internal.generate_c_tag_def_strs ordered_ail_tag_defs in
  let fun_decls =
    string struct_decls ^^ hardline ^^ separate_map hardline fun_to_decl insts
  in
  let compiled_seq =
    compile_sequence sigma insts (Config.get_num_calls ()) output_dir filename fun_decls
  in
  let exit_code, seq, output_msg =
    match compiled_seq with
    | `OtherFailure seq ->
      ( 139,
        seq,
        Printf.sprintf
          "============================================\n\n\
           FATAL ERROR:\n\
           Failure occured while seq-testing %s\n\n\
           ============================================"
          filename )
    | `PostConditionViolation (seq, stats) ->
      let script_doc = BuildScript.generate ~output_dir ~filename_base 1 in
      SUtils.save ~perm:0o777 output_dir "run_tests.sh" script_doc;
      ( 2,
        seq,
        Printf.sprintf
          "============================================\n\n\
           Stats for nerds:\n\
           %d tests succeeded\n\
           POST-CONDITION VIOLATION DETECTED.\n\
           %d tests discarded after shrinking. (%.2f%% reduction)\n\n\
           ============================================"
          stats.successes
          stats.discarded
          (float_of_int stats.discarded /. float_of_int (stats.successes + 1) *. 100.0) )
    | `Success (seq, stats) ->
      let num_tests = List.fold_left (fun acc (_, num) -> acc + num) 0 stats.distrib in
      let distrib_to_str =
        if List.length stats.distrib = 0 then
          "No calls generated"
        else
          List.fold_left
            (fun acc (f, count) ->
               acc
               ^ Printf.sprintf
                   "%s: %d calls (%.2f%% of calls)\n"
                   f
                   count
                   (100.0 *. (float_of_int count /. float_of_int num_tests)))
            ""
            stats.distrib
      in
      let script_doc =
        BuildScript.generate ~output_dir ~filename_base (Config.get_num_tests ())
      in
      SUtils.save ~perm:0o777 output_dir "run_tests.sh" script_doc;
      ( 0,
        seq,
        Printf.sprintf
          "============================================\n\n\
           Stats for nerds:\n\
           passed: %d, failed: %d, skipped: %d\n\
           Distribution of calls:\n\
           %s\n\
           ============================================"
          stats.successes
          stats.failures
          stats.skipped
          distrib_to_str )
  in
  print_endline output_msg;
  SUtils.save output_dir test_file seq;
  exit_code


type seq_config = SeqTestGenConfig.t

let default_seq_cfg : seq_config = SeqTestGenConfig.default

let set_seq_config = SeqTestGenConfig.initialize

(** Workaround for https://github.com/rems-project/cerberus/issues/765 *)
let needs_enum_hack
      ~(with_warning : bool)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (inst : Fulminate.Extract.instrumentation)
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
                  ^^^ string "has enum arguments and so could not be tested.")))
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
                  ^^^ string "has an enum return type and so could not be tested.")))
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
  : Fulminate.Extract.instrumentation list
  =
  let insts = fst (FExtract.collect_instrumentation cabs_tunit prog5) in
  let selected_fsyms =
    Check.select_functions
      !Check.skip_and_only
      (Sym.Set.of_list
         (List.map (fun (inst : Fulminate.Extract.instrumentation) -> inst.fn) insts))
  in
  insts
  |> List.filter (fun (inst : Fulminate.Extract.instrumentation) ->
    Option.is_some inst.internal
    && Sym.Set.mem inst.fn selected_fsyms
    && not (needs_enum_hack ~with_warning sigma inst))


let run_seq
      ~output_dir
      ~filename
      (cabs_tunit : CF.Cabs.translation_unit)
      (sigma : Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma)
      (prog5 : unit Mucore.file)
  : int
  =
  Cerb_debug.begin_csv_timing ();
  let insts = functions_under_test ~with_warning:false cabs_tunit sigma prog5 in
  if Option.is_some prog5.main then
    failwith "Cannot test a file with a `main` function";
  let exit_code = generate ~output_dir ~filename sigma insts in
  Cerb_debug.end_csv_timing "sequence test generation";
  exit_code
