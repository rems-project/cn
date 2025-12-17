module CF = Cerb_frontend
module Cn_to_ail = Cn_to_ail
module A = CF.AilSyntax
module Extract = Extract
module Globals = Globals
module Internal = Internal
module Records = Records
module Ownership = Ownership
module Utils = Utils

let rec group_toplevel_defs new_list = function
  | [] -> new_list
  | loc :: ls ->
    let matching_elems = List.filter (fun toplevel_loc -> loc == toplevel_loc) new_list in
    if List.is_empty matching_elems then
      group_toplevel_defs (loc :: new_list) ls
    else (
      (* Unsafe *)
      let non_matching_elems =
        List.filter (fun toplevel_loc -> loc != toplevel_loc) new_list
      in
      group_toplevel_defs (loc :: non_matching_elems) ls)


type 'a memory_access =
  | Load of
      { loc : Cerb_location.t;
        lvalue : 'a CF.AilSyntax.expression
      }
  | Store of
      { loc : Cerb_location.t;
        lvalue : 'a CF.AilSyntax.expression;
        expr : 'a CF.AilSyntax.expression
      }
  | StoreOp of
      { loc : Cerb_location.t;
        aop : CF.AilSyntax.arithmeticOperator;
        lvalue : 'a CF.AilSyntax.expression;
        expr : 'a CF.AilSyntax.expression
      }
  | Postfix of
      { loc : Cerb_location.t;
        op : [ `Incr | `Decr ];
        lvalue : 'a CF.AilSyntax.expression
      }

let collect_memory_accesses (_, sigm) =
  let open CF.AilSyntax in
  let acc = ref [] in
  (* list of scoped variables *)
  let scan_for_decls_and_update_env (bs, ss) env f_expr f_stmt =
    let lookup_ty sym = List.find (fun (sym', _) -> Sym.equal sym sym') bs in
    let env_cell = ref env in
    List.iter
      (fun s ->
         match s.node with
         (* Update the environment when variables are declared *)
         | AilSdeclaration xs ->
           List.iter
             (function
               | sym, None ->
                 env_cell := lookup_ty sym :: !env_cell;
                 ()
               | sym, Some e ->
                 env_cell := lookup_ty sym :: !env_cell;
                 f_expr e !env_cell)
             xs
         | _ -> f_stmt s !env_cell)
      ss
  in
  let rec aux_expr (AnnotatedExpression (_, _, loc, expr_)) env =
    match expr_ with
    | AilErvalue lvalue ->
      acc := (Load { loc; lvalue }, env) :: !acc;
      aux_expr lvalue env
    | AilEassign (lvalue, expr) ->
      acc := (Store { loc; lvalue; expr }, env) :: !acc;
      aux_expr lvalue env;
      aux_expr expr env
    | AilEcompoundAssign (lvalue, aop, expr) ->
      acc := (StoreOp { loc; lvalue; aop; expr }, env) :: !acc;
      aux_expr lvalue env;
      aux_expr expr env
    | AilEunary (PostfixIncr, lvalue) ->
      acc := (Postfix { loc; op = `Incr; lvalue }, env) :: !acc;
      aux_expr lvalue env
    | AilEunary (PostfixDecr, lvalue) ->
      acc := (Postfix { loc; op = `Decr; lvalue }, env) :: !acc;
      aux_expr lvalue env
    | AilEunion (_, _, None)
    | AilEoffsetof _ | AilEbuiltin _ | AilEstr _ | AilEconst _ | AilEident _
    (* TODO(vla): if the type is a VLA, the size expression is evaluated *)
    | AilEsizeof _
    (* the sub-expr is not evaluated; TODO(vla): except if it is possible
           to have an expression with VLA type here (?) *)
    | AilEsizeof_expr _ | AilEalignof _ | AilEreg_load _ | AilEinvalid _ ->
      ()
    | AilEunary (_, e)
    | AilEcast (_, _, e)
    | AilEassert e
    | AilEunion (_, _, Some e)
    | AilEcompound (_, _, e)
    | AilEmemberof (e, _)
    | AilEmemberofptr (e, _)
    | AilEannot (_, e)
    | AilEva_start (e, _)
    | AilEva_arg (e, _)
    | AilEva_end e
    | AilEprint_type e
    | AilEbmc_assume e
    | AilEarray_decay e
    | AilEfunction_decay e
    | AilEatomic e ->
      aux_expr e env
    | AilEbinary (e1, _, e2) | AilEcond (e1, None, e2) | AilEva_copy (e1, e2) ->
      aux_expr e1 env;
      aux_expr e2 env
    | AilEcond (e1, Some e2, e3) ->
      aux_expr e1 env;
      aux_expr e2 env;
      aux_expr e3 env
    | AilEcall (e, es) ->
      aux_expr e env;
      List.iter (fun e -> aux_expr e env) es
    | AilEgeneric (e, _, gas) ->
      aux_expr e env;
      List.iter (function AilGAtype (_, _, e) | AilGAdefault e -> aux_expr e env) gas
    | AilEarray (_, _, xs) ->
      List.iter (function None -> () | Some e -> aux_expr e env) xs
    | AilEstruct (_, xs) ->
      List.iter (function _, None -> () | _, Some e -> aux_expr e env) xs
    | AilEgcc_statement (bs, ss) ->
      scan_for_decls_and_update_env (bs, ss) env aux_expr aux_stmt
  and aux_stmt stmt env =
    match stmt.node with
    | AilSskip | AilSbreak | AilScontinue | AilSreturnVoid | AilSgoto _ -> ()
    | AilSexpr e | AilSreturn e | AilSreg_store (_, e) -> aux_expr e env
    | AilSblock (bs, ss) -> scan_for_decls_and_update_env (bs, ss) env aux_expr aux_stmt
    | AilSpar ss -> List.iter (fun s -> aux_stmt s env) ss
    | AilSif (e, s1, s2) ->
      aux_expr e env;
      aux_stmt s1 env;
      aux_stmt s2 env
    | AilSwhile (e, s, _) | AilSdo (s, e, _) | AilSswitch (e, s) ->
      aux_expr e env;
      aux_stmt s env
    | AilScase (_, s)
    | AilScase_rangeGNU (_, _, s)
    | AilSdefault s
    | AilSlabel (_, s, _)
    | AilSmarker (_, s) ->
      aux_stmt s env
    | AilSdeclaration _ ->
      (* AilSdeclaration must be handled in `AilSblock` *)
      failwith "unreachable"
  in
  List.iter (fun (_, (_, _, _, _, stmt)) -> aux_stmt stmt []) sigm.function_definitions;
  !acc


let memory_accesses_injections ail_prog =
  let open Cerb_frontend in
  let open Cerb_location in
  let string_of_aop aop =
    match aop with
    | AilSyntax.Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Add -> "+"
    | Sub -> "-"
    | Shl -> "<<"
    | Shr -> ">>"
    | Band -> "&"
    | Bxor -> "^"
    | Bor -> "|"
    (* We could use the below here, but it's only the same by coincidence, Pp_ail is not intended to produce C output
       CF.Pp_utils.to_plain_string (Pp_ail.pp_arithmeticOperator aop) *)
  in
  let loc_of_expr (AilSyntax.AnnotatedExpression (_, _, loc, _)) = loc in
  let pos_bbox loc =
    match bbox [ loc ] with `Other _ -> assert false | `Bbox (b, e) -> (b, e)
  in
  let acc = ref [] in
  let xs = collect_memory_accesses ail_prog in
  List.iter
    (* HK: Currently, `env` is ignored. It will be used in future patches. *)
    (fun (access, _env) ->
       match access with
       | Load { loc; _ } ->
         let b, e = pos_bbox loc in
         acc := (point b, [ "CN_LOAD(" ]) :: (point e, [ ")" ]) :: !acc
       | Store { lvalue; expr; _ } ->
         (* NOTE: we are not using the location of the access (the AilEassign), because if
           in the source the assignment was surrounded by parens its location will contain
           the parens, which will break the CN_STORE macro call *)
         let b, pos1 = pos_bbox (loc_of_expr lvalue) in
         let pos2, e = pos_bbox (loc_of_expr expr) in
         acc
         := (point b, [ "CN_STORE(" ])
            :: (region (pos1, pos2) NoCursor, [ ", " ])
            :: (point e, [ ")" ])
            :: !acc
       | StoreOp { lvalue; aop; expr; loc } ->
         (match bbox [ loc_of_expr expr ] with
          | `Other _ ->
            (* This prettyprinter doesn not produce valid C, but it's
                     correct for simple expressions and we use it here for
                     simple literals *)
            let pp_expr e = CF.Pp_utils.to_plain_string (Pp_ail.pp_expression e) in
            let sstart, ssend = pos_bbox loc in
            let b, _ = pos_bbox (loc_of_expr lvalue) in
            acc
            := (region (sstart, b) NoCursor, [ "" ])
               :: ( point b,
                    [ "CN_STORE_OP("
                      ^ pp_expr lvalue
                      ^ ","
                      ^ string_of_aop aop
                      ^ ","
                      ^ pp_expr expr
                      ^ ")"
                    ] )
               :: (region (b, ssend) NoCursor, [ "" ])
               :: !acc
          | `Bbox _ ->
            let b, pos1 = pos_bbox (loc_of_expr lvalue) in
            let pos2, e = pos_bbox (loc_of_expr expr) in
            acc
            := (point b, [ "CN_STORE_OP(" ])
               :: (region (pos1, pos2) NoCursor, [ "," ^ string_of_aop aop ^ "," ])
               :: (point e, [ ")" ])
               :: !acc)
       | Postfix { loc; op; lvalue } ->
         let op_str = match op with `Incr -> "++" | `Decr -> "--" in
         let b, e = pos_bbox loc in
         let pos1, pos2 = pos_bbox (loc_of_expr lvalue) in
         (* E++ *)
         acc
         := (region (b, pos1) NoCursor, [ "CN_POSTFIX(" ])
            :: (region (pos2, e) NoCursor, [ ", " ^ op_str ^ ")" ])
            :: !acc)
    xs;
  !acc


(* Lifetime of compound literals can change if enclosed in a block via gen_single_stat_control_flow_injs, so need to check for any instances *)
let contains_compound_literal s =
  let rec aux_expr (A.AnnotatedExpression (_, _, _, e_)) =
    match e_ with
    | AilEcompound _ -> true
    | AilEgcc_statement (_, ss) -> List.fold_left ( || ) false (List.map aux_stmt ss)
    | AilEunion (_, _, None)
    | AilEoffsetof _ | AilEbuiltin _ | AilEstr _ | AilEconst _ | AilEident _
    | AilEsizeof _ | AilEalignof _ | AilEreg_load _ | AilEinvalid _ ->
      false
    | AilEsizeof_expr e
    | AilErvalue e
    | AilEunary (_, e)
    | AilEcast (_, _, e)
    | AilEassert e
    | AilEunion (_, _, Some e)
    | AilEmemberof (e, _)
    | AilEmemberofptr (e, _)
    | AilEannot (_, e)
    | AilEva_start (e, _)
    | AilEva_arg (e, _)
    | AilEva_end e
    | AilEprint_type e
    | AilEbmc_assume e
    | AilEarray_decay e
    | AilEfunction_decay e
    | AilEatomic e ->
      aux_expr e
    | AilEbinary (e1, _, e2)
    | AilEcond (e1, None, e2)
    | AilEva_copy (e1, e2)
    | AilEassign (e1, e2)
    | AilEcompoundAssign (e1, _, e2) ->
      aux_expr e1 || aux_expr e2
    | AilEcond (e1, Some e2, e3) -> aux_expr e1 || aux_expr e2 || aux_expr e3
    | AilEcall (e, es) -> aux_expr e || List.fold_left ( || ) false (List.map aux_expr es)
    | AilEgeneric (e, _, gas) ->
      let bs =
        List.map (function A.AilGAtype (_, _, e) | AilGAdefault e -> aux_expr e) gas
      in
      aux_expr e || List.fold_left ( || ) false bs
    | AilEarray (_, _, xs) ->
      let bs = List.map (function None -> false | Some e -> aux_expr e) xs in
      List.fold_left ( || ) false bs
    | AilEstruct (_, xs) ->
      let bs = List.map (function _, None -> false | _, Some e -> aux_expr e) xs in
      List.fold_left ( || ) false bs
  and aux_stmt A.{ node = s_; _ } =
    match s_ with
    | A.(AilSdeclaration decls) ->
      let bs =
        List.map
          (fun (_, e_opt) -> match e_opt with Some e -> aux_expr e | None -> false)
          decls
      in
      List.fold_left ( || ) false bs
    | AilSblock (_, ss) ->
      let bs = List.map aux_stmt ss in
      List.fold_left ( || ) false bs
    | AilSif (e, s1, s2) -> aux_expr e || aux_stmt s1 || aux_stmt s2
    | AilSwhile (e, s, _) | AilSdo (s, e, _) | AilSswitch (e, s) ->
      aux_expr e || aux_stmt s
    | AilScase (_, s) | AilScase_rangeGNU (_, _, s) | AilSdefault s | AilSlabel (_, s, _)
      ->
      aux_stmt s
    | AilSreturn e | AilSexpr e | AilSreg_store (_, e) -> aux_expr e
    | AilSgoto _ | AilScontinue | AilSbreak | AilSskip | AilSreturnVoid | AilSpar _
    | AilSmarker _ ->
      false
  in
  aux_stmt s


let gen_single_stat_control_flow_injs statement =
  let gen_curly_braces_inj loc =
    [ (Utils.get_start_loc loc, [ "{" ]); (Utils.get_end_loc loc, [ "}" ]) ]
  in
  let is_valid_single_stat s =
    let is_single_stat A.{ node = s_; _ } =
      match s_ with
      | A.(AilSexpr _)
      | AilSreturn _ | AilSgoto _ | AilScontinue | AilSbreak | AilSskip | AilSreturnVoid
        ->
        true
      | _ -> false
    in
    let b = is_single_stat s in
    if b && contains_compound_literal s then
      failwith
        "Cannot enclose single statement with curly braces: compound literal found in \
         statement"
    else
      b
  in
  let rec aux_expr (A.AnnotatedExpression (_, _, _, e_)) =
    match e_ with
    | AilEcompound _ -> []
    | AilEgcc_statement (_, ss) -> List.concat (List.map aux_stmt ss)
    | AilEunion (_, _, None)
    | AilEoffsetof _ | AilEbuiltin _ | AilEstr _ | AilEconst _ | AilEident _
    | AilEsizeof _ | AilEalignof _ | AilEreg_load _ | AilEinvalid _ ->
      []
    | AilEsizeof_expr e
    | AilErvalue e
    | AilEunary (_, e)
    | AilEcast (_, _, e)
    | AilEassert e
    | AilEunion (_, _, Some e)
    | AilEmemberof (e, _)
    | AilEmemberofptr (e, _)
    | AilEannot (_, e)
    | AilEva_start (e, _)
    | AilEva_arg (e, _)
    | AilEva_end e
    | AilEprint_type e
    | AilEbmc_assume e
    | AilEarray_decay e
    | AilEfunction_decay e
    | AilEatomic e ->
      aux_expr e
    | AilEbinary (e1, _, e2)
    | AilEcond (e1, None, e2)
    | AilEva_copy (e1, e2)
    | AilEassign (e1, e2)
    | AilEcompoundAssign (e1, _, e2) ->
      aux_expr e1 @ aux_expr e2
    | AilEcond (e1, Some e2, e3) -> aux_expr e1 @ aux_expr e2 @ aux_expr e3
    | AilEcall (e, es) -> aux_expr e @ List.concat (List.map aux_expr es)
    | AilEgeneric (e, _, gas) ->
      let injs =
        List.map (function A.AilGAtype (_, _, e) | AilGAdefault e -> aux_expr e) gas
      in
      aux_expr e @ List.concat injs
    | AilEarray (_, _, xs) ->
      let injs = List.map (function None -> [] | Some e -> aux_expr e) xs in
      List.concat injs
    | AilEstruct (_, xs) ->
      let injs = List.map (function _, None -> [] | _, Some e -> aux_expr e) xs in
      List.concat injs
  and aux_stmt (A.{ node = s_; _ } as stmt) =
    let is_forloop_body A.{ desug_info; _ } = desug_info.is_forloop_body in
    (if is_forloop_body stmt then
       fun z -> gen_curly_braces_inj stmt.loc @ z
     else
       Fun.id)
      (match s_ with
       | A.AilSblock (_, ss) -> List.concat (List.map aux_stmt ss)
       | AilSif (e, (A.{ loc = loc1; _ } as s1), (A.{ loc = loc2; _ } as s2)) ->
         let inj_e = aux_expr e in
         let inj_true =
           if is_valid_single_stat s1 then
             gen_curly_braces_inj loc1 @ aux_stmt s1
           else
             aux_stmt s1
         in
         let inj_false =
           if is_valid_single_stat s2 then
             gen_curly_braces_inj loc2 @ aux_stmt s2
           else
             aux_stmt s2
         in
         inj_e @ inj_true @ inj_false
       | AilSwhile (e, (A.{ loc; _ } as s), _)
       | AilSdo ((A.{ loc; _ } as s), e, _)
       | AilSswitch (e, (A.{ loc; _ } as s)) ->
         let inj_e = aux_expr e in
         let inj_s =
           if is_valid_single_stat s then
             gen_curly_braces_inj loc @ aux_stmt s
           else
             aux_stmt s
         in
         inj_e @ inj_s
       | AilScase (_, (A.{ loc; _ } as s)) | AilScase_rangeGNU (_, _, (A.{ loc; _ } as s))
         ->
         if is_valid_single_stat s then
           gen_curly_braces_inj loc @ aux_stmt s
         else
           aux_stmt s
       | AilSdeclaration xs ->
         let injs =
           List.map
             (fun (_, e_opt) -> match e_opt with None -> [] | Some e -> aux_expr e)
             xs
         in
         List.concat injs
       | AilSlabel (_, s, _) | AilSdefault s -> aux_stmt s
       | AilSreturn e | AilSexpr e | AilSreg_store (_, e) -> aux_expr e
       | AilSgoto _ | AilScontinue | AilSbreak | AilSskip | AilSreturnVoid | AilSpar _
       | AilSmarker _ ->
         [])
  in
  aux_stmt statement


let get_c_control_flow_extra_curly_braces
      (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma)
  =
  sigm.function_definitions
  |> List.map (fun (_, (_, _, _, _, fn_body)) ->
    gen_single_stat_control_flow_injs fn_body)
  |> List.concat


let filter_selected_fns
      (is_sym_selected : Sym.t -> bool)
      ( (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma),
        (instrumentation : Extract.instrumentation list) )
  =
  let filtered_instrumentation =
    List.filter
      (fun (i : Extract.instrumentation) -> is_sym_selected i.fn)
      instrumentation
  in
  let filtered_ail_prog_decls =
    List.filter (fun (decl_sym, _) -> is_sym_selected decl_sym) sigm.declarations
  in
  let filtered_ail_prog_defs =
    List.filter (fun (def_sym, _) -> is_sym_selected def_sym) sigm.function_definitions
  in
  let filtered_sigm =
    { sigm with
      declarations = filtered_ail_prog_decls;
      function_definitions = filtered_ail_prog_defs
    }
  in
  (filtered_instrumentation, filtered_sigm)


let get_main_sym sym_list =
  List.filter (fun sym -> String.equal (Sym.pp_string sym) "main") sym_list


(* Filtering *)
let filter_using_skip_and_only
      skip_and_only
      ( (prog5 : unit Mucore.file),
        (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma),
        (instrumentation : Extract.instrumentation list) )
  =
  let prog5_fns_list = List.map fst (Pmap.bindings_list prog5.funs) in
  let all_fns_sym_set = Sym.Set.of_list prog5_fns_list in
  let main_sym = get_main_sym prog5_fns_list in
  let selected_function_syms =
    Sym.Set.elements (Check.select_functions skip_and_only all_fns_sym_set)
  in
  let is_sym_selected =
    fun sym -> List.mem Sym.equal sym (selected_function_syms @ main_sym)
  in
  filter_selected_fns is_sym_selected (sigm, instrumentation)


let output_to_oc oc str_list = List.iter (Stdlib.output_string oc) str_list

open Internal

let get_instrumented_filename filename =
  Filename.(remove_extension (basename filename)) ^ ".exec.c"


let get_filename_with_prefix output_dir filename = Filename.concat output_dir filename

(* TODO: fix + add CLI flag *)
let _gen_compile_commands_json cc output_dir out_filename =
  let compile_commands_json_oc =
    Stdlib.open_out (get_filename_with_prefix output_dir "compile_commands.json")
  in
  let opam_switch_prefix =
    match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
    | Some p -> p
    | None -> failwith "OPAM_SWITCH_PREFIX not set"
  in
  let compile_commands_json_str =
    [ "[";
      "\n\t{ \"directory\": \"" ^ output_dir ^ "\",";
      "\n\t\"command\": \""
      ^ cc
      ^ " -I"
      ^ opam_switch_prefix
      ^ "/lib/cn/runtime/include/ "
      ^ out_filename
      ^ "\",";
      "\n\t\"file\": \"" ^ out_filename ^ "\" }";
      "\n]"
    ]
  in
  output_to_oc compile_commands_json_oc compile_commands_json_str;
  close_out compile_commands_json_oc


let main
      ~without_ownership_checking
      ~without_loop_invariants
      ~with_loop_leak_checks
      ~without_lemma_checks
      ~exec_c_locs_mode
      ~experimental_ownership_stack_mode
      ~experimental_curly_braces
      ~experimental_lua_runtime
      ~with_testing
      ~skip_and_only
      ?max_bump_blocks
      ?bump_block_size
      filename
      _cc
      in_filename (* WARNING: this file will be deleted after this function *)
      out_filename
      output_dir
      cabs_tunit
      ((startup_sym_opt, (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma)) as
       ail_prog)
      prog5
  =
  let out_filename = get_filename_with_prefix output_dir out_filename in
  (* disabled until fixed *)
  (* _gen_compile_commands_json cc output_dir out_filename; *)
  let (full_instrumentation : Extract.instrumentation list), _ =
    Extract.collect_instrumentation cabs_tunit prog5
  in
  (* Filters based on functions passed to --only and/or --skip *)
  let filtered_instrumentation, filtered_sigm =
    filter_using_skip_and_only skip_and_only (prog5, sigm, full_instrumentation)
  in
  let static_funcs =
    filtered_instrumentation
    |> List.filter (fun (inst : Extract.instrumentation) -> inst.is_static)
    |> List.map (fun (inst : Extract.instrumentation) -> inst.fn)
    |> Sym.Set.of_list
  in
  let filtered_ail_prog = (startup_sym_opt, filtered_sigm) in
  Records.populate_record_map filtered_instrumentation prog5;
  let executable_spec =
    generate_c_specs
      without_ownership_checking
      without_loop_invariants
      with_loop_leak_checks
      without_lemma_checks
      filename
      filtered_instrumentation
      cabs_tunit
      sigm
      prog5
  in
  let c_datatype_defs = generate_c_datatypes sigm in
  let c_function_defs, c_function_decls, _c_function_locs =
    generate_c_functions filename cabs_tunit prog5 sigm
  in
  let c_predicate_defs, c_predicate_decls, _c_predicate_locs =
    generate_c_predicates filename cabs_tunit prog5 sigm
  in
  let c_lemma_defs, c_lemma_decls =
    if without_lemma_checks then
      ("", "")
    else
      generate_c_lemmas filename cabs_tunit sigm prog5
  in
  let conversion_function_defs, conversion_function_decls =
    generate_conversion_and_equality_functions filename sigm
  in
  let ownership_function_defs, ownership_function_decls =
    generate_ownership_functions without_ownership_checking !Cn_to_ail.ownership_ctypes
  in
  let ordered_ail_tag_defs = order_ail_tag_definitions sigm.tag_definitions in
  let c_tag_defs = generate_c_tag_def_strs ordered_ail_tag_defs in
  let cn_converted_struct_defs = generate_cn_versions_of_structs ordered_ail_tag_defs in
  let record_fun_defs, record_fun_decls = Records.generate_c_record_funs sigm in
  let record_defs = Records.generate_all_record_strs () in
  let fn_call_ghost_args_injs =
    generate_fn_call_ghost_args_injs filename cabs_tunit sigm prog5
  in
  let cn_ghost_enum = generate_ghost_enum prog5 in
  let cn_ghost_call_site_glob = generate_ghost_call_site_glob () in
  (* Forward declarations and CN types *)
  let cn_header_decls_list =
    List.concat
      (* TODO instead use hcat on these includes and typedefs *)
      [ [ (* TODO need handling for the stuff in stdlib.h and stdint.h but we
             can't include them here, they'll clash in essentially unavoidable
             ways with the stuff we already included and processed *)
          "#include <cn-executable/cerb_types.h>\n";
          (* TODO necessary because of the types in the struct decls. proper
             handling would be to hoist all definitions and toposort them *)
          (* TODO actually instead of *hoisting* types we can *lower* structs
             etc to the highest place they're valid *)
          "typedef __cerbty_intptr_t intptr_t;\n";
          "typedef __cerbty_uintptr_t uintptr_t;\n";
          "typedef __cerbty_intmax_t intmax_t;\n";
          "typedef __cerbty_uintmax_t uintmax_t;\n";
          (* TODO need to inject definitions for all the __cerbvars in cerberus
             builtins.lem. Hoisting/lowering doesn't affect needing to do this *)
          "static const int __cerbvar_INT_MAX = 0x7fffffff;\n";
          "static const int __cerbvar_INT_MIN = ~0x7fffffff;\n";
          "static const unsigned long long __cerbvar_SIZE_MAX = ~(0ULL);\n";
          "_Noreturn void abort(void);"
        ];
        [ c_tag_defs ];
        [ (if not (String.equal record_defs "") then "\n/* CN RECORDS */\n\n" else "");
          record_defs;
          cn_converted_struct_defs
        ];
        (if List.is_empty c_datatype_defs then [] else [ "/* CN DATATYPES */" ]);
        List.map snd c_datatype_defs;
        [ "\n\n/* OWNERSHIP FUNCTIONS */\n\n";
          ownership_function_decls;
          "/* CONVERSION FUNCTIONS */\n";
          conversion_function_decls;
          "/* RECORD FUNCTIONS */\n";
          record_fun_decls;
          c_function_decls;
          "\n";
          c_predicate_decls;
          c_lemma_decls;
          cn_ghost_enum
        ];
        cn_ghost_call_site_glob
      ]
  in
  (* Definitions for CN helper functions *)
  (* TODO: Topological sort *)
  let cn_defs_list =
    [ (* record_equality_fun_strs; *)
      (* record_equality_fun_strs'; *)
      "/* RECORD */\n";
      record_fun_defs;
      "/* CONVERSION */\n";
      conversion_function_defs;
      "/* OWNERSHIP FUNCTIONS */\n";
      ownership_function_defs;
      "/* CN FUNCTIONS */\n";
      c_function_defs;
      "\n";
      c_predicate_defs;
      c_lemma_defs
    ]
  in
  let c_datatype_locs = List.map fst c_datatype_defs in
  let toplevel_locs = group_toplevel_defs [] c_datatype_locs in
  let toplevel_injections = List.map (fun loc -> (loc, [ "" ])) toplevel_locs in
  let accesses_stmt_injs =
    if without_ownership_checking then
      []
    else
      memory_accesses_injections filtered_ail_prog
  in
  (* Use order of tag definitions from source when injecting on them directly - no call to Internal.order_ail_tag_definitions *)
  let tag_def_injs = generate_tag_definition_injs sigm.tag_definitions in
  let give_precedence_map n = List.map (fun x -> (n, x)) in
  (* workaround for https://github.com/rems-project/cn/issues/392
  HK: This workaround is not so clean, which should be fixed later.
  This is for handling the case where multiple insertions take place at the same location.
  Previously, it was handled by using fixed "precedences", which is incorrect as pointed out in the above issue.
  To address this issue, we assign precedences based on the position of the corresponding closing parenthesis for each "CN_LOAD(" or such.

  **This function is designed specifically for memory access injections and ghost arg injections, so adding a different kind of injections may require modification.**
  *)
  let give_parenthesis_aware_precedence_map l =
    let rec look_for_closing_parenthesis par acc = function
      | [] -> assert false
      | (p, [ s ]) :: xs when Char.equal (String.get s (String.length s - 1)) par ->
        (acc, xs, p, s)
      | x :: xs -> look_for_closing_parenthesis par (x :: acc) xs
    in
    let rec aux acc = function
      | [] -> acc
      | (p, strs) :: xs ->
        let par, static_prec =
          if List.equal String.equal strs [ "{" ] then ('}', 1) else (')', 0)
        in
        let injs, xs, p', closing_expr = look_for_closing_parenthesis par [] xs in
        let pos_x, pos_y =
          match Cerb_location.to_cartesian_user p' with
          | Some (start_pos, _) -> start_pos
          | _ -> failwith "error"
        in
        let open Source_injection in
        (*  ')': 1, '(': 2, '{': 3 *)
        let a =
          if static_prec = 1 then
            (Normal (-1), (p, strs))
          else
            (Cartesian (true, pos_x, pos_y), (p, strs))
        in
        let cs = give_precedence_map (Normal 0) injs in
        let b = (Cartesian (false, pos_x, pos_y), (p', [ closing_expr ])) in
        let cs' = a :: b :: cs in
        aux (cs' @ acc) xs
    in
    aux [] l
  in
  let bot = Source_injection.Normal 0 in
  let in_stmt_injs =
    (if experimental_curly_braces then
       give_parenthesis_aware_precedence_map
         (get_c_control_flow_extra_curly_braces filtered_sigm)
     else
       [])
    @ give_precedence_map bot executable_spec.in_stmt
    @ give_parenthesis_aware_precedence_map accesses_stmt_injs
    @ give_precedence_map bot toplevel_injections
    @ give_precedence_map bot tag_def_injs
    @ give_parenthesis_aware_precedence_map fn_call_ghost_args_injs
  in
  let pre_post_pairs =
    if with_testing || without_ownership_checking then
      executable_spec.pre_post
    else (
      (* XXX: ONLY IF THERE IS A MAIN *)
      (* Inject ownership init function calls and mapping and unmapping of globals into provided main function *)
      let global_ownership_init_pair =
        generate_global_assignments
          ~exec_c_locs_mode
          ~experimental_ownership_stack_mode
          ~experimental_lua_runtime
          ?max_bump_blocks
          ?bump_block_size
          cabs_tunit
          sigm
          prog5
      in
      global_ownership_init_pair @ executable_spec.pre_post)
  in
  (* Save things *)
  let oc = Stdlib.open_out out_filename in
  output_to_oc oc [ "#define __CN_INSTRUMENT\n"; "#include <cn-executable/utils.h>\n" ];
  output_to_oc oc cn_header_decls_list;
  if experimental_lua_runtime then (
      output_to_oc oc [ "#include <lua.h>\n"; "#include <lauxlib.h>\n"; "#include <lualib.h>\n" ];
  );
  output_to_oc
    oc
    [ "#ifndef offsetof\n";
      "#define offsetof(st, m) ((__cerbty_size_t)((char *)&((st *)0)->m - (char *)0))\n";
      "#endif\n"
    ];
  output_string oc "#pragma GCC diagnostic ignored \"-Wattributes\"\n";
  output_string oc "\n/* GLOBAL ACCESSORS */\n";
  output_string
    oc
    ("void* memcpy(void* dest, const void* src, __cerbty_size_t count );\n"
     ^ Globals.accessors_prototypes filename cabs_tunit prog5);
  if experimental_lua_runtime then (
      output_to_oc
        oc
        [ "/* GLOBAL LUA STATE */\n"; "lua_State *L;\n"; ];
  );
  (match
     Source_injection.(
       output_injections
         oc
         { orig_filename = filename;
           filename = in_filename;
           program = ail_prog;
           static_funcs;
           pre_post = pre_post_pairs;
           in_stmt = in_stmt_injs;
           returns = executable_spec.returns;
           inject_in_preproc = true;
           with_testing
         })
   with
   | Ok () -> ()
   | Error str ->
     (* TODO(Christopher/Rini): maybe lift this error to the exception monad? *)
     prerr_endline str);
  output_to_oc oc [ Globals.accessors_str filename cabs_tunit prog5 ];
  output_to_oc oc cn_defs_list;
  close_out oc;
  Stdlib.Sys.remove in_filename
