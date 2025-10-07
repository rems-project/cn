module CF = Cerb_frontend
module Cn_to_ail = Cn_to_ail
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
    | AilEgcc_statement (_, ss) -> List.iter (fun s -> aux_stmt s env) ss
  and aux_stmt stmt env =
    match stmt.node with
    | AilSskip | AilSbreak | AilScontinue | AilSreturnVoid | AilSgoto _ -> ()
    | AilSexpr e | AilSreturn e | AilSreg_store (_, e) -> aux_expr e env
    | AilSblock (bs, ss) ->
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
                   aux_expr e !env_cell)
               xs
           | _ -> aux_stmt s !env_cell)
        ss
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

let main
      ~without_ownership_checking
      ~without_loop_invariants
      ~with_loop_leak_checks
      ~exec_c_locs_mode
      ~experimental_ownership_stack_mode
      ~with_testing
      ~skip_and_only
      filename
      cc
      in_filename (* WARNING: this file will be deleted after this function *)
      out_filename
      output_dir
      cabs_tunit
      ((startup_sym_opt, (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma)) as
       ail_prog)
      prog5
  =
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
  close_out compile_commands_json_oc;
  let out_filename = get_filename_with_prefix output_dir out_filename in
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
      filename
      filtered_instrumentation
      sigm
      prog5
  in
  let c_datatype_defs = generate_c_datatypes sigm in
  let c_function_defs, c_function_decls, _c_function_locs =
    generate_c_functions filename prog5 sigm
  in
  let c_predicate_defs, c_predicate_decls, _c_predicate_locs =
    generate_c_predicates filename prog5 sigm
  in
  let c_lemma_defs, c_lemma_decls = generate_c_lemmas filename sigm prog5 in
  let conversion_function_defs, conversion_function_decls =
    generate_conversion_and_equality_functions filename sigm
  in
  let ownership_function_defs, ownership_function_decls =
    generate_ownership_functions without_ownership_checking !Cn_to_ail.ownership_ctypes
  in
  let c_struct_decls = generate_c_struct_strs sigm.tag_definitions in
  let cn_converted_struct_defs = generate_cn_versions_of_structs sigm.tag_definitions in
  let record_fun_defs, record_fun_decls = Records.generate_c_record_funs sigm in
  let record_defs = Records.generate_all_record_strs () in
  let fn_call_ghost_args_injs = generate_fn_call_ghost_args_injs filename sigm prog5 in
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
          "static const int __cerbvar_INT_MIN = ~0x7fffffff;\n"
        ];
        [ c_struct_decls ];
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
  let struct_locs = List.map (fun (i, (loc, _, _)) -> (i, loc)) sigm.tag_definitions in
  let struct_injs =
    List.map
      (fun (i, loc) -> (loc, [ "struct " ^ Pp.plain (CF.Pp_ail.pp_id i) ]))
      struct_locs
  in
  let give_precedence_map n = List.map (fun x -> (n, x)) in
  let in_stmt_injs =
    give_precedence_map 0 executable_spec.in_stmt
    @ give_precedence_map 1 accesses_stmt_injs
    @ give_precedence_map 0 toplevel_injections
    @ give_precedence_map 0 struct_injs
    @ give_precedence_map 0 fn_call_ghost_args_injs
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
          sigm
          prog5
      in
      global_ownership_init_pair @ executable_spec.pre_post)
  in
  (* Save things *)
  let oc = Stdlib.open_out out_filename in
  output_to_oc oc [ "#define __CN_INSTRUMENT\n"; "#include <cn-executable/utils.h>\n" ];
  output_to_oc oc cn_header_decls_list;
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
     ^ Globals.accessors_prototypes filename prog5);
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
  output_to_oc oc [ Globals.accessors_str filename prog5 ];
  output_to_oc oc cn_defs_list;
  close_out oc;
  Stdlib.Sys.remove in_filename
