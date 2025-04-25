module CF = Cerb_frontend
module Cn_to_ail = Cn_to_ail
module Extract = Extract
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


let filter_injs_by_filename inj_pairs fn =
  List.filter
    (fun (loc, _) ->
       match Cerb_location.get_filename loc with
       | Some name -> String.equal name fn
       | None -> false)
    inj_pairs


let memory_accesses_injections ail_prog =
  let open Cerb_frontend in
  let open Cerb_location in
  let loc_of_expr (AilSyntax.AnnotatedExpression (_, _, loc, _)) = loc in
  let pos_bbox loc =
    match bbox [ loc ] with `Other _ -> assert false | `Bbox (b, e) -> (b, e)
  in
  let acc = ref [] in
  let xs = Ail_analysis.collect_memory_accesses ail_prog in
  List.iter
    (fun access ->
       match access with
       | Ail_analysis.Load { loc; _ } ->
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
       | StoreOp { lvalue; aop; expr; _ } ->
         let b, pos1 = pos_bbox (loc_of_expr lvalue) in
         let pos2, e = pos_bbox (loc_of_expr expr) in
         let op_str =
           match aop with
           | Mul -> "*"
           | Div -> "/"
           | Mod -> "%"
           | Add -> "+"
           | Sub -> "-"
           | Shl -> "<<"
           | Shr -> ">>"
           | Band -> "&"
           | Bxor -> "^"
           | Bor -> "|"
         in
         acc
         := (point b, [ "CN_STORE_OP(" ])
            :: (region (pos1, pos2) NoCursor, [ "," ^ op_str ^ "," ])
            :: (point e, [ ")" ])
            :: !acc
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
      (prog5 : unit Mucore.file)
      (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma)
      (full_instrumentation : Extract.instrumentation list)
  =
  (* Filtering based on Check.skip_and_only *)
  let prog5_fns_list = List.map fst (Pmap.bindings_list prog5.funs) in
  let all_fns_sym_set = Sym.Set.of_list prog5_fns_list in
  let selected_function_syms =
    Sym.Set.elements (Check.select_functions all_fns_sym_set)
  in
  let main_sym =
    List.filter (fun sym -> String.equal (Sym.pp_string sym) "main") prog5_fns_list
  in
  let is_sym_selected =
    fun sym -> List.mem Sym.equal sym (selected_function_syms @ main_sym)
  in
  let filtered_instrumentation =
    List.filter
      (fun (i : Extract.instrumentation) -> is_sym_selected i.fn)
      full_instrumentation
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


let output_to_oc oc str_list = List.iter (Stdlib.output_string oc) str_list

open Internal

let get_instrumented_filename filename =
  Filename.(remove_extension (basename filename)) ^ ".exec.c"


let get_output_filename outdir outfile filename =
  let file = Option.value ~default:(get_instrumented_filename filename) outfile in
  let prefix = match outdir with Some dir_name -> dir_name | None -> "" in
  Filename.concat prefix file


let main
      ?(without_ownership_checking = false)
      ?(without_loop_invariants = false)
      ?(with_loop_leak_checks = false)
      ?(with_test_gen = false)
      in_filename (* WARNING: this file will be delted after this function *)
      out_filename
      ((startup_sym_opt, (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma)) as
       ail_prog)
      prog5
  =
  let (full_instrumentation : Extract.instrumentation list), _ =
    Extract.collect_instrumentation prog5
  in
  let filtered_instrumentation, filtered_sigm =
    filter_selected_fns prog5 sigm full_instrumentation
  in
  let filtered_ail_prog = (startup_sym_opt, filtered_sigm) in
  Records.populate_record_map filtered_instrumentation prog5;
  let executable_spec =
    generate_c_specs
      without_ownership_checking
      without_loop_invariants
      with_loop_leak_checks
      filtered_instrumentation
      sigm
      prog5
  in
  let c_datatype_defs = generate_c_datatypes sigm in
  let c_function_defs, c_function_decls, _c_function_locs =
    generate_c_functions sigm prog5.logical_predicates
  in
  let c_predicate_defs, c_predicate_decls, _c_predicate_locs =
    generate_c_predicates sigm prog5.resource_predicates
  in
  let conversion_function_defs, conversion_function_decls =
    generate_conversion_and_equality_functions sigm
  in
  let ownership_function_defs, ownership_function_decls =
    generate_ownership_functions without_ownership_checking !Cn_to_ail.ownership_ctypes
  in
  let c_struct_decls = generate_c_struct_strs sigm.tag_definitions in
  let cn_converted_struct_defs = generate_cn_versions_of_structs sigm.tag_definitions in
  let record_fun_defs, record_fun_decls = Records.generate_c_record_funs sigm in
  let record_defs = Records.generate_all_record_strs () in
  (* Forward declarations and CN types *)
  let cn_header_decls_list =
    List.concat
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
          c_predicate_decls
        ]
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
      c_predicate_defs
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
  let in_stmt_injs =
    executable_spec.in_stmt @ accesses_stmt_injs @ toplevel_injections @ struct_injs
  in
  let pre_post_pairs =
    if with_test_gen then
      if not (has_main sigm) then
        executable_spec.pre_post
      else
        failwith
          "Input file cannot have predefined main function when passing to CN test-gen \
           tooling"
    else if without_ownership_checking then
      executable_spec.pre_post
    else (
      (* XXX: ONLY IF THERE IS A MAIN *)
      (* Inject ownership init function calls and mapping and unmapping of globals into provided main function *)
      let global_ownership_init_pair = generate_ownership_global_assignments sigm prog5 in
      global_ownership_init_pair @ executable_spec.pre_post)
  in
  (* Save things *)
  let oc = Stdlib.open_out out_filename in
  output_to_oc
    oc
    [ "#define CN_INSTRUMENTATION_MODE\n"; "#include <cn-executable/utils.h>\n" ];
  output_to_oc oc cn_header_decls_list;
  (match
     Source_injection.(
       output_injections
         oc
         { filename = in_filename;
           program = ail_prog;
           pre_post = pre_post_pairs;
           in_stmt = in_stmt_injs;
           returns = executable_spec.returns;
           inject_in_preproc = true
         })
   with
   | Ok () -> ()
   | Error str ->
     (* TODO(Christopher/Rini): maybe lift this error to the exception monad? *)
     prerr_endline str);
  output_to_oc oc cn_defs_list;
  close_out oc;
  Stdlib.Sys.remove in_filename
