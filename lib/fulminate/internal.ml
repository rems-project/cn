module CB = Cerb_backend
open PPrint
open Utils
module BT = BaseTypes
module A = CF.AilSyntax
module IT = IndexTerms
module LRT = LogicalReturnTypes
module LAT = LogicalArgumentTypes
module AT = ArgumentTypes
module OE = Ownership

type executable_spec =
  { pre_post : (CF.Symbol.sym * (string list * string list)) list;
    in_stmt : (Cerb_location.t * string list) list;
    returns :
      (Cerb_location.t * (CF.GenTypes.genTypeCategory A.expression option * string list))
        list
  }

let doc_to_pretty_string = CF.Pp_utils.to_plain_pretty_string

let generate_ail_stat_strs
      ?(with_newline = false)
      (bs, (ail_stats_ : CF.GenTypes.genTypeCategory A.statement_ list))
  =
  let is_assert_true = function
    | A.(AilSexpr (AnnotatedExpression (_, _, _, AilEassert expr))) ->
      (match rm_expr expr with
       | A.(AilEconst (ConstantPredefined PConstantTrue)) -> true
       | _ -> false)
    | _ -> false
  in
  (* Filter out unneeded assert(true) statements *)
  let ail_stats_ = List.filter (fun s -> not (is_assert_true s)) ail_stats_ in
  let doc =
    List.map
      (fun s -> CF.Pp_ail.(with_executable_spec (pp_statement ~bs) (mk_stmt s)))
      ail_stats_
  in
  let doc =
    List.map
      (fun d ->
         let newline = if with_newline then PPrint.hardline else PPrint.empty in
         newline ^^ d ^^ PPrint.hardline)
      doc
  in
  List.map doc_to_pretty_string doc


let rec extract_global_variables = function
  | [] -> []
  | (sym, globs) :: ds ->
    (match globs with
     | Mucore.GlobalDef (ctype, _) ->
       (sym, Sctypes.to_ctype ctype) :: extract_global_variables ds
     | GlobalDecl ctype -> (sym, Sctypes.to_ctype ctype) :: extract_global_variables ds)


let generate_c_specs_internal
      without_ownership_checking
      without_loop_invariants
      with_loop_leak_checks
      (instrumentation : Extract.instrumentation)
      (sigm : _ CF.AilSyntax.sigma)
      (prog5 : unit Mucore.file)
  =
  let dts = sigm.cn_datatypes in
  let preds = prog5.resource_predicates in
  let c_return_type =
    match List.assoc CF.Symbol.equal_sym instrumentation.fn sigm.A.declarations with
    | _, _, A.Decl_function (_, (_, ret_ty), _, _, _, _) -> ret_ty
    | _ -> failwith (__LOC__ ^ ": C function to be instrumented not found in Ail AST")
  in
  let globals = extract_global_variables prog5.globs in
  let ail_executable_spec =
    Cn_to_ail.cn_to_ail_pre_post
      ~without_ownership_checking
      ~with_loop_leak_checks
      dts
      preds
      globals
      c_return_type
      instrumentation.internal
  in
  let pre_str = generate_ail_stat_strs ail_executable_spec.pre in
  let post_str = generate_ail_stat_strs ail_executable_spec.post in
  (* C ownership checking *)
  let (pre_str, post_str), block_ownership_injs =
    if without_ownership_checking then
      ((pre_str, post_str), [])
    else (
      let fn_ownership_stats_opt, block_ownership_injs =
        OE.get_c_fn_local_ownership_checking_injs instrumentation.fn sigm
      in
      match fn_ownership_stats_opt with
      | Some (entry_ownership_bs_and_ss, exit_ownership_bs_and_ss) ->
        let entry_ownership_str = generate_ail_stat_strs entry_ownership_bs_and_ss in
        let exit_ownership_str = generate_ail_stat_strs exit_ownership_bs_and_ss in
        let pre_post_pair =
          ( pre_str @ ("\n\t/* C OWNERSHIP */\n\n" :: entry_ownership_str),
            (* NOTE - the nesting pre - entry - exit - post *)
            ("\n\t/* C OWNERSHIP */\n\n" :: exit_ownership_str) @ post_str )
        in
        (pre_post_pair, block_ownership_injs)
      | None -> ((pre_str, post_str), []))
  in
  (* Needed for extracting correct location for CN statement injection *)
  let modify_magic_comment_loc loc =
    match loc with
    | Cerb_location.Loc_region (start_pos, end_pos, cursor) ->
      Cerb_location.region
        (Cerb_position.change_cnum start_pos (-3), Cerb_position.change_cnum end_pos 2)
        cursor
    | _ -> assert false (* loc should always be a region *)
  in
  let in_stmt =
    List.map
      (fun (loc, bs_and_ss) ->
         (modify_magic_comment_loc loc, generate_ail_stat_strs bs_and_ss))
      ail_executable_spec.in_stmt
  in
  let rec get_return_and_non_return_injs return_ownership_stmts block_ownership_stmts
    = function
    | [] -> (return_ownership_stmts, block_ownership_stmts)
    | (inj : OE.ownership_injection) :: injs ->
      let strs = generate_ail_stat_strs ~with_newline:true inj.bs_and_ss in
      (match inj.injection_kind with
       | OE.ReturnInj return_kind ->
         let return_inj_expr_opt =
           match return_kind with ReturnExpr e -> Some e | ReturnVoid -> None
         in
         let return_ownership_stmt =
           (inj.loc, (return_inj_expr_opt, [ String.concat "\n" strs ]))
         in
         get_return_and_non_return_injs
           (return_ownership_stmt :: return_ownership_stmts)
           block_ownership_stmts
           injs
       | NonReturnInj ->
         let block_ownership_stmt = (inj.loc, [ String.concat "\n" strs ]) in
         get_return_and_non_return_injs
           return_ownership_stmts
           (block_ownership_stmt :: block_ownership_stmts)
           injs)
  in
  let return_ownership_stmts, block_ownership_stmts =
    get_return_and_non_return_injs [] [] block_ownership_injs
  in
  let loop_invariant_injs =
    if without_loop_invariants then
      []
    else (
      let ail_loop_invariants = ail_executable_spec.loops in
      let ail_cond_stats, ail_loop_decls = List.split ail_loop_invariants in
      (* A bit of a hack *)
      let rec remove_last = function
        | [] -> []
        | [ _ ] -> []
        | x :: xs -> x :: remove_last xs
      in
      let rec remove_last_semicolon = function
        | [] -> []
        | [ x ] ->
          let split_x = String.split_on_char ';' x in
          let without_whitespace_x = remove_last split_x in
          let res = String.concat ";" without_whitespace_x in
          [ res ]
        | x :: xs -> x :: remove_last_semicolon xs
      in
      let ail_cond_injs =
        List.map
          (fun (loc, bs_and_ss) ->
             ( get_start_loc loc,
               remove_last_semicolon (generate_ail_stat_strs bs_and_ss) @ [ ", " ] ))
          ail_cond_stats
      in
      let ail_loop_decl_injs =
        List.map
          (fun (loc, bs_and_ss) ->
             (get_start_loc loc, "{" :: generate_ail_stat_strs bs_and_ss))
          ail_loop_decls
      in
      let ail_loop_close_block_injs =
        List.map (fun (loc, _) -> (get_end_loc loc, [ "}" ])) ail_loop_decls
      in
      ail_cond_injs @ ail_loop_decl_injs @ ail_loop_close_block_injs)
  in
  ( [ (instrumentation.fn, (pre_str, post_str)) ],
    in_stmt @ block_ownership_stmts @ loop_invariant_injs,
    return_ownership_stmts )


let generate_c_assume_pres_internal
      (instrumentation_list : Extract.instrumentation list)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
  =
  let aux (inst : Extract.instrumentation) =
    let dts = sigma.cn_datatypes in
    let preds = prog5.resource_predicates in
    let args =
      match List.assoc Sym.equal inst.fn sigma.declarations with
      | _, _, Decl_function (_, _, args, _, _, _) ->
        let arg_names = AT.get_computational (Option.get inst.internal) in
        let arg_cts = List.map (fun (_, ct, _) -> ct) args in
        List.map (fun ((x, bt), ct) -> (x, (bt, ct))) (List.combine arg_names arg_cts)
      | _ -> failwith ("unreachable @ " ^ __LOC__)
    in
    let globals = extract_global_variables prog5.globs in
    Cn_to_ail.cn_to_ail_assume_pre
      dts
      inst.fn
      args
      globals
      preds
      (AT.get_lat (Option.get inst.internal))
  in
  instrumentation_list
  |> List.filter (fun (inst : Extract.instrumentation) -> Option.is_some inst.internal)
  |> List.map aux


(* Extract.instrumentation list -> executable_spec *)
let generate_c_specs
      without_ownership_checking
      without_loop_invariants
      with_loop_leak_checks
      instrumentation_list
      (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma)
      (prog5 : unit Mucore.file)
  : executable_spec
  =
  let generate_c_spec (instrumentation : Extract.instrumentation) =
    generate_c_specs_internal
      without_ownership_checking
      without_loop_invariants
      with_loop_leak_checks
      instrumentation
      sigm
      prog5
  in
  let specs = List.map generate_c_spec instrumentation_list in
  let pre_post, in_stmt, returns = Utils.list_split_three specs in
  { pre_post = List.concat pre_post;
    in_stmt = List.concat in_stmt;
    returns = List.concat returns
  }


let generate_doc_from_ail_struct ail_struct =
  CF.Pp_ail.(with_executable_spec pp_tag_definition ail_struct) ^^ PPrint.hardline


let[@warning "-32" (* unused-value-declaration *)] generate_struct_decl_str
                                                     (tag, (_, _, def))
  =
  match def with
  | C.StructDef _ -> Printf.sprintf "struct %s;\n" (Sym.pp_string tag)
  | UnionDef _ -> ""


let generate_c_records ail_structs =
  let struct_docs = List.map generate_doc_from_ail_struct ail_structs in
  doc_to_pretty_string (PPrint.concat struct_docs)


let[@warning "-32" (* unused-value-declaration *)] generate_str_from_ail_struct ail_struct
  =
  doc_to_pretty_string (generate_doc_from_ail_struct ail_struct)


let generate_str_from_ail_structs ail_structs =
  let docs = List.map generate_doc_from_ail_struct ail_structs in
  doc_to_pretty_string (Utils.concat_map_newline docs)


let generate_c_datatypes (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma) =
  let ail_datatypes =
    match sigm.cn_datatypes with
    | [] -> []
    | d :: ds ->
      let ail_dt1 = Cn_to_ail.cn_to_ail_datatype ~first:true d in
      let ail_dts = List.map Cn_to_ail.cn_to_ail_datatype ds in
      ail_dt1 :: ail_dts
  in
  let locs_and_struct_strs =
    List.map
      (fun (loc, structs) ->
         let doc =
           Utils.concat_map_newline (List.map generate_doc_from_ail_struct structs)
         in
         (loc, doc_to_pretty_string doc))
      ail_datatypes
  in
  locs_and_struct_strs


let generate_c_struct_strs c_structs =
  "\n/* ORIGINAL C STRUCTS */\n\n" ^ generate_str_from_ail_structs c_structs


let generate_cn_versions_of_structs c_structs =
  let ail_structs = List.concat (List.map Cn_to_ail.cn_to_ail_struct c_structs) in
  "\n/* CN VERSIONS OF C STRUCTS */\n\n" ^ generate_str_from_ail_structs ail_structs


let generate_fun_def_and_decl_docs funs =
  let decls, defs = List.split funs in
  let defs_prog : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma =
    { A.empty_sigma with declarations = decls; function_definitions = defs }
  in
  let decls_prog : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma =
    { A.empty_sigma with declarations = decls; function_definitions = [] }
  in
  let pp_program_with_exec_spec prog =
    CF.Pp_ail.(with_executable_spec (pp_program ~show_include:true) (None, prog))
  in
  let defs_doc = pp_program_with_exec_spec defs_prog in
  let decls_doc = pp_program_with_exec_spec decls_prog in
  (defs_doc, decls_doc)


let generate_c_functions
      (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma)
      (logical_predicates : (Sym.t * Definition.Function.t) list)
  =
  let ail_funs_and_records =
    List.map
      (fun cn_f -> Cn_to_ail.cn_to_ail_function cn_f sigm.cn_datatypes sigm.cn_functions)
      logical_predicates
  in
  let ail_funs, _ = List.split ail_funs_and_records in
  let locs_and_decls, defs = List.split ail_funs in
  let locs, decls = List.split locs_and_decls in
  let defs = List.filter_map Fun.id defs in
  let decl_str_comment = "\n/* CN FUNCTIONS */\n\n" in
  let defs_doc, decls_doc = generate_fun_def_and_decl_docs (List.combine decls defs) in
  (doc_to_pretty_string defs_doc, decl_str_comment ^ doc_to_pretty_string decls_doc, locs)


let[@warning "-32" (* unused-value-declaration *)] rec remove_duplicates eq_fun = function
  | [] -> []
  | t :: ts ->
    if List.mem eq_fun t ts then
      remove_duplicates eq_fun ts
    else
      t :: remove_duplicates eq_fun ts


let generate_c_predicates
      (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma)
      (resource_predicates : (Sym.t * Definition.Predicate.t) list)
  =
  let ail_funs, _ =
    Cn_to_ail.cn_to_ail_predicates
      resource_predicates
      sigm.cn_datatypes
      []
      sigm.cn_predicates
  in
  let locs_and_decls, defs = List.split ail_funs in
  let locs, decls = List.split locs_and_decls in
  let defs_doc, decls_doc = generate_fun_def_and_decl_docs (List.combine decls defs) in
  ( "\n/* CN PREDICATES */\n\n" ^ doc_to_pretty_string defs_doc,
    doc_to_pretty_string decls_doc,
    locs )


let generate_ownership_functions without_ownership_checking ownership_ctypes =
  let rec remove_duplicates ret_list = function
    | [] -> []
    | x :: xs ->
      if List.mem execCtypeEqual x ret_list then
        remove_duplicates (x :: ret_list) xs
      else
        x :: remove_duplicates (x :: ret_list) xs
  in
  let ctypes = remove_duplicates [] ownership_ctypes in
  let ail_funs =
    List.map
      (fun ctype ->
         Cn_to_ail.generate_get_or_put_ownership_function
           ~without_ownership_checking
           ctype)
      ctypes
  in
  let defs_doc, decls_doc = generate_fun_def_and_decl_docs ail_funs in
  let comment = "\n/* OWNERSHIP FUNCTIONS */\n\n" in
  (comment ^ doc_to_pretty_string defs_doc, doc_to_pretty_string decls_doc)


let generate_conversion_and_equality_functions
      (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma)
  =
  let struct_conversion_funs =
    List.map Cn_to_ail.generate_struct_conversion_to_function sigm.tag_definitions
    @ List.map Cn_to_ail.generate_struct_conversion_from_function sigm.tag_definitions
  in
  let struct_equality_funs =
    List.map Cn_to_ail.generate_struct_equality_function sigm.tag_definitions
  in
  let datatype_equality_funs =
    List.map Cn_to_ail.generate_datatype_equality_function sigm.cn_datatypes
  in
  let struct_map_get_funs =
    List.map Cn_to_ail.generate_struct_map_get sigm.tag_definitions
  in
  let struct_default_funs =
    List.map Cn_to_ail.generate_struct_default_function sigm.tag_definitions
  in
  let datatype_map_get_funs =
    List.map Cn_to_ail.generate_datatype_map_get sigm.cn_datatypes
  in
  let datatype_default_funs =
    List.map Cn_to_ail.generate_datatype_default_function sigm.cn_datatypes
  in
  let ail_funs =
    List.fold_left
      ( @ )
      []
      (List.map
         List.concat
         [ struct_conversion_funs;
           struct_equality_funs;
           datatype_equality_funs;
           struct_map_get_funs;
           struct_default_funs;
           datatype_map_get_funs;
           datatype_default_funs
         ])
  in
  let defs_doc, decls_doc = generate_fun_def_and_decl_docs ail_funs in
  let comment = "\n/* GENERATED STRUCT FUNCTIONS */\n\n" in
  (comment ^ doc_to_pretty_string defs_doc, comment ^ doc_to_pretty_string decls_doc)


let get_main (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma) =
  List.filter
    (fun (fn_sym, _) -> String.equal "main" (Sym.pp_string fn_sym))
    sigm.function_definitions


let has_main (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma) =
  List.non_empty (get_main sigm)


let generate_ownership_global_assignments
      (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma)
      (prog5 : unit Mucore.file)
  =
  match get_main sigm with
  | [] -> failwith "CN-exec: No main function so ownership globals cannot be initialised"
  | (main_sym, _) :: _ ->
    let globals = extract_global_variables prog5.globs in
    let global_map_fcalls = List.map OE.generate_c_local_ownership_entry_fcall globals in
    let global_map_stmts_ = List.map (fun e -> A.AilSexpr e) global_map_fcalls in
    let assignments = OE.get_ownership_global_init_stats () in
    let init_and_global_mapping_str =
      generate_ail_stat_strs ([], assignments @ global_map_stmts_)
    in
    let global_unmapping_stmts_ = List.map OE.generate_c_local_ownership_exit globals in
    let global_unmapping_str = generate_ail_stat_strs ([], global_unmapping_stmts_) in
    [ (main_sym, (init_and_global_mapping_str, global_unmapping_str)) ]
