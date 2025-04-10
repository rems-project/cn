module CF = Cerb_frontend
open Utils
module A = CF.AilSyntax
module C = CF.Ctype

type ownership_mode =
  | Pre
  | Post
  | Loop

type ail_bindings_and_statements =
  A.bindings * CF.GenTypes.genTypeCategory A.statement_ list

(* Differentiate between whether return statement carries an expression or not *)
type return_kind =
  | ReturnVoid
  | ReturnExpr of CF.GenTypes.genTypeCategory A.expression

(* Injections are treated differently depending on whether they involve returns or not *)
type injection_kind =
  | ReturnInj of return_kind
  | NonReturnInj

type ownership_injection =
  { loc : Cerb_location.t;
    bs_and_ss : ail_bindings_and_statements;
    injection_kind : injection_kind
  }

let ownership_mode_to_enum_str = function Pre -> "GET" | Post -> "PUT" | Loop -> "LOOP"

let get_cn_stack_depth_sym = Sym.fresh "get_cn_stack_depth"

let cn_stack_depth_incr_sym = Sym.fresh "ghost_stack_depth_incr"

let cn_stack_depth_decr_sym = Sym.fresh "ghost_stack_depth_decr"

let cn_postcondition_leak_check_sym = Sym.fresh "cn_postcondition_leak_check"

let cn_loop_put_back_ownership_sym = Sym.fresh "cn_loop_put_back_ownership"

let cn_loop_leak_check_and_put_back_ownership_sym =
  Sym.fresh "cn_loop_leak_check_and_put_back_ownership"


let c_add_ownership_fn_sym = Sym.fresh "c_add_to_ghost_state"

let c_remove_ownership_fn_sym = Sym.fresh "c_remove_from_ghost_state"

(* TODO: Use these to reduce verbosity of output. Mirrors C+CN input slightly less since
   we replace declarations with a call to one of these macros
   let c_declare_and_map_local_sym = Sym.fresh "c_declare_and_map_local"

   let c_declare_init_and_map_local_sym = Sym.fresh "c_declare_init_and_map_local"
*)

let get_ownership_global_init_stats () =
  let cn_ghost_state_init_fcall =
    mk_expr
      A.(
        AilEcall (mk_expr (AilEident (Sym.fresh "initialise_ownership_ghost_state")), []))
  in
  let cn_ghost_stack_depth_init_fcall =
    mk_expr
      A.(AilEcall (mk_expr (AilEident (Sym.fresh "initialise_ghost_stack_depth")), []))
  in
  List.map
    (fun e -> A.(AilSexpr e))
    [ cn_ghost_state_init_fcall; cn_ghost_stack_depth_init_fcall ]


(* c_map_local_to_stack_depth((uintptr_t) &xs, sizeof(struct node * )) *)

let generate_c_local_ownership_entry_fcall (local_sym, local_ctype) =
  let local_ident = mk_expr A.(AilEident local_sym) in
  let arg1 =
    A.(
      AilEcast (C.no_qualifiers, C.uintptr_t, mk_expr (AilEunary (Address, local_ident))))
  in
  let arg2 = A.(AilEsizeof (C.no_qualifiers, local_ctype)) in
  let arg3 = A.(AilEcall (mk_expr (AilEident get_cn_stack_depth_sym), [])) in
  mk_expr
    (AilEcall
       (mk_expr (AilEident c_add_ownership_fn_sym), List.map mk_expr [ arg1; arg2; arg3 ]))


let generate_c_local_cn_addr_var sym =
  (* Hardcoding parts of cn_to_ail_base_type to prevent circular dependency between
     this module and Cn_internal_to_ail, which includes Ownership already. *)
  let cn_addr_sym = generate_sym_with_suffix ~suffix:"_addr_cn" sym in
  let annots = [ CF.Annot.Atypedef (Sym.fresh "cn_pointer") ] in
  (* Ctype_ doesn't matter to pretty-printer when typedef annotations are present *)
  let inner_ctype = mk_ctype ~annots C.Void in
  let cn_ptr_ctype = mk_ctype C.(Pointer (C.no_qualifiers, inner_ctype)) in
  let binding = create_binding cn_addr_sym cn_ptr_ctype in
  let addr_of_sym = mk_expr A.(AilEunary (Address, mk_expr (AilEident sym))) in
  let fcall_sym = Sym.fresh "convert_to_cn_pointer" in
  let conversion_fcall = A.(AilEcall (mk_expr (AilEident fcall_sym), [ addr_of_sym ])) in
  let decl = A.(AilSdeclaration [ (cn_addr_sym, Some (mk_expr conversion_fcall)) ]) in
  (binding, decl)


let generate_c_local_ownership_entry_bs_and_ss (sym, ctype) =
  let entry_fcall = generate_c_local_ownership_entry_fcall (sym, ctype) in
  let entry_fcall_stat = A.(AilSexpr entry_fcall) in
  let addr_cn_binding, addr_cn_decl = generate_c_local_cn_addr_var sym in
  ([ addr_cn_binding ], [ entry_fcall_stat; addr_cn_decl ])


(* int x = 0, y = 5;

   ->

   int x = 0, _dummy = (c_map_local(&x), 0), y = 5, _dummy2 = (c_map_local(&y), 0); *)

(* NEW SCHEME:

   Case 1: initial value provided
   =================================

   int x = 0, y = 5;

   ->

   int x = (c_map_local(&x), 0), y = (c_map_local(&y), 5);

   Case 2: no initial value (expr_opt = None)
   ===========================================

   int x;

   ->

   ?????????
   NB: In this special case for inside a for-loop condition, it's unlikely that there will be no initial value provided. Leaving as a TODO for now.
*)

(* TODO: Include binding + declaration of <sym>_addr_cn via generate_c_local_cn_addr_var function *)
let rec gen_loop_ownership_entry_decls bindings = function
  | [] -> []
  | (sym, expr_opt) :: xs ->
    (* For now, requiring that every variable being declared has some initial value *)
    (match expr_opt with
     | Some expr ->
       let ctype = find_ctype_from_bindings bindings sym in
       let entry_fcall = generate_c_local_ownership_entry_fcall (sym, ctype) in
       let new_rhs_expr = mk_expr A.(AilEbinary (entry_fcall, Comma, expr)) in
       (sym, Some new_rhs_expr) :: gen_loop_ownership_entry_decls bindings xs
     | None ->
       let error_msg =
         Printf.sprintf
           "TODO: Uninitialised declared variables in loop conditions not supported yet. \
            For a quick fix, provide some initial value for variable %s\n"
           (Sym.pp_string sym)
       in
       failwith error_msg)


let generate_c_local_ownership_entry_inj ~is_forloop loc decls bindings =
  if is_forloop then (
    let new_decls = gen_loop_ownership_entry_decls bindings decls in
    [ (loc, bindings, [ A.AilSdeclaration new_decls ]) ])
  else (
    let ownership_bs_and_ss =
      List.map
        (fun (sym, _) ->
           let ctype = find_ctype_from_bindings bindings sym in
           generate_c_local_ownership_entry_bs_and_ss (sym, ctype))
        decls
    in
    let bs, ss = List.split ownership_bs_and_ss in
    [ (get_end_loc loc, List.concat bs, List.concat ss) ])


(* c_remove_local_footprint((uintptr_t) &xs, cn_ownership_global_ghost_state,
   sizeof(struct node * )); *)
let generate_c_local_ownership_exit (local_sym, local_ctype) =
  let local_ident = mk_expr A.(AilEident local_sym) in
  let arg1 =
    A.(
      AilEcast (C.no_qualifiers, C.uintptr_t, mk_expr (AilEunary (Address, local_ident))))
  in
  let arg2 = A.(AilEsizeof (C.no_qualifiers, local_ctype)) in
  A.(
    AilSexpr
      (mk_expr
         A.(
           AilEcall
             ( mk_expr (AilEident c_remove_ownership_fn_sym),
               List.map mk_expr [ arg1; arg2 ] ))))


let get_c_local_ownership_checking params =
  let entry_ownership_bs_and_ss =
    List.map (fun param -> generate_c_local_ownership_entry_bs_and_ss param) params
  in
  let entry_ownership_bs, entry_ownership_ss = List.split entry_ownership_bs_and_ss in
  let exit_ownership_stats = List.map generate_c_local_ownership_exit params in
  ( (List.concat entry_ownership_bs, List.concat entry_ownership_ss),
    ([], exit_ownership_stats) )


let rec collect_visibles bindings = function
  | [] -> []
  | A.{ loc = _; is_forloop = _; attrs = _; node = AilSdeclaration decls } :: ss ->
    let decl_syms_and_ctypes =
      List.map (fun (sym, _) -> (sym, find_ctype_from_bindings bindings sym)) decls
    in
    decl_syms_and_ctypes @ collect_visibles bindings ss
  | _ :: ss -> collect_visibles bindings ss


let rec take n = function
  | [] -> []
  | x :: xs ->
    if n = 0 then
      []
    else
      x :: take (n - 1) xs


let rec get_c_control_flow_block_unmaps_aux
          break_vars
          continue_vars
          return_vars
          bindings
          A.{ loc; is_forloop = _; attrs = _; node = s_ }
  : ownership_injection list
  =
  match s_ with
  | A.(AilSdeclaration _) -> []
  | AilSblock (bs, ss) ->
    let injs =
      List.mapi
        (fun i s ->
           let ss_ = take (i + 1) ss in
           let visibles = collect_visibles (bs @ bindings) ss_ in
           get_c_control_flow_block_unmaps_aux
             (visibles @ break_vars)
             (visibles @ continue_vars)
             (visibles @ return_vars)
             (bs @ bindings)
             s)
        ss
    in
    List.concat injs
  | AilSif (_, s1, s2) ->
    let injs =
      get_c_control_flow_block_unmaps_aux break_vars continue_vars return_vars bindings s1
    in
    let injs' =
      get_c_control_flow_block_unmaps_aux break_vars continue_vars return_vars bindings s2
    in
    injs @ injs'
  | AilSwhile (_, s, _) | AilSdo (s, _, _) ->
    get_c_control_flow_block_unmaps_aux
      []
      []
      return_vars
      bindings
      s (* For while and do-while loops *)
  | AilSswitch (_, s) ->
    get_c_control_flow_block_unmaps_aux [] continue_vars return_vars bindings s
  | AilScase (_, s) | AilScase_rangeGNU (_, _, s) | AilSdefault s | AilSlabel (_, s, _) ->
    get_c_control_flow_block_unmaps_aux break_vars continue_vars return_vars bindings s
  | AilSgoto _ -> [] (* TODO *)
  | AilSreturnVoid ->
    [ { loc;
        bs_and_ss = ([], List.map generate_c_local_ownership_exit return_vars);
        injection_kind = ReturnInj ReturnVoid
      }
    ]
  | AilSreturn e ->
    [ { loc;
        bs_and_ss = ([], List.map generate_c_local_ownership_exit return_vars);
        injection_kind = ReturnInj (ReturnExpr e)
      }
    ]
  | AilScontinue ->
    let loc_before_continue = get_start_loc loc in
    [ { loc = loc_before_continue;
        bs_and_ss = ([], List.map generate_c_local_ownership_exit continue_vars);
        injection_kind = NonReturnInj
      }
    ]
  | AilSbreak ->
    let loc_before_break = get_start_loc loc in
    [ { loc = loc_before_break;
        bs_and_ss = ([], List.map generate_c_local_ownership_exit break_vars);
        injection_kind = NonReturnInj
      }
    ]
  | AilSskip | AilSexpr _ | AilSpar _ | AilSreg_store _ | AilSmarker _ -> []


let get_c_control_flow_block_unmaps stat =
  get_c_control_flow_block_unmaps_aux [] [] [] [] stat


(* Ghost state tracking for block declarations *)
let rec get_c_block_entry_exit_injs_aux bindings A.{ loc; is_forloop; node = s_; _ } =
  let gen_standard_block_injs bs ss =
    let exit_injs =
      List.map
        (fun (b_sym, ((_, _, _), _, _, b_ctype)) ->
           ( get_end_loc ~offset:(-1) loc,
             [ generate_c_local_ownership_exit (b_sym, b_ctype) ] ))
        bs
    in
    let exit_injs' = List.map (fun (loc, stats) -> (loc, [], stats)) exit_injs in
    let stat_injs = List.map (fun s -> get_c_block_entry_exit_injs_aux bs s) ss in
    List.concat stat_injs @ exit_injs'
  in
  match s_ with
  | A.(AilSdeclaration decls) ->
    generate_c_local_ownership_entry_inj ~is_forloop loc decls bindings
  | AilSblock
      ( bs,
        ([ A.{ loc = decl_loc; node = AilSdeclaration decls; _ };
           A.{ node = A.AilSwhile (_, s, _); _ }
         ] as ss) ) ->
    if is_forloop then (
      (* WIP: special case for for-loops *)
      let inj = generate_c_local_ownership_entry_inj ~is_forloop decl_loc decls bs in
      let injs' = get_c_block_entry_exit_injs_aux [] s in
      inj @ injs')
    else
      gen_standard_block_injs bs ss
  | AilSblock (bs, ss) -> gen_standard_block_injs bs ss
  | AilSif (_, s1, s2) ->
    let injs = get_c_block_entry_exit_injs_aux bindings s1 in
    let injs' = get_c_block_entry_exit_injs_aux bindings s2 in
    injs @ injs'
  | AilSwhile (_, s, _) -> get_c_block_entry_exit_injs_aux bindings s
  | AilSdo (s, _, _) -> get_c_block_entry_exit_injs_aux bindings s
  | AilSswitch (_, s)
  | AilScase (_, s)
  | AilScase_rangeGNU (_, _, s)
  | AilSdefault s
  | AilSlabel (_, s, _) ->
    get_c_block_entry_exit_injs_aux bindings s
  | AilSgoto _ | AilSreturn _ | AilScontinue | AilSbreak | AilSskip | AilSreturnVoid
  | AilSexpr _ | AilSpar _ | AilSreg_store _ | AilSmarker _ ->
    []


let get_c_block_entry_exit_injs stat : ownership_injection list =
  let injs = get_c_block_entry_exit_injs_aux [] stat in
  List.map
    (fun (loc, bs, ss) -> { loc; bs_and_ss = (bs, ss); injection_kind = NonReturnInj })
    injs


let rec remove_duplicates ds = function
  | [] -> []
  | l :: ls ->
    let loc_eq_fn loc loc' =
      String.equal
        (Cerb_location.location_to_string loc)
        (Cerb_location.location_to_string loc')
    in
    if List.mem loc_eq_fn l ds then
      remove_duplicates ds ls
    else
      l :: remove_duplicates (l :: ds) ls


let get_c_block_local_ownership_checking_injs
      A.({ loc = _; is_forloop = _; attrs = _; node = fn_block } as statement)
  =
  match fn_block with
  | A.(AilSblock _) ->
    let injs = get_c_block_entry_exit_injs statement in
    let injs' = get_c_control_flow_block_unmaps statement in
    let injs = injs @ injs' in
    let locs = List.map (fun o_inj -> o_inj.loc) injs in
    let locs = remove_duplicates [] locs in
    let rec combine_injs_over_location loc = function
      | [] -> []
      | inj :: injs' ->
        if
          String.equal
            (Cerb_location.location_to_string loc)
            (Cerb_location.location_to_string inj.loc)
        then (
          let bs, ss = inj.bs_and_ss in
          (bs, ss, inj.injection_kind) :: combine_injs_over_location loc injs')
        else
          combine_injs_over_location loc injs'
    in
    (* If any of the individual injections to be combined is a return injection, the entire combined injection becomes a return injection *)
    let rec get_return_inj_kind = function
      | [] -> NonReturnInj
      | ReturnInj r :: _ -> ReturnInj r
      | NonReturnInj :: xs -> get_return_inj_kind xs
    in
    (* Injections at the same location need to be grouped together *)
    let combined_injs =
      List.map
        (fun l ->
           let injs' = combine_injs_over_location l injs in
           let bs_list, ss_list, inj_kind_list = Utils.list_split_three injs' in
           let inj_kind = get_return_inj_kind inj_kind_list in
           { loc = l;
             bs_and_ss = (List.concat bs_list, List.concat ss_list);
             injection_kind = inj_kind
           })
        locs
    in
    combined_injs
  | _ ->
    Printf.printf "Ownership: function body is not a block";
    []


(* Ghost state *)
let get_c_fn_local_ownership_checking_injs
      sym
      (sigm : CF.GenTypes.genTypeCategory CF.AilSyntax.sigma)
  =
  match
    ( List.assoc_opt Sym.equal sym sigm.function_definitions,
      List.assoc_opt Sym.equal sym sigm.declarations )
  with
  | ( Some (_, _, _, param_syms, fn_body),
      Some (_, _, Decl_function (_, _, param_types, _, _, _)) ) ->
    let param_types = List.map (fun (_, ctype, _) -> ctype) param_types in
    let params = List.combine param_syms param_types in
    let ownership_stats_pair = get_c_local_ownership_checking params in
    let block_ownership_injs = get_c_block_local_ownership_checking_injs fn_body in
    (Some ownership_stats_pair, block_ownership_injs)
  | _, _ -> (None, [])
