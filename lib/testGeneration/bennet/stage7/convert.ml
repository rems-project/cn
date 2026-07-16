module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module CtA = Fulminate.Cn_to_ail
module Utils = Fulminate.Utils
module Records = Fulminate.Records
module BT = BaseTypes
module T = Terms.Normal
module MT = MakeTerm
module LC = LogicalConstraints

module Make (AD : Domain.T) = struct
  module Stage6 = Stage6.Make (AD)

  let mk_expr = Utils.mk_expr

  let mk_stmt = Utils.mk_stmt

  let bt_to_ctype (bt : BT.t) : C.ctype = CtA.bt_to_ail_ctype bt

  (* Convert BT to ctype for variable binding - Unit becomes void* instead of void *)
  let bt_to_ctype_for_binding (bt : BT.t) : C.ctype =
    match bt with
    | BT.Unit -> C.(mk_ctype_pointer no_qualifiers (Ctype ([], Void)))
    | _ -> bt_to_ctype bt


  let name_of_bt (bt : BT.t) : string =
    match bt with
    | BT.Unit -> "void"
    | _ ->
      let ct = bt_to_ctype bt in
      let ct' =
        match bt_to_ctype bt with
        | Ctype (_, Pointer (_, ct')) -> ct'
        | _ -> failwith ("name_of_bt: expected pointer type, got " ^ Pp.plain (BT.pp bt))
      in
      let default =
        CF.Pp_utils.to_plain_string
          CF.Pp_ail.(with_executable_spec (pp_ctype C.no_qualifiers) ct')
      in
      Utils.get_typedef_string ct |> Option.value ~default


  let bt_to_domain_type_string (bt : BT.t) : string =
    match bt with
    | BT.Bits (Signed, sz) -> Printf.sprintf "int%d_t" sz
    | BT.Bits (Unsigned, sz) -> Printf.sprintf "uint%d_t" sz
    | BT.Loc () -> "uintptr_t"
    | _ -> failwith ("bt_to_domain_type_string: unsupported type " ^ Pp.plain (BT.pp bt))


  let _str_name_of_bt (bt : BT.t) : string =
    name_of_bt bt |> String.split_on_char ' ' |> String.concat "_"


  let transform_it
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (name : Sym.t)
        (it : T.t)
    =
    let var = Sym.fresh_anon () in
    let it_bt = T.get_bt it in
    let bs, ss, e =
      match it_bt with
      | BT.Unit ->
        (* For unit types, return NULL directly *)
        ([], [], mk_expr (AilEconst ConstantNull))
      | _ ->
        CtA.cn_to_ail_expr_toplevel filename sigma.cn_datatypes [] (Some name) None it
    in
    ( [ Utils.create_binding var (bt_to_ctype_for_binding it_bt) ],
      A.
        [ AilSdeclaration
            [ ( var,
                Some
                  (mk_expr
                     (AilEgcc_statement (bs, List.map mk_stmt (ss @ [ A.AilSexpr e ]))))
              )
            ]
        ],
      mk_expr (AilEident var) )


  let transform_lc filename (sigma : CF.GenTypes.genTypeCategory A.sigma) (lc : LC.t) =
    let var = Sym.fresh_anon () in
    let bs, ss, e =
      CtA.cn_to_ail_logical_constraint filename sigma.cn_datatypes [] None lc
    in
    ( [ Utils.create_binding var (bt_to_ctype_for_binding BT.Bool) ],
      A.
        [ AilSdeclaration
            [ ( var,
                Some
                  (mk_expr
                     (AilEgcc_statement (bs, List.map mk_stmt (ss @ [ A.AilSexpr e ]))))
              )
            ]
        ],
      mk_expr (AilEident var) )


  let string_ident (str : string) : CF.GenTypes.genTypeCategory A.expression_ =
    AilEident (Sym.fresh str)


  let string_call (str : string) (es : CF.GenTypes.genTypeCategory A.expression list)
    : CF.GenTypes.genTypeCategory A.expression_
    =
    A.AilEcall (mk_expr (string_ident str), es)


  (* Module for SMT term generation *)
  module Smt = Symbolic.Smt.Make (AD)

  (** Generate a cn_term value read for an IT expression (substituting with actual runtime value) *)
  let generate_value_read
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (it : T.t)
    : Pp.document
    =
    let open Pp in
    let bt = T.get_bt it in
    let bs, ss, e =
      CtA.cn_to_ail_expr_toplevel filename sigma.cn_datatypes [] None None it
    in
    let e_cn =
      if List.is_empty bs && List.is_empty ss then
        e
      else
        mk_expr (A.AilEgcc_statement (bs, List.map mk_stmt (ss @ [ A.AilSexpr e ])))
    in
    let e_str =
      CF.Pp_utils.to_plain_string CF.Pp_ail.(with_executable_spec pp_expression e_cn)
    in
    match bt with
    | BT.Bits (Signed, width) ->
      !^"cn_smt_bits"
      ^^ parens
           (!^"true"
            ^^ comma
            ^^^ int width
            ^^ comma
            ^^^ !^"convert_from_cn_bits_i"
            ^^ int width
            ^^ parens !^e_str)
    | BT.Bits (Unsigned, width) ->
      !^"cn_smt_bits"
      ^^ parens
           (!^"false"
            ^^ comma
            ^^^ int width
            ^^ comma
            ^^^ !^"convert_from_cn_bits_u"
            ^^ int width
            ^^ parens !^e_str)
    | BT.Loc () ->
      !^"cn_smt_pointer"
      ^^ parens (!^"(uintptr_t)" ^^ !^"convert_from_cn_pointer" ^^ parens !^e_str)
    | BT.Bool -> !^"cn_smt_bool" ^^ parens (!^"convert_from_cn_bool" ^^ parens !^e_str)
    | _ ->
      (* Unsupported type for direct value read - use default SMT term *)
      !^"cn_smt_default" ^^ parens (Smt.convert_basetype bt)


  (* Optimize concrete subterms: replace compound subterms not containing [sym]
       with a fresh MT.sym_ whose name is the SMT-wrapped C expression. *)
  let rec optimize_concrete_terms
            filename
            (sigma : CF.GenTypes.genTypeCategory A.sigma)
            (protected : Sym.Set.t)
            (it : T.t)
    : T.t
    =
    let open Terms in
    if Sym.Set.is_empty (Sym.Set.inter protected (T.free_vars it)) then (
      match T.get_term it with
      | Sym _ | Const _ | SizeOf _ | OffsetOf _ | Nil _ | CN_None _ ->
        (* Leaves and trivial constants - don't recompile *)
        it
      | _ ->
        (* Compound term not containing target: compile via CtA and wrap as SMT value *)
        let bt = T.get_bt it in
        let smt_doc = generate_value_read filename sigma it in
        MT.sym_ (Sym.fresh (Pp.plain smt_doc), bt, Locations.other __LOC__))
    else (
      let bt = T.get_bt it in
      let loc = T.get_loc it in
      match T.get_term it with
      | Sym _ | Const _ -> it
      | Unop (op, t) ->
        IT (Unop (op, optimize_concrete_terms filename sigma protected t), bt, loc)
      | Binop (op, t1, t2) ->
        IT
          ( Binop
              ( op,
                optimize_concrete_terms filename sigma protected t1,
                optimize_concrete_terms filename sigma protected t2 ),
            bt,
            loc )
      | ITE (cond, t2, t3) ->
        IT
          ( ITE
              ( optimize_concrete_terms filename sigma protected cond,
                optimize_concrete_terms filename sigma protected t2,
                optimize_concrete_terms filename sigma protected t3 ),
            bt,
            loc )
      | Cast (cast_bt, t) ->
        IT (Cast (cast_bt, optimize_concrete_terms filename sigma protected t), bt, loc)
      | Tuple ts ->
        IT
          (Tuple (List.map (optimize_concrete_terms filename sigma protected) ts), bt, loc)
      | NthTuple (n, t) ->
        IT (NthTuple (n, optimize_concrete_terms filename sigma protected t), bt, loc)
      | Struct (tag, members) ->
        IT
          ( Struct
              ( tag,
                List.map
                  (fun (id, t) ->
                     (id, optimize_concrete_terms filename sigma protected t))
                  members ),
            bt,
            loc )
      | StructMember (t, m) ->
        IT (StructMember (optimize_concrete_terms filename sigma protected t, m), bt, loc)
      | StructUpdate ((t1, m), t2) ->
        IT
          ( StructUpdate
              ( (optimize_concrete_terms filename sigma protected t1, m),
                optimize_concrete_terms filename sigma protected t2 ),
            bt,
            loc )
      | Record members ->
        IT
          ( Record
              (List.map
                 (fun (id, t) -> (id, optimize_concrete_terms filename sigma protected t))
                 members),
            bt,
            loc )
      | RecordMember (t, m) ->
        IT (RecordMember (optimize_concrete_terms filename sigma protected t, m), bt, loc)
      | RecordUpdate ((t1, m), t2) ->
        IT
          ( RecordUpdate
              ( (optimize_concrete_terms filename sigma protected t1, m),
                optimize_concrete_terms filename sigma protected t2 ),
            bt,
            loc )
      | MemberShift (t, tag, id) ->
        IT
          ( MemberShift (optimize_concrete_terms filename sigma protected t, tag, id),
            bt,
            loc )
      | ArrayShift { base; ct; index } ->
        IT
          ( ArrayShift
              { base = optimize_concrete_terms filename sigma protected base;
                ct;
                index = optimize_concrete_terms filename sigma protected index
              },
            bt,
            loc )
      | CopyAllocId { addr; loc = loc_copy } ->
        IT
          ( CopyAllocId
              { addr = optimize_concrete_terms filename sigma protected addr;
                loc = optimize_concrete_terms filename sigma protected loc_copy
              },
            bt,
            loc )
      | HasAllocId l ->
        IT (HasAllocId (optimize_concrete_terms filename sigma protected l), bt, loc)
      | Aligned { t; align } ->
        IT
          ( Aligned
              { t = optimize_concrete_terms filename sigma protected t;
                align = optimize_concrete_terms filename sigma protected align
              },
            bt,
            loc )
      | WrapI (ity, t) ->
        IT (WrapI (ity, optimize_concrete_terms filename sigma protected t), bt, loc)
      | MapConst (mbt, t) ->
        IT (MapConst (mbt, optimize_concrete_terms filename sigma protected t), bt, loc)
      | MapSet (t1, t2, t3) ->
        IT
          ( MapSet
              ( optimize_concrete_terms filename sigma protected t1,
                optimize_concrete_terms filename sigma protected t2,
                optimize_concrete_terms filename sigma protected t3 ),
            bt,
            loc )
      | MapGet (t1, t2) ->
        IT
          ( MapGet
              ( optimize_concrete_terms filename sigma protected t1,
                optimize_concrete_terms filename sigma protected t2 ),
            bt,
            loc )
      | MapDef ((s, s_bt), t) ->
        IT
          ( MapDef
              ( (s, s_bt),
                optimize_concrete_terms filename sigma (Sym.Set.add s protected) t ),
            bt,
            loc )
      | EachI ((i1, (s, s_bt), i2), t) ->
        IT
          ( EachI
              ( (i1, (s, s_bt), i2),
                optimize_concrete_terms filename sigma (Sym.Set.add s protected) t ),
            bt,
            loc )
      | Let ((nm, t1), t2) ->
        IT
          ( Let
              ( (nm, optimize_concrete_terms filename sigma protected t1),
                optimize_concrete_terms filename sigma (Sym.Set.add nm protected) t2 ),
            bt,
            loc )
      | Match (scrutinee, cases) ->
        IT
          ( Match
              ( optimize_concrete_terms filename sigma protected scrutinee,
                List.map
                  (fun (pat, body) ->
                     let pat_syms =
                       List.fold_left
                         (fun acc (s, _) -> Sym.Set.add s acc)
                         protected
                         (T.bound_by_pattern pat)
                     in
                     (pat, optimize_concrete_terms filename sigma pat_syms body))
                  cases ),
            bt,
            loc )
      | Apply (f, args) ->
        IT
          ( Apply (f, List.map (optimize_concrete_terms filename sigma protected) args),
            bt,
            loc )
      | Constructor (c, args) ->
        IT
          ( Constructor
              ( c,
                List.map
                  (fun (id, t) ->
                     (id, optimize_concrete_terms filename sigma protected t))
                  args ),
            bt,
            loc )
      | CN_Some t ->
        IT (CN_Some (optimize_concrete_terms filename sigma protected t), bt, loc)
      | IsSome t ->
        IT (IsSome (optimize_concrete_terms filename sigma protected t), bt, loc)
      | GetOpt t ->
        IT (GetOpt (optimize_concrete_terms filename sigma protected t), bt, loc)
      | Cons (t1, t2) ->
        IT
          ( Cons
              ( optimize_concrete_terms filename sigma protected t1,
                optimize_concrete_terms filename sigma protected t2 ),
            bt,
            loc )
      | Head t -> IT (Head (optimize_concrete_terms filename sigma protected t), bt, loc)
      | Tail t -> IT (Tail (optimize_concrete_terms filename sigma protected t), bt, loc)
      | Representable (sct, t) ->
        IT
          ( Representable (sct, optimize_concrete_terms filename sigma protected t),
            bt,
            loc )
      | Good (sct, t) ->
        IT (Good (sct, optimize_concrete_terms filename sigma protected t), bt, loc)
      | _ -> it)


  (** Generate cn_term construction code for a constraint, substituting free variables
      (except the target) with their runtime values *)
  let generate_constraint_term
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        ~(target : Sym.t)
        ~(target_bt : BT.t)
        (constraint_it : T.t)
    : Pp.document
    =
    (* Get all free variables with their types from the original constraint *)
    let free_vars_bts = T.free_vars_bts constraint_it in
    (* First, optimize compound concrete subterms *)
    let optimized_it =
      optimize_concrete_terms filename sigma (Sym.Set.singleton target) constraint_it
    in
    (* Substitute remaining free variables (except target) with value reads *)
    let substituted_it =
      Sym.Map.fold
        (fun sym bt acc ->
           if Sym.equal sym target then
             acc
           else (
             (* Generate a symbolic term that represents reading the runtime value *)
             let sym_it = MT.sym_ (sym, bt, Locations.other __LOC__) in
             let value_read_str = Pp.plain (generate_value_read filename sigma sym_it) in
             (* Create a fresh symbol that will be printed as the value read expression *)
             let value_sym = Sym.fresh value_read_str in
             T.subst (T.make_rename ~from:sym ~to_:value_sym) acc))
        free_vars_bts
        optimized_it
    in
    (* Now generate the target variable as cn_smt_sym *)
    let target_sym_str =
      let open Pp in
      !^"cn_smt_sym"
      ^^ parens
           (parens !^"cn_sym"
            ^^ braces
                 (!^".name = "
                  ^^ dquotes (Sym.pp target)
                  ^^ comma
                  ^^^ !^".id = "
                  ^^ int (Sym.num target))
            ^^ comma
            ^^^ Smt.convert_basetype target_bt)
    in
    let target_value_sym = Sym.fresh (Pp.plain target_sym_str) in
    let final_it =
      T.subst (T.make_rename ~from:target ~to_:target_value_sym) substituted_it
    in
    (* Convert to cn_term AST construction code *)
    Smt.convert_indexterm sigma final_it


  (** Generate cn_term construction code for an address expression, keeping ALL
      free variables symbolic as cn_smt_sym references. *)
  let generate_addr_term
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (it_addr : T.t)
    : Pp.document
    =
    let open Pp in
    let free_vars_bts = T.free_vars_bts it_addr in
    let all_protected = Sym.Set.of_seq (Sym.Map.to_seq free_vars_bts |> Seq.map fst) in
    let optimized_it = optimize_concrete_terms filename sigma all_protected it_addr in
    let final_it =
      Sym.Map.fold
        (fun sym bt acc ->
           let sym_str =
             plain
               (!^"cn_smt_sym"
                ^^ parens
                     (parens !^"cn_sym"
                      ^^ braces
                           (!^".name = "
                            ^^ dquotes (Sym.pp sym)
                            ^^ comma
                            ^^^ !^".id = "
                            ^^ int (Sym.num sym))
                      ^^ comma
                      ^^^ Smt.convert_basetype bt))
           in
           let value_sym = Sym.fresh sym_str in
           T.subst (T.make_rename ~from:sym ~to_:value_sym) acc)
        free_vars_bts
        optimized_it
    in
    Smt.convert_indexterm sigma final_it


  (** Generate pre-declarations for backward abstract interpretation blame in ALSO mode.
      Returns (pre_decl_stmts, num_other_vars, addr_term_name, ids_name, syms_name). *)
  let generate_bwd_blame_parts
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (it_addr : T.t)
        (ptr_sym : Sym.t)
    : CF.GenTypes.genTypeCategory A.statement_ list * int * string * string * string
    =
    let open Pp in
    let unique_id = Sym.num (Sym.fresh "_bwd") in
    let free_vars_bts = T.free_vars_bts it_addr in
    let other_vars =
      Sym.Map.bindings free_vars_bts
      |> List.filter (fun (s, _) -> not (Sym.equal s ptr_sym))
    in
    let num_other = List.length other_vars in
    let addr_term_name = "_bwd_addr_term_" ^ string_of_int unique_id in
    let ids_name = "_bwd_other_ids_" ^ string_of_int unique_id in
    let syms_name = "_bwd_other_syms_" ^ string_of_int unique_id in
    let addr_term_expr = generate_addr_term filename sigma it_addr in
    let decl_addr_str =
      plain (!^"cn_term*" ^^^ !^addr_term_name ^^^ !^"=" ^^^ addr_term_expr)
    in
    let ids_inner =
      if num_other = 0 then
        !^"NULL"
      else
        separate_map (comma ^^ space) (fun (s, _) -> Sym.pp s) other_vars
    in
    let decl_ids_str =
      plain (!^"const void*" ^^^ !^ids_name ^^ !^"[]" ^^^ !^"=" ^^^ braces ids_inner)
    in
    let syms_inner =
      if num_other = 0 then
        !^"{NULL, 0}"
      else
        separate_map
          (comma ^^ space)
          (fun (s, _) ->
             !^"(bennet_absint_sym){.name ="
             ^^^ dquotes (Sym.pp s)
             ^^ !^", .id ="
             ^^^ int (Sym.num s)
             ^^ !^"}")
          other_vars
    in
    let decl_syms_str =
      plain
        (!^"const bennet_absint_sym"
         ^^^ !^syms_name
         ^^ !^"[]"
         ^^^ !^"="
         ^^^ braces syms_inner)
    in
    let mk_decl_stmt str = A.AilSexpr (mk_expr (A.AilEident (Sym.fresh str))) in
    let stmts = List.map mk_decl_stmt [ decl_addr_str; decl_ids_str; decl_syms_str ] in
    (stmts, num_other, addr_term_name, ids_name, syms_name)


  let rec pointer_of (it : T.t) : Sym.t * BT.t =
    match it with
    | IT (CopyAllocId { loc = ptr; _ }, _, _)
    | IT (ArrayShift { base = ptr; _ }, _, _)
    | IT (MemberShift (ptr, _, _), _, _) ->
      pointer_of ptr
    | IT (Sym x, bt, _) | IT (Cast (_, IT (Sym x, bt, _)), _, _) -> (x, bt)
    | _ ->
      let pointers =
        it |> T.free_vars_bts |> Sym.Map.filter (fun _ bt -> BT.equal bt (BT.Loc ()))
      in
      if not (Sym.Map.cardinal pointers == 1) then
        Cerb_debug.print_debug 2 [] (fun () ->
          Pp.(
            plain
              (braces
                 (separate_map
                    (comma ^^ space)
                    (fun (x, bt) -> Sym.pp x ^^ colon ^^^ BT.pp bt)
                    (List.of_seq (Sym.Map.to_seq pointers)))
               ^^^ !^" in "
               ^^ T.pp it)));
      if Sym.Map.is_empty pointers then (
        print_endline (Pp.plain (T.pp it));
        failwith __LOC__);
      Sym.Map.choose pointers


  (** Convert Sctypes.t to a C type string for sizeof *)
  let sct_to_c_type_string (sct : Sctypes.t) : string =
    CF.Pp_utils.to_plain_string
      CF.Pp_ail.(with_executable_spec (pp_ctype C.no_qualifiers) (Sctypes.to_ctype sct))


  (** Statically compute the byte offset from the base pointer for an address MT.
      Returns an IT expression of type [Memory.size_bt] representing the offset. *)
  let rec compute_offset_it (var_sym : Sym.t) (it : T.t) : T.t =
    let loc = Locations.other __LOC__ in
    let zero = MT.num_lit_ Z.zero Memory.size_bt loc in
    match it with
    | IT (Sym x, _, _) when Sym.equal x var_sym -> zero
    | IT (Cast (_, IT (Sym x, _, _)), _, _) when Sym.equal x var_sym -> zero
    | IT (CopyAllocId { loc = ptr; _ }, _, _) -> compute_offset_it var_sym ptr
    | IT (MemberShift (base, tag, member), _, _) ->
      let base_offset = compute_offset_it var_sym base in
      let member_offset = Terms.IT (OffsetOf (tag, member), Memory.size_bt, loc) in
      MT.add_ (base_offset, member_offset) loc
    | IT (ArrayShift { base; ct; index }, _, _) ->
      let base_offset = compute_offset_it var_sym base in
      let elem_size = MT.sizeOf_ ct loc in
      let index_cast = MT.cast_ Memory.size_bt index loc in
      MT.add_ (base_offset, MT.mul_ (elem_size, index_cast) loc) loc
    | _ -> failwith ("compute_offset_it: unexpected IT shape: " ^ Pp.plain (T.pp it))


  (** Generate BENNET_REFINE_ASSIGNMENT calls for ArbitraryDomain pointer codegen *)
  let generate_assignment_refinements
        (filename : string)
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        ~(var_sym : Sym.t)
        ~(last_var : Sym.t)
        (name : Sym.t)
        (asgns : (T.t * Sctypes.t * T.t option) list)
    : A.bindings * CF.GenTypes.genTypeCategory A.statement_ list
    =
    let open Pp in
    List.fold_right
      (fun (it_addr, sct, max_addr) (acc_b, acc_s) ->
         let pointer_opt = try Some (pointer_of it_addr) with _ -> None in
         match pointer_opt with
         | Some (p, _) when Sym.equal p var_sym ->
           let offset_it = compute_offset_it var_sym it_addr in
           let b_off, s_off, e_off = transform_it filename sigma name offset_it in
           let (A.AnnotatedExpression (_, _, _, e_off_)) = e_off in
           let raw_offset_expr =
             mk_expr (CtA.wrap_with_convert_from e_off_ Memory.size_bt)
           in
           let offset_str =
             CF.Pp_utils.to_plain_string
               CF.Pp_ail.(with_executable_spec pp_expression raw_offset_expr)
           in
           let b_extra, s_extra, bytes_str =
             match max_addr with
             | Some it_max when not (Sym.Set.mem var_sym (T.free_vars it_max)) ->
               let loc = Locations.other __LOC__ in
               let end_bytes_it =
                 MT.mul_ (MT.cast_ Memory.size_bt it_max loc, MT.sizeOf_ sct loc) loc
               in
               let range_size_it = MT.sub_ (end_bytes_it, offset_it) loc in
               let b_range, s_range, e_range =
                 transform_it filename sigma name range_size_it
               in
               let (A.AnnotatedExpression (_, _, _, e_range_)) = e_range in
               let raw_range_expr =
                 mk_expr (CtA.wrap_with_convert_from e_range_ Memory.size_bt)
               in
               ( b_range,
                 s_range,
                 CF.Pp_utils.to_plain_string
                   CF.Pp_ail.(with_executable_spec pp_expression raw_range_expr) )
             | _ -> ([], [], "sizeof(" ^ sct_to_c_type_string sct ^ ")")
           in
           let blame_vars =
             let open Pp in
             let addr_fvs = T.free_vars it_addr in
             let max_fvs =
               match max_addr with
               | Some it_max -> T.free_vars it_max
               | None -> Sym.Set.empty
             in
             Sym.Set.union addr_fvs max_fvs
             |> Sym.Set.remove var_sym
             |> Sym.Set.to_seq
             |> List.of_seq
             |> concat_map (fun v -> !^(", " ^ plain (Sym.pp v)))
           in
           let bwd_stmts, num_other, addr_term_name, ids_name, syms_name =
             generate_bwd_blame_parts filename sigma it_addr var_sym
           in
           let macro_call =
             !^"BENNET_REFINE_ASSIGNMENT"
             ^^ parens
                  (!^"uintptr_t"
                   ^^ comma
                   ^^^ Sym.pp var_sym
                   ^^ comma
                   ^^^ !^offset_str
                   ^^ comma
                   ^^^ !^bytes_str
                   ^^ comma
                   ^^^ Sym.pp last_var
                   ^^ comma
                   ^^^ !^addr_term_name
                   ^^ comma
                   ^^^ int num_other
                   ^^ comma
                   ^^^ !^ids_name
                   ^^ comma
                   ^^^ !^syms_name
                   ^^ blame_vars
                   ^^ !^", NULL")
           in
           let s_macro =
             A.AilSexpr (mk_expr (A.AilEident (Sym.fresh (plain macro_call))))
           in
           (b_off @ b_extra @ acc_b, s_off @ s_extra @ bwd_stmts @ [ s_macro ] @ acc_s)
         | _ -> (acc_b, acc_s))
      asgns
      ([], [])


  (** Generate cn_term construction code for a constraint with two symbolic variables:
      the target variable and one additional variable kept symbolic. *)
  let generate_constraint_term_two_syms
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        ~(target : Sym.t)
        ~(target_bt : BT.t)
        ~(also_sym : Sym.t)
        ~(also_sym_expr : Pp.document)
        (constraint_it : T.t)
    : Pp.document
    =
    let open Pp in
    let free_vars_bts = T.free_vars_bts constraint_it in
    let protected = Sym.Set.of_list [ target; also_sym ] in
    let optimized_it = optimize_concrete_terms filename sigma protected constraint_it in
    (* Substitute free variables except target and also_sym with value reads *)
    let substituted_it =
      Sym.Map.fold
        (fun sym bt acc ->
           if Sym.equal sym target || Sym.equal sym also_sym then
             acc
           else (
             let sym_it = MT.sym_ (sym, bt, Locations.other __LOC__) in
             let value_read_str = plain (generate_value_read filename sigma sym_it) in
             let value_sym = Sym.fresh value_read_str in
             T.subst (T.make_rename ~from:sym ~to_:value_sym) acc))
        free_vars_bts
        optimized_it
    in
    (* Replace target with cn_smt_sym *)
    let target_sym_str =
      !^"cn_smt_sym"
      ^^ parens
           (parens !^"cn_sym"
            ^^ braces
                 (!^".name = "
                  ^^ dquotes (Sym.pp target)
                  ^^ comma
                  ^^^ !^".id = "
                  ^^ int (Sym.num target))
            ^^ comma
            ^^^ Smt.convert_basetype target_bt)
    in
    let target_value_sym = Sym.fresh (plain target_sym_str) in
    let it_after_target =
      T.subst (T.make_rename ~from:target ~to_:target_value_sym) substituted_it
    in
    (* Replace also_sym with its symbolic expression *)
    let also_value_sym = Sym.fresh (plain also_sym_expr) in
    let final_it =
      T.subst (T.make_rename ~from:also_sym ~to_:also_value_sym) it_after_target
    in
    Smt.convert_indexterm sigma final_it


  (** Generate backward abstract interpretation refinements for constraint refinement.
      Emitted between BENNET_REFINE_CONSTRAINT_BEGIN and BENNET_REFINE_CONSTRAINT_END,
      inside the [if (_refine_is_bottom)] guard. Uses the pre-refine domain (var_cs)
      and _refine_bt that are in scope from the BEGIN macro. *)
  let generate_constraint_backward_refinements
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        ~(var_sym : Sym.t)
        (constraint_it : T.t)
    : CF.GenTypes.genTypeCategory A.statement_ list
    =
    let open Pp in
    (* var_sym hasn't been sampled yet during domain refinement, so generate it
       as a symbolic reference using _refine_sym/_refine_bt from the BEGIN macro *)
    let var_sym_expr =
      !^"cn_smt_sym((cn_sym){.name = _refine_sym.name, .id = _refine_sym.id}, _refine_bt)"
    in
    let refinable_free_vars =
      T.free_vars_bts constraint_it
      |> Sym.Map.remove var_sym
      |> Sym.Map.filter (fun _ -> Stage6.Term.is_arbitrary_supported_bt)
      |> Sym.Map.bindings
    in
    if List.is_empty refinable_free_vars then
      []
    else (
      let per_var_stmts =
        refinable_free_vars
        |> List.map (fun (v, v_bt) ->
          let v_c_ty = bt_to_domain_type_string v_bt in
          let v_base_type_expr = Smt.convert_basetype v_bt in
          let constraint_term =
            generate_constraint_term_two_syms
              filename
              sigma
              ~target:v
              ~target_bt:v_bt
              ~also_sym:var_sym
              ~also_sym_expr:var_sym_expr
              constraint_it
          in
          let macro_call =
            !^"BENNET_REFINE_CONSTRAINT_BACKWARD"
            ^^ parens
                 (!^v_c_ty
                  ^^ comma
                  ^^^ Sym.pp v
                  ^^ comma
                  ^^^ parens
                        (parens !^"bennet_absint_sym"
                         ^^ braces
                              (!^".name = "
                               ^^ dquotes (Sym.pp v)
                               ^^ comma
                               ^^^ !^".id = "
                               ^^ int (Sym.num v)))
                  ^^ comma
                  ^^^ v_base_type_expr
                  ^^ comma
                  ^^^ constraint_term
                  ^^ comma
                  ^^^ Sym.pp var_sym
                  ^^ !^"_cs")
          in
          A.AilSexpr (mk_expr (AilEident (Sym.fresh (plain macro_call)))))
      in
      per_var_stmts)


  (** Generate BENNET_REFINE_CONSTRAINT calls for a list of constraints *)
  let generate_constraint_refinements
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        ~(var_sym : Sym.t)
        ~(var_bt : BT.t)
        ~(last_var : Sym.t)
        (constraints : T.t list)
    : CF.GenTypes.genTypeCategory A.statement_ list
    =
    let open Pp in
    let c_ty = bt_to_domain_type_string var_bt in
    (* Generate base type using Smt.convert_basetype *)
    let base_type_expr = Smt.convert_basetype var_bt in
    constraints
    |> List.concat_map (fun constraint_it ->
      let constraint_term =
        generate_constraint_term
          filename
          sigma
          ~target:var_sym
          ~target_bt:var_bt
          constraint_it
      in
      let blame_vars =
        T.free_vars constraint_it
        |> Sym.Set.remove var_sym
        |> Sym.Set.to_seq
        |> List.of_seq
        |> concat_map (fun v -> !^(", " ^ plain (Sym.pp v)))
      in
      if TestGenConfig.has_dynamic_arbitrary_propagation () then (
        let macro_begin =
          !^"BENNET_REFINE_CONSTRAINT_BEGIN"
          ^^ parens
               (!^c_ty
                ^^ comma
                ^^^ Sym.pp var_sym
                ^^ comma
                ^^^ parens
                      (parens !^"cn_sym"
                       ^^ braces
                            (!^".name = "
                             ^^ dquotes (Sym.pp var_sym)
                             ^^ comma
                             ^^^ !^".id = "
                             ^^ int (Sym.num var_sym)))
                ^^ comma
                ^^^ base_type_expr
                ^^ comma
                ^^^ constraint_term
                ^^ comma
                ^^^ Sym.pp last_var
                ^^ blame_vars
                ^^ !^", NULL")
        in
        let s_begin =
          [ A.AilSexpr (mk_expr (AilEident (Sym.fresh (plain macro_begin)))) ]
        in
        let s_backward =
          generate_constraint_backward_refinements filename sigma ~var_sym constraint_it
        in
        let macro_end =
          !^"BENNET_REFINE_CONSTRAINT_END"
          ^^ parens (!^c_ty ^^ comma ^^^ Sym.pp var_sym ^^ comma ^^^ Sym.pp last_var)
        in
        let s_end = [ A.AilSexpr (mk_expr (AilEident (Sym.fresh (plain macro_end)))) ] in
        s_begin @ s_backward @ s_end)
      else (
        let macro_call =
          !^"BENNET_REFINE_CONSTRAINT"
          ^^ parens
               (!^c_ty
                ^^ comma
                ^^^ Sym.pp var_sym
                ^^ comma
                ^^^ parens
                      (parens !^"cn_sym"
                       ^^ braces
                            (!^".name = "
                             ^^ dquotes (Sym.pp var_sym)
                             ^^ comma
                             ^^^ !^".id = "
                             ^^ int (Sym.num var_sym)))
                ^^ comma
                ^^^ base_type_expr
                ^^ comma
                ^^^ constraint_term
                ^^ comma
                ^^^ Sym.pp last_var
                ^^ blame_vars
                ^^ !^", NULL")
        in
        [ A.AilSexpr (mk_expr (AilEident (Sym.fresh (plain macro_call)))) ]))


  let pp_relative (bt : BT.t) (r : AD.Relative.t) =
    let open Pp in
    let cty =
      match bt with
      | Loc () -> "uintptr_t"
      | Bits (Signed, sz) -> Printf.sprintf "int%d_t" sz
      | Bits (Unsigned, sz) -> Printf.sprintf "uint%d_t" sz
      | _ -> failwith ("unsupported type: " ^ Pp.plain (BaseTypes.pp bt))
    in
    if AD.Relative.is_top r then
      !^(Printf.sprintf "bennet_domain_%s_top(%s)" AD.CInt.name cty)
    else if AD.Relative.is_bottom r then
      !^(Printf.sprintf "bennet_domain_%s_bottom(%s)" AD.CInt.name cty)
    else
      !^(Printf.sprintf
           "bennet_domain_%s_of(%s, %s)"
           AD.CInt.name
           cty
           (AD.Relative.pp_args r))


  (** Generate BENNET_ASSERT_DOMAIN_EVAL_CONSTRAINT calls for blame-first phase *)
  let generate_eval_constraints
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (name : Sym.t)
        ~(free_vars : (Sym.t * BT.t) list)
        (constraints : T.t list)
    : A.bindings * CF.GenTypes.genTypeCategory A.statement_ list
    =
    let free_var_set =
      List.fold_left (fun acc (x, _) -> Sym.Set.add x acc) Sym.Set.empty free_vars
    in
    List.fold_right
      (fun constraint_it (acc_b, acc_s) ->
         let b_cond, s_cond, e_cond = transform_it filename sigma name constraint_it in
         let blame_syms =
           T.free_vars constraint_it
           |> Sym.Set.inter free_var_set
           |> Sym.Set.to_seq
           |> List.of_seq
         in
         let s_eval =
           A.AilSexpr
             (mk_expr
                (AilEcall
                   ( mk_expr (string_ident "BENNET_ASSERT_DOMAIN_EVAL_CONSTRAINT"),
                     [ e_cond ]
                     @ List.map (fun x -> mk_expr (AilEident x)) blame_syms
                     @ [ mk_expr (AilEconst ConstantNull) ] )))
         in
         (b_cond @ acc_b, s_cond @ [ s_eval ] @ acc_s))
      constraints
      ([], [])


  (** Generate BENNET_ASSERT_DOMAIN_EVAL_ASSIGNMENT calls for blame-first phase *)
  let generate_eval_assignments
        (filename : string)
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (name : Sym.t)
        ~(free_vars : (Sym.t * BT.t) list)
        ~(ad : AD.t)
        (asgns : (T.t * Sctypes.t * T.t option) list)
    : A.bindings * CF.GenTypes.genTypeCategory A.statement_ list
    =
    let open Pp in
    (* Only process Loc() variables *)
    let loc_vars = List.filter (fun (_, bt) -> BT.equal bt (BT.Loc ())) free_vars in
    List.fold_right
      (fun (var_sym, _var_bt) (acc_b, acc_s) ->
         let rel = AD.relative_to var_sym (BT.Loc ()) ad in
         let domain_expr = plain (pp_relative (BT.Loc ()) rel) in
         let domain_cast = "(bennet_domain(uintptr_t)*)" ^ domain_expr in
         List.fold_right
           (fun (it_addr, sct, max_addr) (acc_b2, acc_s2) ->
              let pointer_opt = try Some (pointer_of it_addr) with _ -> None in
              match pointer_opt with
              | Some (p, _) when Sym.equal p var_sym ->
                let b_addr, s_addr, e_addr = transform_it filename sigma name it_addr in
                let addr_sym =
                  match e_addr with
                  | A.AnnotatedExpression (_, _, _, A.AilEident sym) -> sym
                  | _ -> failwith "generate_eval_assignments: expected AilEident"
                in
                let base_ptr_str =
                  "(void*)convert_from_cn_pointer(" ^ Sym.pp_string var_sym ^ ")"
                in
                let addr_str =
                  "(void*)convert_from_cn_pointer(" ^ Sym.pp_string addr_sym ^ ")"
                in
                let b_extra, s_extra, bytes_str =
                  match max_addr with
                  | Some it_max when not (Sym.Set.mem var_sym (T.free_vars it_max)) ->
                    let loc = Locations.other __LOC__ in
                    let end_bytes_it =
                      MT.mul_ (MT.cast_ Memory.size_bt it_max loc, MT.sizeOf_ sct loc) loc
                    in
                    let start_offset_it = compute_offset_it var_sym it_addr in
                    let range_size_it = MT.sub_ (end_bytes_it, start_offset_it) loc in
                    let b_range, s_range, e_range =
                      transform_it filename sigma name range_size_it
                    in
                    let (A.AnnotatedExpression (_, _, _, e_range_)) = e_range in
                    let raw_range_expr =
                      mk_expr (CtA.wrap_with_convert_from e_range_ Memory.size_bt)
                    in
                    ( b_range,
                      s_range,
                      CF.Pp_utils.to_plain_string
                        CF.Pp_ail.(with_executable_spec pp_expression raw_range_expr) )
                  | _ -> ([], [], "sizeof(" ^ sct_to_c_type_string sct ^ ")")
                in
                let bwd_stmts, num_other, addr_term_name, ids_name, syms_name =
                  generate_bwd_blame_parts filename sigma it_addr var_sym
                in
                let macro_call =
                  !^"BENNET_ASSERT_DOMAIN_EVAL_ASSIGNMENT"
                  ^^ parens
                       (!^"uintptr_t"
                        ^^ comma
                        ^^^ Sym.pp var_sym
                        ^^ comma
                        ^^^ !^base_ptr_str
                        ^^ comma
                        ^^^ !^addr_str
                        ^^ comma
                        ^^^ !^bytes_str
                        ^^ comma
                        ^^^ !^domain_cast
                        ^^ comma
                        ^^^ !^addr_term_name
                        ^^ comma
                        ^^^ int num_other
                        ^^ comma
                        ^^^ !^ids_name
                        ^^ comma
                        ^^^ !^syms_name)
                in
                let s_macro =
                  A.AilSexpr (mk_expr (A.AilEident (Sym.fresh (plain macro_call))))
                in
                ( b_addr @ b_extra @ acc_b2,
                  s_addr @ s_extra @ bwd_stmts @ [ s_macro ] @ acc_s2 )
              | _ -> (acc_b2, acc_s2))
           asgns
           (acc_b, acc_s))
      loc_vars
      ([], [])


  (** Generate BENNET_ASSERT_DOMAIN_REFINE_ASSIGNMENT calls for AssertDomainElab context *)
  let generate_assert_domain_assignment_refinements
        (filename : string)
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        ~(var_sym : Sym.t)
        ~(backtrack_var : Sym.t)
        (name : Sym.t)
        (asgns : (T.t * Sctypes.t * T.t option) list)
    : A.bindings * CF.GenTypes.genTypeCategory A.statement_ list
    =
    let open Pp in
    List.fold_right
      (fun (it_addr, sct, max_addr) (acc_b, acc_s) ->
         let pointer_opt = try Some (pointer_of it_addr) with _ -> None in
         match pointer_opt with
         | Some (p, _) when Sym.equal p var_sym ->
           let b_addr, s_addr, e_addr = transform_it filename sigma name it_addr in
           let addr_sym =
             match e_addr with
             | A.AnnotatedExpression (_, _, _, A.AilEident sym) -> sym
             | _ ->
               failwith
                 "generate_assert_domain_assignment_refinements: expected AilEident"
           in
           let base_ptr_str =
             "(void*)convert_from_cn_pointer(" ^ Sym.pp_string var_sym ^ ")"
           in
           let addr_str =
             "(void*)convert_from_cn_pointer(" ^ Sym.pp_string addr_sym ^ ")"
           in
           let b_extra, s_extra, bytes_str =
             match max_addr with
             | Some it_max when not (Sym.Set.mem var_sym (T.free_vars it_max)) ->
               (* For array range assignments, bytes = it_max * sizeof(sct) - start_offset *)
               let loc = Locations.other __LOC__ in
               let end_bytes_it =
                 MT.mul_ (MT.cast_ Memory.size_bt it_max loc, MT.sizeOf_ sct loc) loc
               in
               let start_offset_it = compute_offset_it var_sym it_addr in
               let range_size_it = MT.sub_ (end_bytes_it, start_offset_it) loc in
               let b_range, s_range, e_range =
                 transform_it filename sigma name range_size_it
               in
               let (A.AnnotatedExpression (_, _, _, e_range_)) = e_range in
               let raw_range_expr =
                 mk_expr (CtA.wrap_with_convert_from e_range_ Memory.size_bt)
               in
               ( b_range,
                 s_range,
                 CF.Pp_utils.to_plain_string
                   CF.Pp_ail.(with_executable_spec pp_expression raw_range_expr) )
             | _ -> ([], [], "sizeof(" ^ sct_to_c_type_string sct ^ ")")
           in
           let bwd_stmts, num_other, addr_term_name, ids_name, syms_name =
             generate_bwd_blame_parts filename sigma it_addr var_sym
           in
           let macro_call =
             !^"BENNET_ASSERT_DOMAIN_REFINE_ASSIGNMENT"
             ^^ parens
                  (!^"uintptr_t"
                   ^^ comma
                   ^^^ Sym.pp backtrack_var
                   ^^ comma
                   ^^^ Sym.pp var_sym
                   ^^ comma
                   ^^^ !^base_ptr_str
                   ^^ comma
                   ^^^ !^addr_str
                   ^^ comma
                   ^^^ !^bytes_str
                   ^^ comma
                   ^^^ !^addr_term_name
                   ^^ comma
                   ^^^ int num_other
                   ^^ comma
                   ^^^ !^ids_name
                   ^^ comma
                   ^^^ !^syms_name)
           in
           let s_macro =
             A.AilSexpr (mk_expr (A.AilEident (Sym.fresh (plain macro_call))))
           in
           (b_addr @ b_extra @ acc_b, s_addr @ s_extra @ bwd_stmts @ [ s_macro ] @ acc_s)
         | _ -> (acc_b, acc_s))
      asgns
      ([], [])


  (** Generate BENNET_ASSERT_DOMAIN_REFINE_CONSTRAINT calls for assert_domain context *)
  let generate_assert_domain_constraint_refinements
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        ~(var_sym : Sym.t)
        ~(var_bt : BT.t)
        ~(backtrack_var : Sym.t)
        (constraints : T.t list)
    : CF.GenTypes.genTypeCategory A.statement_ list
    =
    let open Pp in
    let c_ty = bt_to_domain_type_string var_bt in
    let base_type_expr = Smt.convert_basetype var_bt in
    constraints
    |> List.map (fun constraint_it ->
      let constraint_term =
        generate_constraint_term
          filename
          sigma
          ~target:var_sym
          ~target_bt:var_bt
          constraint_it
      in
      let blame_vars =
        T.free_vars constraint_it
        |> Sym.Set.to_seq
        |> List.of_seq
        |> concat_map (fun x -> !^(", " ^ plain (Sym.pp x)))
      in
      let macro_call =
        !^"BENNET_ASSERT_DOMAIN_REFINE_CONSTRAINT"
        ^^ parens
             (!^c_ty
              ^^ comma
              ^^^ Sym.pp backtrack_var
              ^^ comma
              ^^^ Sym.pp var_sym
              ^^ comma
              ^^^ parens
                    (parens !^"cn_sym"
                     ^^ braces
                          (!^".name = "
                           ^^ dquotes (Sym.pp var_sym)
                           ^^ comma
                           ^^^ !^".id = "
                           ^^ int (Sym.num var_sym)))
              ^^ comma
              ^^^ base_type_expr
              ^^ comma
              ^^^ constraint_term
              ^^ blame_vars
              ^^ !^", NULL")
      in
      A.AilSexpr (mk_expr (AilEident (Sym.fresh (plain macro_call)))))


  let generate_let_return_backward_refinements
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        ~(x_sym : Sym.t)
        ~(x_bt : BT.t)
        ~(refinable_free_vars : (Sym.t * BT.t) list)
        (expr_it : T.t)
    : CF.GenTypes.genTypeCategory A.statement_ list
    =
    let open Pp in
    let x_c_ty = bt_to_domain_type_string x_bt in
    let x_base_type_expr = Smt.convert_basetype x_bt in
    (* Generate per-variable backward blocks *)
    let per_var_blocks =
      refinable_free_vars
      |> List.filter_map (fun (v, v_bt) ->
        let v_c_ty = bt_to_domain_type_string v_bt in
        if not (String.equal v_c_ty x_c_ty) then
          None
        else
          Some
            (let v_base_type_expr = Smt.convert_basetype v_bt in
             let constraint_term =
               generate_constraint_term filename sigma ~target:v ~target_bt:v_bt expr_it
             in
             braces
               (nest
                  2
                  (hardline
                   ^^ !^"bennet_absint_sym _lr_sym_v = "
                   ^^ braces
                        (!^".name = "
                         ^^ dquotes (Sym.pp v)
                         ^^ comma
                         ^^^ !^".id = "
                         ^^ int (Sym.num v))
                   ^^ semi
                   ^^ hardline
                   ^^ !^"cn_base_type _lr_bt_v = "
                   ^^ v_base_type_expr
                   ^^ semi
                   ^^ hardline
                   ^^ !^"cn_term* _lr_expr = "
                   ^^ constraint_term
                   ^^ semi
                   ^^ hardline
                   ^^ !^(Printf.sprintf
                           "bennet_domain(%s)* _lr_D_v = \
                            (bennet_domain(%s)*)bennet_domain_transform_backward(%s, \
                            _lr_expr, _lr_sym_v, &_lr_bt_x, &_lr_bt_v, _lr_D_x);"
                           v_c_ty
                           v_c_ty
                           x_c_ty)
                   ^^ hardline
                   ^^ !^(Printf.sprintf
                           "if (_lr_D_v != NULL && !bennet_domain_is_bottom(%s, _lr_D_v) \
                            && !bennet_domain_is_top(%s, _lr_D_v)) {"
                           v_c_ty
                           v_c_ty)
                   ^^ nest
                        2
                        (hardline
                         ^^ !^(Printf.sprintf "bennet_failure_blame_domain(%s, " v_c_ty)
                         ^^ Sym.pp v
                         ^^ !^", _lr_D_v);")
                   ^^ hardline
                   ^^ !^"}")
                ^^ hardline)))
      |> separate hardline
    in
    (* Wrap in: { D_x = get_domain; if (D_x != NULL && !is_top) { ... per-var ... } } *)
    let full_block =
      braces
        (nest
           2
           (hardline
            ^^ !^(Printf.sprintf
                    "bennet_domain(%s)* _lr_D_x = bennet_failure_get_domain(%s, "
                    x_c_ty
                    x_c_ty)
            ^^ Sym.pp x_sym
            ^^ !^");"
            ^^ hardline
            ^^ !^(Printf.sprintf
                    "if (_lr_D_x != NULL && !bennet_domain_is_top(%s, _lr_D_x)) {"
                    x_c_ty)
            ^^ nest
                 2
                 (hardline
                  ^^ !^"cn_base_type _lr_bt_x = "
                  ^^ x_base_type_expr
                  ^^ semi
                  ^^ hardline
                  ^^ per_var_blocks)
            ^^ hardline
            ^^ !^"}")
         ^^ hardline)
    in
    [ A.AilSexpr (mk_expr (AilEident (Sym.fresh (plain full_block)))) ]


  let rec transform_term
            (filename : string)
            (sigma : CF.GenTypes.genTypeCategory A.sigma)
            (ctx : Stage6.Ctx.t)
            (name : Sym.t)
            (tm : Stage6.Term.t)
    : A.bindings
      * CF.GenTypes.genTypeCategory A.statement_ list
      * CF.GenTypes.genTypeCategory A.expression
    =
    let (Annot (tm_, (path_vars, last_var), bt, _)) = tm in
    match tm_ with
    | `Arbitrary ->
      (match bt with
       | Loc () -> ([], [], mk_expr (string_call "BENNET_ARBITRARY_POINTER" []))
       | Bits (sign, bits) ->
         let sign_str = match sign with Unsigned -> "UNSIGNED" | Signed -> "SIGNED" in
         ( [],
           [],
           mk_expr
             (string_call
                ("BENNET_ARBITRARY_" ^ sign_str)
                [ mk_expr
                    (AilEconst
                       (ConstantInteger (IConstant (Z.of_int bits, Decimal, None))))
                ]) )
       | _ ->
         failwith
           (Printf.sprintf
              "Arbitrary: only pointer and bitvector types are supported, got %s"
              (Pp.plain (BT.pp bt))))
    | `Symbolic -> failwith "TODO"
    | `ArbitraryDomain _ ->
      (match bt with
       | Loc () -> ([], [], mk_expr (string_call "BENNET_ARBITRARY_POINTER" []))
       | Bits (sign, bits) ->
         let sign_str = match sign with Unsigned -> "UNSIGNED" | Signed -> "SIGNED" in
         ( [],
           [],
           mk_expr
             (string_call
                ("BENNET_ARBITRARY_" ^ sign_str)
                [ mk_expr
                    (AilEconst
                       (ConstantInteger (IConstant (Z.of_int bits, Decimal, None))))
                ]) )
       | _ ->
         failwith
           (Printf.sprintf
              "ArbitraryDomain: only pointer and bitvector types are supported, got %s"
              (Pp.plain (BT.pp bt))))
    | `ArbitrarySpecialized ((min_inc, min_ex), (max_inc, max_ex)) ->
      let cn_ty =
        match bt with
        | Loc () -> "cn_pointer"
        | Bits (sign, bits) ->
          let sign_char = match sign with Unsigned -> 'u' | Signed -> 'i' in
          Printf.sprintf "cn_bits_%c%d" sign_char bits
        | _ ->
          failwith
            (Printf.sprintf
               "ArbitrarySpecialized: only pointer and bitvector types are supported, \
                got %s"
               (Pp.plain (BT.pp bt)))
      in
      (* Build argument expressions for bounds - NULL for None *)
      let mk_bound_arg = function
        | None -> mk_expr (AilEconst ConstantNull)
        | Some it ->
          let bs, ss, e = transform_it filename sigma name it in
          mk_expr (AilEgcc_statement (bs, List.map mk_stmt (ss @ [ A.AilSexpr e ])))
      in
      let free_vars_from_bounds =
        List.fold_left
          Sym.Set.union
          Sym.Set.empty
          (List.filter_map (Option.map T.free_vars) [ min_inc; min_ex; max_inc; max_ex ])
      in
      ( [],
        [],
        mk_expr
          (string_call
             "BENNET_SPECIALIZED"
             ([ mk_expr (string_ident cn_ty);
                mk_bound_arg min_ex;
                mk_bound_arg min_inc;
                mk_bound_arg max_inc;
                mk_bound_arg max_ex;
                mk_expr (AilEident last_var)
              ]
              @ List.map
                  (fun x -> mk_expr (AilEident x))
                  (List.of_seq (Sym.Set.to_seq free_vars_from_bounds))
              @ [ mk_expr (AilEconst ConstantNull) ])) )
    | `PickSizedElab (choice_var, wgts) ->
      let var = Sym.fresh_anon () in
      let bs, ss =
        List.split
          (List.mapi
             (fun i (_, gr) ->
                let bs, ss, e = transform_term filename sigma ctx name gr in
                ( bs,
                  A.(
                    [ AilSexpr
                        (mk_expr
                           (AilEcall
                              ( mk_expr (string_ident "BENNET_PICK_CASE_BEGIN"),
                                List.map
                                  mk_expr
                                  [ AilEconst
                                      (ConstantInteger
                                         (IConstant (Z.of_int i, Decimal, None)))
                                  ] )))
                    ]
                    @ ss
                    @ [ AilSexpr
                          (mk_expr
                             (AilEcall
                                ( mk_expr (string_ident "BENNET_PICK_CASE_END"),
                                  [ mk_expr (AilEident var); e ] )))
                      ]) ))
             wgts)
      in
      ( List.flatten bs,
        A.
          [ AilSexpr
              (mk_expr
                 (AilEcall
                    ( mk_expr (string_ident "BENNET_PICK_BEGIN"),
                      List.map
                        mk_expr
                        [ AilEident (Sym.fresh (name_of_bt bt));
                          AilEident var;
                          AilEident choice_var;
                          AilEident last_var
                        ]
                      @ List.flatten
                          (List.mapi
                             (fun i (w, _) ->
                                List.map
                                  mk_expr
                                  [ AilEconst
                                      (ConstantInteger (IConstant (w, Decimal, None)));
                                    AilEconst
                                      (ConstantInteger
                                         (IConstant (Z.of_int i, Decimal, None)))
                                  ])
                             wgts) )))
          ]
        @ List.flatten ss
        @ [ AilSexpr
              (mk_expr
                 (AilEcall
                    ( mk_expr (string_ident "BENNET_PICK_END"),
                      [ mk_expr (AilEident choice_var) ] )))
          ],
        A.(mk_expr (AilEident var)) )
    | `Call (fsym, iargs) ->
      (match Stage6.Ctx.find_opt fsym ctx with
       | Some _ -> ()
       | None ->
         failwith (Printf.sprintf "Function %s not found in context" (Sym.pp_string fsym)));
      let sym = GenUtils.get_mangled_name fsym in
      let bs, ss, es =
        iargs
        |> List.map (fun x ->
          let bs, ss, e = transform_it filename sigma name x in
          (bs, ss, e))
        |> List.fold_left
             (fun (bs, ss, es) (b, s, e) -> (bs @ b, ss @ s, es @ [ e ]))
             ([], [], [])
      in
      let sized_call =
        A.(
          if (Stage6.Ctx.find fsym ctx).recursive then
            [ AilEcall (mk_expr (string_ident "bennet_get_size"), []) ]
          else
            [])
      in
      let es = es @ List.map mk_expr sized_call in
      ( bs,
        ss,
        mk_expr
          (AilEgcc_statement
             ( [],
               [ mk_stmt
                   (AilSexpr
                      (mk_expr
                         (AilEident
                            (Sym.fresh
                               ("const void* path_vars[] = { "
                                ^ String.concat
                                    ", "
                                    ((path_vars
                                      |> Sym.Set.to_seq
                                      |> List.of_seq
                                      |> List.map Sym.pp_string)
                                     @ [ "NULL" ])
                                ^ " }")))));
                 mk_stmt
                   (AilSexpr
                      (mk_expr
                         (AilEcall
                            ( mk_expr (string_ident "BENNET_CALL"),
                              [ mk_expr (string_ident (name_of_bt bt));
                                mk_expr (AilEident last_var);
                                mk_expr (AilEcall (mk_expr (AilEident sym), es))
                              ] ))))
               ] )) )
    | `CallSized (fsym, iargs, (n, sym_size)) ->
      (match Stage6.Ctx.find_opt fsym ctx with
       | Some _ -> ()
       | None ->
         failwith (Printf.sprintf "Function %s not found in context" (Sym.pp_string fsym)));
      let sym = GenUtils.get_mangled_name fsym in
      let bs, ss, es =
        iargs
        |> List.map (fun x ->
          let bs, ss, e = transform_it filename sigma name x in
          (bs, ss, e))
        |> List.fold_left
             (fun (bs, ss, es) (b, s, e) -> (bs @ b, ss @ s, es @ [ e ]))
             ([], [], [])
      in
      let sized_call =
        A.(
          if n <= 0 then
            failwith "Invalid sized call"
          else if n = 1 then
            [ AilEbinary
                ( mk_expr (string_ident "bennet_rec_size"),
                  Arithmetic Sub,
                  mk_expr (AilEconst (ConstantInteger (IConstant (Z.one, Decimal, None))))
                )
            ]
          else if TestGenConfig.is_random_size_splits () then
            [ AilEident sym_size ]
          else
            [ AilEbinary
                ( mk_expr (string_ident "bennet_rec_size"),
                  Arithmetic Div,
                  mk_expr
                    (AilEconst (ConstantInteger (IConstant (Z.of_int n, Decimal, None))))
                )
            ])
      in
      let es = es @ List.map mk_expr sized_call in
      ( bs,
        ss,
        mk_expr
          (AilEgcc_statement
             ( [],
               [ mk_stmt
                   (AilSexpr
                      (mk_expr
                         (AilEident
                            (Sym.fresh
                               ("const void* path_vars[] = { "
                                ^ String.concat
                                    ", "
                                    ((path_vars
                                      |> Sym.Set.to_seq
                                      |> List.of_seq
                                      |> List.map Sym.pp_string)
                                     @ [ "NULL" ])
                                ^ " }")))));
                 mk_stmt
                   (AilSexpr
                      (mk_expr
                         (AilEcall
                            ( mk_expr (string_ident "BENNET_CALL"),
                              [ mk_expr (string_ident (name_of_bt bt));
                                mk_expr (AilEident last_var);
                                mk_expr (AilEcall (mk_expr (AilEident sym), es))
                              ] ))))
               ] )) )
    | `AsgnElab (_, (((p_sym, p_bt), it_addr), sct), it_val, gt_rest) ->
      let b_addr, s_addr, e_addr = transform_it filename sigma name it_addr in
      let b_value, s_value, AnnotatedExpression (_, _, _, e_value_) =
        transform_it filename sigma name it_val
      in
      let ptr_ty =
        match p_bt with
        | Loc () -> "uintptr_t"
        | Bits (Unsigned, sz) -> "uint" ^ string_of_int sz ^ "_t"
        | Bits (Signed, sz) -> "int" ^ string_of_int sz ^ "_t"
        | _ -> failwith ("Unsupported pointer type @ " ^ __LOC__)
      in
      let bwd_stmts, num_other, addr_term_name, ids_name, syms_name =
        generate_bwd_blame_parts filename sigma it_addr p_sym
      in
      let s_assign =
        A.
          [ AilSexpr
              (mk_expr
                 (AilEcall
                    ( mk_expr (string_ident "BENNET_ASSIGN"),
                      [ mk_expr (AilEident p_sym);
                        mk_expr
                          (let b, s =
                             let b, s, e =
                               transform_it
                                 filename
                                 sigma
                                 name
                                 (MT.cast_
                                    (BT.Loc ())
                                    (MT.sym_ (p_sym, p_bt, Locations.other __LOC__))
                                    (Locations.other __LOC__))
                             in
                             (b, List.map mk_stmt (s @ [ A.AilSexpr e ]))
                           in
                           A.AilEgcc_statement (b, s));
                        mk_expr (AilEident (Sym.fresh ptr_ty));
                        e_addr;
                        mk_expr
                          (string_ident
                             (CF.Pp_utils.to_plain_string
                                CF.Pp_ail.(
                                  with_executable_spec
                                    (pp_ctype C.no_qualifiers)
                                    (Sctypes.to_ctype sct))));
                        mk_expr
                          (CtA.wrap_with_convert_from ~sct e_value_ (T.get_bt it_val));
                        mk_expr (AilEident last_var);
                        mk_expr (AilEident (Sym.fresh addr_term_name));
                        mk_expr (AilEident (Sym.fresh (string_of_int num_other)));
                        mk_expr (AilEident (Sym.fresh ids_name));
                        mk_expr (AilEident (Sym.fresh syms_name))
                      ]
                      @ List.map
                          (fun x -> mk_expr (AilEident x))
                          (List.of_seq (Sym.Set.to_seq (T.free_vars it_addr)))
                      @ [ mk_expr (AilEconst ConstantNull) ] )))
          ]
      in
      let b_rest, s_rest, e_rest = transform_term filename sigma ctx name gt_rest in
      (b_addr @ b_value @ b_rest, s_addr @ s_value @ bwd_stmts @ s_assign @ s_rest, e_rest)
    | `LetStar
        ((x, GenTerms.Annot (`Arbitrary, _, (Bits (sign, bits) as x_bt), _)), gt_rest) ->
      let b_let = [ Utils.create_binding x (bt_to_ctype_for_binding x_bt) ] in
      let func_name =
        match sign with
        | Unsigned -> "BENNET_LET_ARBITRARY_UNSIGNED"
        | Signed -> "BENNET_LET_ARBITRARY_SIGNED"
      in
      let s_let =
        [ A.AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (string_ident func_name),
                    List.map
                      mk_expr
                      [ AilEconst
                          (ConstantInteger
                             (IConstant
                                ( Z.of_int (TestGenConfig.get_max_backtracks ()),
                                  Decimal,
                                  None )));
                        AilEconst
                          (ConstantInteger (IConstant (Z.of_int bits, Decimal, None)));
                        AilEident x;
                        AilEident last_var
                      ] )))
        ]
      in
      let b_rest, s_rest, e_rest = transform_term filename sigma ctx name gt_rest in
      (b_let @ b_rest, s_let @ s_rest, e_rest)
    | `LetStar ((_, GenTerms.Annot (`Symbolic, _, Bits (_, _), _)), _) ->
      failwith "TODO: LetStar Symbolic"
    | `LetStar
        ( ( x,
            GenTerms.Annot
              ( `ArbitrarySpecialized ((min_inc, min_ex), (max_inc, max_ex)),
                _,
                (Bits (sign, bits) as x_bt),
                _ ) ),
          gt_rest ) ->
      let b_let = [ Utils.create_binding x (bt_to_ctype_for_binding x_bt) ] in
      let s_let =
        let func_name =
          match sign with
          | Unsigned -> "BENNET_LET_SPECIALIZED_UNSIGNED"
          | Signed -> "BENNET_LET_SPECIALIZED_SIGNED"
        in
        let mk_bound_arg = function
          | None -> mk_expr (AilEconst ConstantNull)
          | Some it ->
            let bs, ss, e = transform_it filename sigma name it in
            mk_expr (AilEgcc_statement (bs, List.map mk_stmt (ss @ [ A.AilSexpr e ])))
        in
        let free_vars_from_bounds =
          List.fold_left
            Sym.Set.union
            Sym.Set.empty
            (List.filter_map
               (Option.map T.free_vars)
               [ min_inc; min_ex; max_inc; max_ex ])
        in
        [ A.AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (string_ident func_name),
                    [ mk_expr
                        (AilEconst
                           (ConstantInteger
                              (IConstant
                                 ( Z.of_int (TestGenConfig.get_max_backtracks ()),
                                   Decimal,
                                   None ))));
                      mk_expr
                        (AilEconst
                           (ConstantInteger (IConstant (Z.of_int bits, Decimal, None))));
                      mk_expr (AilEident x);
                      mk_expr (AilEident last_var);
                      mk_bound_arg min_ex;
                      mk_bound_arg min_inc;
                      mk_bound_arg max_inc;
                      mk_bound_arg max_ex
                    ]
                    @ List.map
                        (fun x -> mk_expr (AilEident x))
                        (List.of_seq (Sym.Set.to_seq free_vars_from_bounds))
                    @ [ mk_expr (AilEconst ConstantNull) ] )))
        ]
      in
      let b_rest, s_rest, e_rest = transform_term filename sigma ctx name gt_rest in
      (b_let @ b_rest, s_let @ s_rest, e_rest)
    | `LetStar
        ( ( x,
            GenTerms.Annot
              (`ArbitraryDomain (d, constraints, _), _, (Bits (sign, bits) as x_bt), _) ),
          gt_rest ) ->
      let b_let = [ Utils.create_binding x (bt_to_ctype_for_binding x_bt) ] in
      let cty =
        match x_bt with
        | Loc () -> "uintptr_t"
        | Bits (Signed, sz) -> Printf.sprintf "int%d_t" sz
        | Bits (Unsigned, sz) -> Printf.sprintf "uint%d_t" sz
        | _ -> failwith ("unsupported type: " ^ Pp.plain (BaseTypes.pp x_bt))
      in
      let domain_str = "(bennet_domain(" ^ cty ^ ")*)" ^ Pp.plain (pp_relative x_bt d) in
      let s_let =
        if TestGenConfig.has_dynamic_arbitrary_domain () && List.length constraints > 0
        then (
          (* Use BEGIN/REFINE/END pattern *)
          let begin_func_name =
            match sign with
            | Unsigned -> "BENNET_LET_ARBITRARY_DOMAIN_BEGIN_UNSIGNED"
            | Signed -> "BENNET_LET_ARBITRARY_DOMAIN_BEGIN_SIGNED"
          in
          let s_begin =
            [ A.AilSexpr
                (mk_expr
                   (AilEcall
                      ( mk_expr (string_ident begin_func_name),
                        List.map
                          mk_expr
                          [ AilEconst
                              (ConstantInteger (IConstant (Z.of_int bits, Decimal, None)));
                            AilEident x;
                            AilEident last_var;
                            AilEident (Sym.fresh domain_str)
                          ] )))
            ]
          in
          let sorted_constraints =
            List.stable_sort
              (fun a b ->
                 let fv_count c = Sym.Set.cardinal (Sym.Set.remove x (T.free_vars c)) in
                 Int.compare (fv_count a) (fv_count b))
              constraints
          in
          let s_refine =
            generate_constraint_refinements
              filename
              sigma
              ~var_sym:x
              ~var_bt:x_bt
              ~last_var
              sorted_constraints
          in
          let end_func_name =
            match sign with
            | Unsigned -> "BENNET_LET_ARBITRARY_DOMAIN_END_UNSIGNED"
            | Signed -> "BENNET_LET_ARBITRARY_DOMAIN_END_SIGNED"
          in
          let s_end =
            [ A.AilSexpr
                (mk_expr
                   (AilEcall
                      ( mk_expr (string_ident end_func_name),
                        List.map
                          mk_expr
                          [ AilEconst
                              (ConstantInteger
                                 (IConstant
                                    ( Z.of_int (TestGenConfig.get_max_backtracks ()),
                                      Decimal,
                                      None )));
                            AilEconst
                              (ConstantInteger (IConstant (Z.of_int bits, Decimal, None)));
                            AilEident x;
                            AilEident last_var
                          ] )))
            ]
          in
          s_begin @ s_refine @ s_end)
        else (
          (* Use original combined macro *)
          let func_name =
            match sign with
            | Unsigned -> "BENNET_LET_ARBITRARY_DOMAIN_UNSIGNED"
            | Signed -> "BENNET_LET_ARBITRARY_DOMAIN_SIGNED"
          in
          [ A.AilSexpr
              (mk_expr
                 (AilEcall
                    ( mk_expr (string_ident func_name),
                      List.map
                        mk_expr
                        [ AilEconst
                            (ConstantInteger
                               (IConstant
                                  ( Z.of_int (TestGenConfig.get_max_backtracks ()),
                                    Decimal,
                                    None )));
                          AilEconst
                            (ConstantInteger (IConstant (Z.of_int bits, Decimal, None)));
                          AilEident x;
                          AilEident last_var;
                          AilEident (Sym.fresh domain_str)
                        ] )))
          ])
      in
      let b_rest, s_rest, e_rest = transform_term filename sigma ctx name gt_rest in
      (b_let @ b_rest, s_let @ s_rest, e_rest)
    | `LetStar ((_, GenTerms.Annot (`Symbolic, _, Loc (), _)), _) ->
      failwith "TODO: LetStar Symbolic Loc"
    | `LetStar
        ( ( x,
            GenTerms.Annot
              ( `ArbitrarySpecialized ((min_inc, min_ex), (max_inc, max_ex)),
                _,
                (Loc () as x_bt),
                _ ) ),
          gt_rest ) ->
      let b_let = [ Utils.create_binding x (bt_to_ctype_for_binding x_bt) ] in
      let s_let =
        let mk_bound_arg = function
          | None -> mk_expr (AilEconst ConstantNull)
          | Some it ->
            let bs, ss, e = transform_it filename sigma name it in
            mk_expr (AilEgcc_statement (bs, List.map mk_stmt (ss @ [ A.AilSexpr e ])))
        in
        let free_vars_from_bounds =
          List.fold_left
            Sym.Set.union
            Sym.Set.empty
            (List.filter_map
               (Option.map T.free_vars)
               [ min_inc; min_ex; max_inc; max_ex ])
        in
        [ A.AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (string_ident "BENNET_LET_SPECIALIZED_POINTER"),
                    [ mk_expr
                        (AilEconst
                           (ConstantInteger
                              (IConstant
                                 ( Z.of_int (TestGenConfig.get_max_backtracks ()),
                                   Decimal,
                                   None ))));
                      mk_expr (AilEident x);
                      mk_expr (AilEident last_var);
                      mk_bound_arg min_ex;
                      mk_bound_arg min_inc;
                      mk_bound_arg max_inc;
                      mk_bound_arg max_ex
                    ]
                    @ List.map
                        (fun x -> mk_expr (AilEident x))
                        (List.of_seq (Sym.Set.to_seq free_vars_from_bounds))
                    @ [ mk_expr (AilEconst ConstantNull) ] )))
        ]
      in
      let b_rest, s_rest, e_rest = transform_term filename sigma ctx name gt_rest in
      (b_let @ b_rest, s_let @ s_rest, e_rest)
    | `LetStar ((x, GenTerms.Annot (`Arbitrary, _, (Loc () as x_bt), _)), gt_rest) ->
      let b_let = [ Utils.create_binding x (bt_to_ctype_for_binding x_bt) ] in
      let s_let =
        [ A.AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (string_ident "BENNET_LET_ARBITRARY_POINTER"),
                    List.map
                      mk_expr
                      [ AilEconst
                          (ConstantInteger
                             (IConstant
                                ( Z.of_int (TestGenConfig.get_max_backtracks ()),
                                  Decimal,
                                  None )));
                        AilEident x;
                        AilEident last_var
                      ] )))
        ]
      in
      let b_rest, s_rest, e_rest = transform_term filename sigma ctx name gt_rest in
      (b_let @ b_rest, s_let @ s_rest, e_rest)
    | `LetStar
        ( ( x,
            GenTerms.Annot
              (`ArbitraryDomain (d, constraints, asgns), _, (Loc () as x_bt), _) ),
          gt_rest ) ->
      let b_let = [ Utils.create_binding x (bt_to_ctype_for_binding x_bt) ] in
      let domain_str = "(bennet_domain(uintptr_t)*)" ^ Pp.plain (pp_relative x_bt d) in
      let b_asgn, s_let =
        if
          TestGenConfig.has_dynamic_arbitrary_domain ()
          && (List.length constraints > 0 || not (List.is_empty asgns))
        then (
          (* Use BEGIN/REFINE/END pattern *)
          let s_begin =
            [ A.AilSexpr
                (mk_expr
                   (AilEcall
                      ( mk_expr (string_ident "BENNET_LET_ARBITRARY_DOMAIN_BEGIN_POINTER"),
                        List.map
                          mk_expr
                          [ AilEident x;
                            AilEident last_var;
                            AilEident (Sym.fresh domain_str)
                          ] )))
            ]
          in
          let sorted_constraints =
            List.stable_sort
              (fun a b ->
                 let fv_count c = Sym.Set.cardinal (Sym.Set.remove x (T.free_vars c)) in
                 Int.compare (fv_count a) (fv_count b))
              constraints
          in
          let s_refine =
            generate_constraint_refinements
              filename
              sigma
              ~var_sym:x
              ~var_bt:x_bt
              ~last_var
              sorted_constraints
          in
          let sorted_asgns =
            List.stable_sort
              (fun (addr_a, _, max_a) (addr_b, _, max_b) ->
                 let fv_count (addr, _, max_opt) =
                   let fvs = T.free_vars addr in
                   let fvs =
                     match max_opt with
                     | Some m -> Sym.Set.union fvs (T.free_vars m)
                     | None -> fvs
                   in
                   Sym.Set.cardinal (Sym.Set.remove x fvs)
                 in
                 Int.compare (fv_count (addr_a, (), max_a)) (fv_count (addr_b, (), max_b)))
              asgns
          in
          let b_asgn, s_asgn =
            generate_assignment_refinements
              filename
              sigma
              ~var_sym:x
              ~last_var
              name
              sorted_asgns
          in
          let s_end =
            [ A.AilSexpr
                (mk_expr
                   (AilEcall
                      ( mk_expr (string_ident "BENNET_LET_ARBITRARY_DOMAIN_END_POINTER"),
                        List.map
                          mk_expr
                          [ AilEconst
                              (ConstantInteger
                                 (IConstant
                                    ( Z.of_int (TestGenConfig.get_max_backtracks ()),
                                      Decimal,
                                      None )));
                            AilEident x;
                            AilEident last_var
                          ] )))
            ]
          in
          (b_asgn, s_begin @ s_asgn @ s_refine @ s_end))
        else
          ( [],
            [ (* Use original combined macro *)
              A.AilSexpr
                (mk_expr
                   (AilEcall
                      ( mk_expr (string_ident "BENNET_LET_ARBITRARY_DOMAIN_POINTER"),
                        List.map
                          mk_expr
                          [ AilEconst
                              (ConstantInteger
                                 (IConstant
                                    ( Z.of_int (TestGenConfig.get_max_backtracks ()),
                                      Decimal,
                                      None )));
                            AilEident x;
                            AilEident last_var;
                            AilEident (Sym.fresh domain_str)
                          ] )))
            ] )
      in
      let b_rest, s_rest, e_rest = transform_term filename sigma ctx name gt_rest in
      (b_let @ b_asgn @ b_rest, s_let @ s_rest, e_rest)
    | `LetStar ((_, GenTerms.Annot (`Arbitrary, _, bt, _)), _) ->
      failwith ("unreachable @ " ^ __LOC__ ^ " with type: " ^ Pp.plain (BT.pp bt))
    | `LetStar ((_, GenTerms.Annot (`Symbolic, _, bt, _)), _) ->
      failwith ("unreachable @ " ^ __LOC__ ^ " with type: " ^ Pp.plain (BT.pp bt))
    | `LetStar ((_, GenTerms.Annot (`ArbitraryDomain _, _, bt, _)), _) ->
      failwith ("unreachable @ " ^ __LOC__ ^ " with type: " ^ Pp.plain (BT.pp bt))
    | `LetStar ((_, GenTerms.Annot (`ArbitrarySpecialized _, _, bt, _)), _) ->
      failwith ("unreachable @ " ^ __LOC__ ^ " with type: " ^ Pp.plain (BT.pp bt))
    | `LetStar ((x, GenTerms.Annot (`Return it, _, x_bt, _)), gt_rest) ->
      let b_value, s_value, e_value = transform_it filename sigma name it in
      let free_vars_set = T.free_vars it in
      let free_vars_list = List.of_seq (Sym.Set.to_seq free_vars_set) in
      let s_let =
        if
          TestGenConfig.has_dynamic_return_propagation ()
          && Stage6.Term.is_arbitrary_supported_bt x_bt
        then (
          (* Determine refinable free vars (Bits or Loc types) for backward interpretation.
         Only do backward interp when x_bt itself is Bits or Loc (not structs/records). *)
          let refinable_free_vars =
            T.free_vars_bts it
            |> Sym.Map.filter (fun _ -> Stage6.Term.is_arbitrary_supported_bt)
            |> Sym.Map.bindings
          in
          (* Use split BEGIN/END macros with backward interp in between *)
          let s_begin =
            A.
              [ AilSexpr
                  (mk_expr
                     (AilEcall
                        ( mk_expr (string_ident "BENNET_LET_RETURN_BEGIN"),
                          [ mk_expr (string_ident (name_of_bt x_bt));
                            mk_expr (AilEident x);
                            e_value
                          ] )))
              ]
          in
          (* Generate backward interpretation block as raw C *)
          let s_backward =
            generate_let_return_backward_refinements
              filename
              sigma
              ~x_sym:x
              ~x_bt
              ~refinable_free_vars
              it
          in
          (* END macro with blame propagation *)
          let s_end =
            A.
              [ AilSexpr
                  (mk_expr
                     (AilEcall
                        ( mk_expr (string_ident "BENNET_LET_RETURN_END"),
                          [ mk_expr (AilEident x); mk_expr (AilEident last_var) ]
                          @ List.map (fun x -> mk_expr (AilEident x)) free_vars_list
                          @ [ mk_expr (AilEconst ConstantNull) ] )))
              ]
          in
          s_begin @ s_backward @ s_end)
        else
          A.
            [ AilSexpr
                (mk_expr
                   (string_call
                      "BENNET_LET_RETURN"
                      ([ mk_expr (string_ident (name_of_bt x_bt));
                         mk_expr (AilEident x);
                         (* Below might cause issues if it contains a comma *)
                         e_value;
                         mk_expr (AilEident last_var)
                       ]
                       @ List.map (fun x -> mk_expr (AilEident x)) free_vars_list
                       @ [ mk_expr (AilEconst ConstantNull) ])))
            ]
      in
      let b_rest, s_rest, e_rest = transform_term filename sigma ctx name gt_rest in
      (b_value @ b_rest, s_value @ s_let @ s_rest, e_rest)
    | `LetStar ((x, (GenTerms.Annot (`Call _, _, x_bt, _) as gt_inner)), gt_rest)
    | `LetStar ((x, (GenTerms.Annot (`CallSized _, _, x_bt, _) as gt_inner)), gt_rest)
    | `LetStar ((x, (GenTerms.Annot (`MapElab _, _, x_bt, _) as gt_inner)), gt_rest)
    | `LetStar ((x, (GenTerms.Annot (`LetStar _, _, x_bt, _) as gt_inner)), gt_rest) ->
      let b_value, s_value, e_value = transform_term filename sigma ctx name gt_inner in
      let s_let =
        A.
          [ AilSexpr
              (mk_expr
                 (string_call
                    "BENNET_LET"
                    [ mk_expr
                        (AilEconst
                           (ConstantInteger
                              (IConstant
                                 ( Z.of_int (TestGenConfig.get_max_backtracks ()),
                                   Decimal,
                                   None ))));
                      mk_expr (string_ident (name_of_bt x_bt));
                      mk_expr (AilEident x);
                      mk_expr (AilEident last_var);
                      e_value
                    ]))
          ]
      in
      let b_rest, s_rest, e_rest = transform_term filename sigma ctx name gt_rest in
      (b_value @ b_rest, s_value @ s_let @ s_rest, e_rest)
    | `LetStar ((_, GenTerms.Annot (`PickSizedElab _, _, _, _)), _) ->
      failwith "Should be unreachable due to lifting of `pick`"
    | `LetStar ((_, GenTerms.Annot (`ITE _, _, _, _)), _) ->
      failwith "Should be unreachable due to lifting of `if-else`"
    | `LetStar ((_, GenTerms.Annot (`Assert _, _, _, _)), _) ->
      failwith "Should be unreachable due to lifting of `assert`"
    | `LetStar ((_, GenTerms.Annot (`AssertDomainElab _, _, _, _)), _) ->
      failwith "Should be unreachable due to lifting of `assert_domain`"
    | `LetStar ((_, GenTerms.Annot (`AsgnElab _, _, _, _)), _) ->
      failwith "Should be unreachable due to lifting of `assign`"
    | `LetStar ((_, GenTerms.Annot (`SplitSizeElab _, _, _, _)), _) ->
      failwith "Should be unreachable due to lifting"
    | `Return it ->
      let b, s, e = transform_it filename sigma name it in
      (b, s, e)
    | `Assert (lc, gt_rest) ->
      let b1, s1, e1 = transform_lc filename sigma lc in
      let s_assert =
        A.
          [ AilSexpr
              (mk_expr
                 (AilEcall
                    ( mk_expr (string_ident "BENNET_ASSERT"),
                      [ e1 ]
                      @ [ mk_expr (AilEident last_var) ]
                      @ List.map
                          (fun x -> mk_expr (AilEident x))
                          (List.of_seq (Sym.Set.to_seq (LC.free_vars lc)))
                      @ [ mk_expr (AilEconst ConstantNull) ] )))
          ]
      in
      let b2, s2, e2 = transform_term filename sigma ctx name gt_rest in
      (b1 @ b2, s1 @ s_assert @ s2, e2)
    | `AssertDomainElab (backtrack_var, ad, its, asgns, gt_rest) ->
      if not (TestGenConfig.is_runtime_assert_domain ()) then
        (* Skip assert_domain, just process the rest *)
        transform_term filename sigma ctx name gt_rest
      else (
        (* Get free variables from the abstract domain and from constraints *)
        let ad_free_vars = AD.free_vars_bts ad in
        let constraint_free_vars =
          T.free_vars_bts_list its
          |> Sym.Map.filter (fun _ -> Stage6.Term.is_arbitrary_supported_bt)
        in
        let all_free_vars =
          Sym.Map.union
            (fun _ bt1 bt2 ->
               assert (BT.equal bt1 bt2);
               Some bt1)
            ad_free_vars
            constraint_free_vars
        in
        let free_vars = all_free_vars |> Sym.Map.to_seq |> List.of_seq in
        let n = List.length free_vars in
        (* Generate BEGIN macro call *)
        let s_begin =
          A.
            [ AilSexpr
                (mk_expr
                   (AilEcall
                      ( mk_expr (string_ident "BENNET_ASSERT_DOMAIN_BEGIN"),
                        [ mk_expr (AilEident backtrack_var);
                          mk_expr
                            (AilEconst
                               (ConstantInteger (IConstant (Z.of_int n, Decimal, None))))
                        ] )))
            ]
        in
        (* Phase 1: Blame evaluation (only when dynamic assert domain is enabled) *)
        let b_eval, s_eval =
          if TestGenConfig.has_dynamic_assert_domain () then (
            let b_eval_a, s_eval_a =
              generate_eval_assignments filename sigma name ~free_vars ~ad asgns
            in
            let b_eval_c, s_eval_c =
              generate_eval_constraints filename sigma name ~free_vars its
            in
            (b_eval_a @ b_eval_c, s_eval_a @ s_eval_c))
          else
            ([], [])
        in
        (* Phase 2: Per-variable VAR_BEGIN / REFINE_CONSTRAINT / REFINE_ASSIGNMENT /
           VAR_END, wrapped in blamed check when dynamic assert domain is enabled *)
        let s_vars, b_extra =
          free_vars
          |> List.map (fun (x, x_bt) ->
            let cty = bt_to_domain_type_string x_bt in
            let cn_ty =
              match x_bt with
              | BT.Bits (Signed, sz) -> Printf.sprintf "cn_bits_i%d" sz
              | BT.Bits (Unsigned, sz) -> Printf.sprintf "cn_bits_u%d" sz
              | BT.Loc () -> "cn_pointer"
              | _ -> failwith ("unsupported type: " ^ Pp.plain (BaseTypes.pp x_bt))
            in
            let rel = AD.relative_to x x_bt ad in
            let domain_expr = Pp.plain (pp_relative x_bt rel) in
            (* VAR_BEGIN: declare per-variable domain on the stack *)
            let s_var_begin =
              A.AilSexpr
                (mk_expr
                   (AilEcall
                      ( mk_expr (string_ident "BENNET_ASSERT_DOMAIN_VAR_BEGIN"),
                        [ mk_expr (AilEident (Sym.fresh cty));
                          mk_expr (AilEident backtrack_var);
                          mk_expr (AilEident x);
                          mk_expr
                            (AilEident
                               (Sym.fresh ("(bennet_domain(" ^ cty ^ ")*)" ^ domain_expr)))
                        ] )))
            in
            (* REFINE_CONSTRAINT calls (when dynamic absint enabled and constraints
               non-empty) *)
            let s_refines =
              if TestGenConfig.has_dynamic_assert_domain () && not (List.is_empty its)
              then (
                let relevant_constraints =
                  List.filter (fun it -> Sym.Set.mem x (T.free_vars it)) its
                in
                generate_assert_domain_constraint_refinements
                  filename
                  sigma
                  ~var_sym:x
                  ~var_bt:x_bt
                  ~backtrack_var
                  relevant_constraints)
              else
                []
            in
            (* REFINE_ASSIGNMENT calls (only for Loc() variables, when dynamic absint
               enabled) *)
            let b_asgn_refines, s_asgn_refines =
              if TestGenConfig.has_dynamic_assert_domain () && BT.equal x_bt (BT.Loc ())
              then
                generate_assert_domain_assignment_refinements
                  filename
                  sigma
                  ~var_sym:x
                  ~backtrack_var
                  name
                  asgns
              else
                ([], [])
            in
            (* VAR_END: check the (possibly refined) domain *)
            let s_var_end =
              A.AilSexpr
                (mk_expr
                   (AilEcall
                      ( mk_expr (string_ident "BENNET_ASSERT_DOMAIN_VAR_END"),
                        [ mk_expr (AilEident (Sym.fresh cty));
                          mk_expr (AilEident (Sym.fresh cn_ty));
                          mk_expr (AilEident x);
                          mk_expr (AilEident backtrack_var)
                        ] )))
            in
            let refinement_stmts =
              [ s_var_begin ] @ s_asgn_refines @ s_refines @ [ s_var_end ]
            in
            (* Wrap refinement in blamed check when dynamic assert domain enabled *)
            let final_stmts =
              if TestGenConfig.has_dynamic_assert_domain () then
                [ A.AilSif
                    ( mk_expr
                        (AilEcall
                           ( mk_expr (string_ident "bennet_failure_is_blamed"),
                             [ mk_expr (AilEident x) ] )),
                      mk_stmt (A.AilSblock ([], List.map mk_stmt refinement_stmts)),
                      mk_stmt A.AilSskip )
                ]
              else
                refinement_stmts
            in
            (final_stmts, b_asgn_refines))
          |> List.split
          |> fun (ss, bs) -> (List.flatten ss, List.flatten bs)
        in
        (* Generate END macro call *)
        let s_end =
          A.
            [ AilSexpr
                (mk_expr
                   (AilEcall
                      ( mk_expr (string_ident "BENNET_ASSERT_DOMAIN_END"),
                        [ mk_expr (AilEident backtrack_var);
                          mk_expr (AilEident last_var)
                        ] )))
            ]
        in
        (* Wrap Phase 2 in failure type check when dynamic assert domain enabled *)
        let s_vars_guarded =
          if TestGenConfig.has_dynamic_assert_domain () then
            [ A.AilSif
                ( mk_expr
                    (AilEbinary
                       ( mk_expr
                           (AilEcall
                              ( mk_expr (string_ident "bennet_failure_get_failure_type"),
                                [] )),
                         Ne,
                         mk_expr (AilEident (Sym.fresh "BENNET_FAILURE_NONE")) )),
                  mk_stmt (A.AilSblock ([], List.map mk_stmt s_vars)),
                  mk_stmt A.AilSskip )
            ]
          else
            s_vars
        in
        let b_rest, s_rest, e_rest = transform_term filename sigma ctx name gt_rest in
        ( b_eval @ b_extra @ b_rest,
          s_begin @ s_eval @ s_vars_guarded @ s_end @ s_rest,
          e_rest ))
    | `ITE (it_if, gt_then, gt_else) ->
      let b_if, s_if, e_if = transform_it filename sigma name it_if in
      let b_then, s_then, e_then = transform_term filename sigma ctx name gt_then in
      let b_else, s_else, e_else = transform_term filename sigma ctx name gt_else in
      let res_sym = Sym.fresh_anon () in
      let res_expr = mk_expr (AilEident res_sym) in
      let res_binding = Utils.create_binding res_sym (bt_to_ctype_for_binding bt) in
      let res_stmt_ e = A.(AilSexpr (mk_expr (AilEassign (res_expr, e)))) in
      ( b_if @ [ res_binding ],
        (s_if
         @ A.
             [ AilSdeclaration [ (res_sym, None) ];
               AilSif
                 ( CtA.wrap_with_convert_from_cn_bool e_if,
                   mk_stmt
                     (AilSblock (b_then, List.map mk_stmt (s_then @ [ res_stmt_ e_then ]))),
                   mk_stmt
                     (AilSblock (b_else, List.map mk_stmt (s_else @ [ res_stmt_ e_else ])))
                 )
             ]),
        res_expr )
    | `MapElab ((i, i_bt, (it_min, it_max), it_perm), gt_inner) ->
      let sym_map = Sym.fresh_anon () in
      let b_map = Utils.create_binding sym_map (bt_to_ctype_for_binding bt) in
      let b_i = Utils.create_binding i (bt_to_ctype_for_binding i_bt) in
      let b_min, s_min, e_min = transform_it filename sigma name it_min in
      let b_max, s_max, e_max = transform_it filename sigma name it_max in
      let e_args =
        [ mk_expr (AilEident sym_map);
          mk_expr (AilEident i);
          mk_expr (string_ident (name_of_bt i_bt))
        ]
      in
      let e_perm =
        let b_perm, s_perm, e_perm = transform_it filename sigma name it_perm in
        A.(
          mk_expr
            (AilEgcc_statement (b_perm, List.map mk_stmt (s_perm @ [ AilSexpr e_perm ]))))
      in
      let s_begin =
        A.(
          s_min
          @ s_max
          @ [ AilSexpr
                (mk_expr
                   (AilEcall
                      ( mk_expr (string_ident "BENNET_MAP_BEGIN"),
                        e_args
                        @ [ e_perm; e_max; mk_expr (AilEident last_var) ]
                        @ List.map
                            (fun x -> mk_expr (AilEident x))
                            (List.of_seq
                               (Sym.Set.to_seq (Sym.Set.remove i (T.free_vars it_perm))))
                        @ [ mk_expr (AilEconst ConstantNull) ] )))
            ])
      in
      let b_val, s_val, e_val = transform_term filename sigma ctx name gt_inner in
      let s_end =
        A.(
          s_val
          @ [ AilSexpr
                (mk_expr
                   (AilEcall
                      (mk_expr (string_ident "BENNET_MAP_END"), e_args @ [ e_min; e_val ])))
            ])
      in
      ( [ b_map; b_i ] @ b_min @ b_max @ b_val,
        s_begin @ s_end,
        mk_expr (AilEident sym_map) )
    | `SplitSizeElab (_, _, gt_rest) when not (TestGenConfig.is_random_size_splits ()) ->
      transform_term filename sigma ctx name gt_rest
    | `SplitSizeElab (marker_var, syms, gt_rest) ->
      let e_tmp = mk_expr (AilEident marker_var) in
      let syms_l = syms |> Sym.Set.to_seq |> List.of_seq in
      let b =
        syms_l |> List.map (fun x -> Utils.create_binding x (C.mk_ctype_integer Size_t))
      in
      let e_syms =
        syms_l |> List.map (fun x -> mk_expr (AilEunary (Address, mk_expr (AilEident x))))
      in
      let s =
        let open A in
        List.map (fun x -> AilSdeclaration [ (x, None) ]) syms_l
        @ [ AilSexpr
              (mk_expr
                 (AilEcall
                    ( mk_expr (string_ident "BENNET_SPLIT_BEGIN"),
                      [ e_tmp ] @ e_syms @ [ mk_expr (AilEconst ConstantNull) ] )));
            AilSexpr
              (mk_expr
                 (AilEcall
                    ( mk_expr (string_ident "BENNET_SPLIT_END"),
                      [ e_tmp; mk_expr (AilEident last_var) ]
                      @ List.map
                          (fun x -> mk_expr (AilEident x))
                          (List.of_seq (Sym.Set.to_seq Sym.Set.empty))
                      @ [ mk_expr (AilEconst ConstantNull) ] )))
          ]
      in
      let b', s', e' = transform_term filename sigma ctx name gt_rest in
      (b @ b', s @ s', e')


  let transform_gen_def
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (ctx : Stage6.Ctx.t)
        ((name, gr) : Sym.t * Stage6.Def.t)
    : A.sigma_declaration * 'a A.sigma_function_definition
    =
    let loc = Locations.other __LOC__ in
    let bt_ret = gr.oarg in
    let ct_ret =
      match bt_ret with
      | BT.Record _ ->
        let struct_tag = CtA.lookup_records_map_with_default bt_ret in
        C.(mk_ctype_pointer no_qualifiers (Ctype ([], Struct struct_tag)))
      | BT.Unit -> C.(mk_ctype_pointer no_qualifiers (Ctype ([], Void)))
      | _ -> bt_to_ctype bt_ret
    in
    let decl : A.declaration =
      A.Decl_function
        ( false,
          (C.no_qualifiers, ct_ret),
          (List.map (fun (_, bt) -> (C.no_qualifiers, bt_to_ctype bt, false)) gr.iargs
           @
           if gr.recursive then
             [ (C.no_qualifiers, C.mk_ctype_integer Size_t, false) ]
           else
             []),
          false,
          false,
          false )
    in
    let sigma_decl : A.sigma_declaration = (name, (loc, CF.Annot.Attrs [], decl)) in
    let s_timing =
      if
        gr.spec
        && (TestGenConfig.will_print_timing_info ()
            || Option.is_some (TestGenConfig.get_output_tyche ()))
      then
        A.
          [ AilSexpr
              (mk_expr
                 (string_call
                    "bennet_info_timing_start"
                    [ mk_expr (AilEident (Sym.fresh "\"bennet\"")) ]))
          ]
      else
        []
    in
    let s1 =
      A.(
        AilSexpr
          (mk_expr
             (AilEcall
                ( mk_expr
                    (AilEident
                       (Sym.fresh
                          (if gr.recursive then "BENNET_INIT_SIZED" else "BENNET_INIT"))),
                  [] ))))
    in
    let b2, s2, e2 = transform_term gr.filename sigma ctx name gr.body in
    let sigma_def : CF.GenTypes.genTypeCategory A.sigma_function_definition =
      ( name,
        ( loc,
          0,
          CF.Annot.Attrs [],
          (List.map fst gr.iargs
           @
           if gr.recursive then
             [ Sym.fresh "bennet_rec_size" ]
           else
             []),
          mk_stmt
            (A.AilSblock
               ( b2,
                 List.map
                   mk_stmt
                   (s_timing
                    @ [ s1 ]
                    @ s2
                    @ A.
                        [ AilSexpr
                            (mk_expr
                               (AilEcall
                                  (mk_expr (string_ident "bennet_decrement_depth"), [])))
                        ]
                    @ A.[ AilSreturn (mk_expr (AilEcast (C.no_qualifiers, ct_ret, e2))) ]
                   ) )) ) )
    in
    (sigma_decl, sigma_def)


  let transform
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (prog5 : unit Mucore.file)
        (ctx : Stage6.Ctx.t)
    : Pp.document
    =
    let struct_defs =
      ctx
      |> List.filter_map (fun ((name, def) : Sym.t * Stage6.Def.t) ->
        (* Generate struct definitions for spec functions only *)
        if not def.spec then
          None
        else
          let open Pp in
          let struct_name = "cn_test_generator_" ^ Sym.pp_string name ^ "_record" in
          (* Get parameter C types from def *)
          let c_types = Option.get def.c_types in
          let param_name_strings =
            c_types |> List.map (fun (param_name, _) -> Sym.pp_string param_name)
          in
          let param_field_docs =
            c_types
            |> List.map (fun (param_name, ctype) ->
              let field_ty_doc =
                CF.Pp_ail.(
                  with_executable_spec (pp_ctype ~is_human:false C.no_qualifiers) ctype)
              in
              field_ty_doc ^^^ Sym.pp param_name ^^ semi)
          in
          (* Determine which oarg fields are globals (not in param_names or iargs) *)
          let global_field_docs =
            let inputs_outputs =
              match def.oarg with
              | BT.Record fields ->
                fields |> List.map (fun (id, bt) -> (Id.get_string id, bt))
              | _ -> []
            in
            let it_param_names =
              List.map (fun (sym, _bt) -> Sym.pp_string sym) def.iargs
            in
            let all_param_names = it_param_names @ param_name_strings in
            let global_fields =
              inputs_outputs
              |> List.filter (fun (field_name, _bt) ->
                not (List.exists (String.equal field_name) all_param_names))
            in
            global_fields
            |> List.filter_map (fun (global_name, _bt) ->
              (* Look up in globals using string comparison *)
              match
                prog5.globs
                |> List.find_opt (fun (global_sym, _) ->
                  String.equal (Sym.pp_string global_sym) global_name)
              with
              | Some (global_sym, Mucore.GlobalDecl sct)
              | Some (global_sym, Mucore.GlobalDef (sct, _)) ->
                (* Globals are stored as pointers in the struct *)
                let ctype = C.mk_ctype_pointer C.no_qualifiers (Sctypes.to_ctype sct) in
                let field_ty_doc =
                  CF.Pp_ail.(
                    with_executable_spec (pp_ctype ~is_human:false C.no_qualifiers) ctype)
                in
                Some (field_ty_doc ^^^ Sym.pp global_sym ^^ semi)
              | None ->
                failwith
                  (Printf.sprintf
                     "Could not find C type for global %s in function %s"
                     global_name
                     (Sym.pp_string name)))
          in
          let field_docs = param_field_docs @ global_field_docs in
          Some
            (!^"struct"
             ^^^ !^struct_name
             ^^^ braces (nest 2 (hardline ^^ separate hardline field_docs) ^^ hardline)
             ^^ semi
             ^^ hardline
             ^^ !^"typedef struct"
             ^^^ !^struct_name
             ^^^ !^struct_name
             ^^ semi))
    in
    let defs =
      List.map
        (fun ((_, gr) : _ * Stage6.Def.t) -> (GenUtils.get_mangled_name gr.name, gr))
        ctx
    in
    let declarations, function_definitions =
      defs |> List.map (transform_gen_def sigma ctx) |> List.split
    in
    let sigma : 'a A.sigma = { A.empty_sigma with declarations; function_definitions } in
    let record_defs = Records.generate_all_record_strs () in
    let include_guard_name =
      ctx
      |> List.hd
      |> (fun ((_, gr) : _ * Stage6.Def.t) -> gr.filename)
      |> Filename.basename
      |> Filename.remove_extension
      |> String.to_seq
      |> Seq.map (fun c -> match c with 'a' .. 'z' | 'A' .. 'Z' | '_' -> c | _ -> '_')
      |> String.of_seq
      |> String.uppercase_ascii
      |> Fun.flip ( ^ ) "_GEN_H"
      |> Pp.string
    in
    let harnesses =
      let open Pp in
      ctx
      |> List.filter (fun ((_, gr) : _ * Stage6.Def.t) -> gr.spec)
      |> List.map (fun ((name, def) : Sym.t * Stage6.Def.t) ->
        let struct_name = "cn_test_generator_" ^ Sym.pp_string name ^ "_record" in
        let bennet_name = "bennet_" ^ Sym.pp_string name in
        (* Extract fields from the Record type that bennet_* returns *)
        let inputs_outputs =
          match def.oarg with
          | BT.Record fields ->
            fields |> List.map (fun (id, bt) -> (Sym.fresh (Id.get_string id), bt))
          | _ -> []
        in
        (* Compute CN Record struct tag for accessing bennet_* return value *)
        let bt_record = def.oarg in
        let cn_struct_tag = CtA.lookup_records_map_with_default bt_record in
        (* Generate harness function that converts from CN types to C types *)
        !^"struct"
        ^^^ !^struct_name
        ^^ star
        ^^^ !^("cn_test_generator_" ^ Sym.pp_string name)
        ^^ parens (!^"void**" ^^^ !^"gen_state")
        ^^^ braces
              (nest
                 2
                 (hardline
                  ^^ !^"/* Call bennet function to get CN-typed result */"
                  ^^ hardline
                  ^^ !^"struct"
                  ^^^ Sym.pp cn_struct_tag
                  ^^ star
                  ^^^ !^"cn_result"
                  ^^^ equals
                  ^^^ !^bennet_name
                  ^^ parens empty
                  ^^ semi
                  ^^ twice hardline
                  ^^ !^"if"
                  ^^^ parens (!^"cn_result" ^^^ !^"==" ^^^ !^"NULL")
                  ^^^ !^"return"
                  ^^^ !^"NULL"
                  ^^ semi
                  ^^ twice hardline
                  ^^ !^"/* Allocate C-typed result struct */"
                  ^^ hardline
                  ^^ !^"struct"
                  ^^^ !^struct_name
                  ^^ star
                  ^^^ !^"result"
                  ^^^ equals
                  ^^^ !^"malloc"
                  ^^ parens (!^"sizeof" ^^ parens (!^"struct" ^^^ !^struct_name))
                  ^^ semi
                  ^^ hardline
                  ^^ !^"if"
                  ^^^ parens (!^"result" ^^^ !^"==" ^^^ !^"NULL")
                  ^^^ !^"return"
                  ^^^ !^"NULL"
                  ^^ semi
                  ^^ twice hardline
                  ^^ !^"/* Convert fields from CN types to C types */"
                  ^^ hardline
                  ^^ separate
                       hardline
                       (inputs_outputs
                        |> List.map (fun (field_name, bt) ->
                          let field_str = Sym.pp_string field_name in
                          let conversion_expr =
                            match CtA.get_conversion_from_fn_str bt with
                            | Some conv_fn ->
                              !^conv_fn ^^ parens (!^"cn_result->" ^^ !^field_str)
                            | None ->
                              (* No conversion needed for this type *)
                              !^"cn_result->" ^^ !^field_str
                          in
                          !^"result->"
                          ^^ !^field_str
                          ^^^ equals
                          ^^^ conversion_expr
                          ^^ semi))
                  ^^ twice hardline
                  ^^ !^"return"
                  ^^^ !^"result"
                  ^^ semi)
               ^^ hardline))
      |> separate (twice hardline)
    in
    let open Pp in
    (!^"#ifndef" ^^^ include_guard_name)
    ^^ hardline
    ^^ (!^"#define" ^^^ include_guard_name)
    ^^ twice hardline
    ^^ !^"#include"
    ^^^ angles !^"bennet/prelude.h"
    ^^ twice hardline
    ^^ !^"/* TAG DEFINITIONS */"
    ^^ hardline
    ^^ !^record_defs
    ^^ twice hardline
    ^^ !^"/* STRUCT DEFINITIONS */"
    ^^ hardline
    ^^ separate hardline struct_defs
    ^^ twice hardline
    ^^ !^"/* FUNCTION DECLARATIONS */"
    ^^ hardline
    ^^ CF.Pp_ail.(
         with_executable_spec
           (separate_map (twice hardline) (fun (tag, (_, _, decl)) ->
              CF.Pp_ail.pp_function_prototype tag decl))
           declarations)
    ^^ twice hardline
    ^^ !^"/* EVERYTHING ELSE */"
    ^^ hardline
    ^^ CF.Pp_ail.(with_executable_spec (pp_program ~show_include:true) (None, sigma))
    ^^ hardline
    ^^ harnesses
    ^^ hardline
    ^^ (!^"#endif //" ^^^ include_guard_name)
    ^^ hardline
end
