module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module BT = BaseTypes
module AT = ArgumentTypes
module LAT = LogicalArgumentTypes
module IT = IndexTerms
module CtA = Fulminate.Cn_to_ail
module Utils = Fulminate.Utils

let mk_expr = Utils.mk_expr

let mk_stmt = Utils.mk_stmt

let rec string_of_ctype ctype =
  match ctype with
  | C.Ctype (_, Pointer (_, Ctype (_, Function ((_, ret), args, _)))) ->
    string_of_ctype ret
    ^ "_from_"
    ^ String.concat "_and_" (List.map (fun (_, ct, _) -> string_of_ctype ct) args)
  | _ -> String.concat "_" (String.split_on_char ' ' (Utils.str_of_ctype ctype))


let bt_to_ctype (bt : BT.t) : C.ctype = CtA.bt_to_ail_ctype bt

let name_of_bt (bt : BT.t) : string =
  let ct = bt_to_ctype bt in
  let ct' =
    match bt_to_ctype bt with Ctype (_, Pointer (_, ct')) -> ct' | _ -> failwith __LOC__
  in
  let default = CF.Pp_utils.to_plain_string (CF.Pp_ail.pp_ctype C.no_qualifiers ct') in
  Utils.get_typedef_string ct |> Option.value ~default


let owned_sct_sym (sct : Sctypes.t) : Sym.t =
  Sym.fresh ("cn_analyze_shape_owned_" ^ string_of_ctype (Sctypes.to_ctype sct))


let pred_sym (psym : Sym.t) : Sym.t = Sym.fresh ("cn_analyze_shape_" ^ Sym.pp_string psym)

let compile_sct (sct : Sctypes.t)
  : A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition
  =
  let fsym = owned_sct_sym sct in
  let ptr_sym = Sym.fresh "ptr" in
  let parent_sym = Sym.fresh "parent_ptr" in
  let sz_sym = Sym.fresh "sz" in
  let bt = Memory.bt_of_sct sct in
  let s =
    mk_stmt
      A.(
        AilSblock
          ( [],
            [ mk_stmt
                (AilSexpr
                   (mk_expr
                      (AilEcall
                         ( mk_expr (AilEident (Sym.fresh "cn_analyze_shape_owned")),
                           List.map
                             mk_expr
                             [ CtA.wrap_with_convert_from
                                 (AilEident parent_sym)
                                 (BT.Loc ());
                               CtA.wrap_with_convert_from
                                 (AilEident sz_sym)
                                 Memory.size_bt
                             ] ))));
              mk_stmt
                (AilSreturn
                   (mk_expr
                      (CtA.wrap_with_convert_to
                         ~sct
                         (AilEunary
                            ( Indirection,
                              mk_expr
                                (AilEcast
                                   ( C.no_qualifiers,
                                     Sctypes.to_ctype (Sctypes.pointer_ct sct),
                                     mk_expr
                                       (CtA.wrap_with_convert_from
                                          (AilEident ptr_sym)
                                          (BT.Loc ())) )) ))
                         bt)))
            ] ))
  in
  let cn_pointer_sct = (C.no_qualifiers, CtA.bt_to_ail_ctype (BT.Loc ()), false) in
  let cn_size_t_sct = (C.no_qualifiers, CtA.bt_to_ail_ctype Memory.size_bt, false) in
  let decl =
    ( fsym,
      ( Locations.other __LOC__,
        CF.Annot.Attrs [],
        A.Decl_function
          ( false,
            (C.no_qualifiers, CtA.bt_to_ail_ctype bt),
            [ cn_pointer_sct; cn_pointer_sct; cn_size_t_sct ],
            false,
            false,
            false ) ) )
  in
  let def =
    ( fsym,
      (Locations.other __LOC__, 0, CF.Annot.Attrs [], [ ptr_sym; parent_sym; sz_sym ], s)
    )
  in
  (decl, def)


let rec extract_global_variables = function
  | [] -> []
  | (sym, globs) :: ds ->
    (match globs with
     | Mucore.GlobalDef (ctype, _) ->
       (sym, Sctypes.to_ctype ctype) :: extract_global_variables ds
     | GlobalDecl ctype -> (sym, Sctypes.to_ctype ctype) :: extract_global_variables ds)


let compile_it
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (it : IT.t)
  =
  CtA.cn_to_ail_expr_toplevel
    filename
    sigma.cn_datatypes
    (extract_global_variables prog5.globs)
    None
    None
    it


let get_parent_and_size (sct : Sctypes.t) (arg : IT.t) loc =
  let rec aux (it : IT.t) =
    match it with
    | IT (ArrayShift { base; ct; index }, _, loc) ->
      let base, offset = aux base in
      ( base,
        IT.add_
          (offset, IT.mul_ (IT.cast_ Memory.size_bt index loc, IT.sizeOf_ ct loc) loc)
          loc )
    | IT (MemberShift (base, tag, member), _, loc) ->
      aux
        (IT.pointer_offset_
           (base, IT.(IT (OffsetOf (tag, member), Memory.size_bt, loc)))
           loc)
    | IT (_, _, loc) -> (it, IT.num_lit_ Z.zero Memory.uintptr_bt loc)
  in
  let base, offset = aux arg in
  (base, IT.add_ (offset, IT.sizeOf_ sct loc) loc)


let owned_sct_call
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (sct : Sctypes.t)
      (pointer : IT.t)
  : A.bindings
    * CF.GenTypes.genTypeCategory A.statement_ list
    * CF.GenTypes.genTypeCategory A.expression
  =
  let parent, size = get_parent_and_size sct pointer (Locations.other __LOC__) in
  let b1, s1, e1 = compile_it filename sigma prog5 pointer in
  let b2, s2, e2 = compile_it filename sigma prog5 parent in
  let b3, s3, e3 = compile_it filename sigma prog5 size in
  let fsym = owned_sct_sym sct in
  let e4 = mk_expr A.(AilEcall (mk_expr (AilEident fsym), [ e1; e2; e3 ])) in
  (b1 @ b2 @ b3, s1 @ s2 @ s3, e4)


let compile_req
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (req : Request.t)
      (loc : Locations.t)
  : A.bindings
    * CF.GenTypes.genTypeCategory A.statement_ list
    * CF.GenTypes.genTypeCategory A.expression
  =
  let rec aux (req : Request.t) =
    match req with
    | P { name = Owned (sct, _); pointer; iargs } ->
      assert (List.is_empty iargs);
      owned_sct_call filename sigma prog5 sct pointer
    | P { name = PName name; pointer; iargs } ->
      let b, s, es =
        pointer :: iargs
        |> List.map (compile_it filename sigma prog5)
        |> List.fold_left
             (fun (b, s, es) (b', s', e) -> (b @ b', s @ s', es @ [ e ]))
             ([], [], [])
      in
      let e = A.(mk_expr (AilEcall (mk_expr (AilEident (pred_sym name)), es))) in
      (b, s, e)
    | Q { name; pointer; q = q_sym, q_bt; q_loc; step; permission; iargs } ->
      assert (List.is_empty iargs);
      let q_it = IT.sym_ (q_sym, q_bt, q_loc) in
      let e_perm =
        let b_perm, s_perm, e_perm = compile_it filename sigma prog5 permission in
        A.(
          mk_expr
            (AilEgcc_statement (b_perm, List.map mk_stmt (s_perm @ [ AilSexpr e_perm ]))))
      in
      let b1, s1, e_min, e_max =
        let it_min, it_max = IT.Bounds.get_bounds (q_sym, q_bt) permission in
        let b1, s1, e_min = compile_it filename sigma prog5 it_min in
        let b2, s2, e_max = compile_it filename sigma prog5 it_max in
        (b1 @ b2, s1 @ s2, e_min, e_max)
      in
      let map_sym = Sym.fresh_anon () in
      let b_val, s_val, e_val =
        aux
          (P { name; pointer = IT.arrayShift_ ~base:pointer ~index:q_it step loc; iargs })
      in
      let s2 =
        A.
          [ AilSexpr
              (mk_expr
                 (AilEcall
                    ( mk_expr (AilEident (Sym.fresh "CN_ANALYZE_SHAPE_EACH_BEGIN")),
                      List.map
                        mk_expr
                        [ AilEident map_sym;
                          AilEident q_sym;
                          AilEident (Sym.fresh (name_of_bt q_bt))
                        ]
                      @ [ e_perm; e_min ] )))
          ]
        @ s_val
        @ [ AilSexpr
              (mk_expr
                 (AilEcall
                    ( mk_expr (AilEident (Sym.fresh "CN_ANALYZE_SHAPE_EACH_END")),
                      List.map
                        mk_expr
                        [ AilEident map_sym;
                          AilEident q_sym;
                          AilEident (Sym.fresh (name_of_bt q_bt))
                        ]
                      @ [ e_val; e_max ] )))
          ]
      in
      (b1 @ b_val, s1 @ s2, mk_expr (A.AilEident map_sym))
  in
  aux req


let compile_lat
      ?(f : 'a -> A.bindings * CF.GenTypes.genTypeCategory A.statement_ list =
        fun _ -> ([], []))
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (lat : 'a LAT.t)
  : A.bindings * CF.GenTypes.genTypeCategory A.statement_ list
  =
  let rec aux (lat : 'a LAT.t) =
    match lat with
    | Define ((x, it), _, lat') ->
      let b1, s1, e = compile_it filename sigma prog5 it in
      let b2 = [ Utils.create_binding x (CtA.bt_to_ail_ctype (IT.get_bt it)) ] in
      let s2 = A.[ AilSdeclaration [ (x, Some e) ] ] in
      let b3, s3 = aux lat' in
      (b1 @ b2 @ b3, s1 @ s2 @ s3)
    | Resource ((x, (req, bt)), (loc, _), lat') ->
      let b1, s1, e = compile_req filename sigma prog5 req loc in
      let b2 = [ Utils.create_binding x (CtA.bt_to_ail_ctype bt) ] in
      let s2 =
        if BT.equal bt BT.Unit then
          A.[ AilSexpr e ]
        else
          A.[ AilSdeclaration [ (x, Some e) ] ]
      in
      let b3, s3 = aux lat' in
      (b1 @ b2 @ b3, s1 @ s2 @ s3)
    | Constraint (_, _, lat') -> aux lat'
    | I i -> f i
  in
  aux lat


let compile_clauses
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (cls : Definition.Clause.t list)
  : A.bindings * CF.GenTypes.genTypeCategory A.statement_ list
  =
  let rec aux (cls : Definition.Clause.t list)
    : A.bindings * CF.GenTypes.genTypeCategory A.statement_ list
    =
    let aux_it it =
      if BT.equal (IT.get_bt it) BT.Unit then
        ([], [ A.AilSreturnVoid ])
      else (
        let b, s, e = compile_it filename sigma prog5 it in
        (b, s @ [ AilSreturn e ]))
    in
    match cls with
    | [ cl ] ->
      assert (IT.is_true cl.guard);
      compile_lat ~f:aux_it filename sigma prog5 cl.packing_ft
    | cl :: cls' ->
      let b_if, s_if, e_if = compile_it filename sigma prog5 cl.guard in
      let b_then, s_then = compile_lat ~f:aux_it filename sigma prog5 cl.packing_ft in
      let b_else, s_else = aux cls' in
      let s_then_else =
        A.
          [ AilSif
              ( CtA.wrap_with_convert_from_cn_bool e_if,
                mk_stmt (AilSblock (b_then, List.map mk_stmt s_then)),
                mk_stmt (AilSblock (b_else, List.map mk_stmt s_else)) )
          ]
      in
      (b_if, s_if @ s_then_else)
    | [] -> failwith ("unreachable @ " ^ __LOC__)
  in
  aux cls


let compile_pred
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (sym : Sym.t)
      (pred : Definition.Predicate.t)
  : A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition
  =
  let fsym = pred_sym sym in
  let ret_type = CtA.bt_to_ail_ctype ~pred_sym:(Some sym) (snd pred.oarg) in
  let bs, ss =
    match pred.clauses with
    | Some clauses -> compile_clauses filename sigma prog5 clauses
    | None -> ([], [])
  in
  let params =
    List.map
      (fun (sym, bt) -> (sym, CtA.bt_to_ail_ctype bt))
      ((pred.pointer, BT.(Loc ())) :: pred.iargs)
  in
  let param_syms, param_types = List.split params in
  let param_types = List.map (fun t -> (C.no_qualifiers, t, false)) param_types in
  let decl =
    ( fsym,
      ( pred.loc,
        CF.Annot.Attrs [],
        A.(
          Decl_function
            (false, (C.no_qualifiers, ret_type), param_types, false, false, false)) ) )
  in
  let def =
    ( fsym,
      ( pred.loc,
        0,
        CF.Annot.Attrs [],
        param_syms,
        mk_stmt A.(AilSblock (bs, List.map mk_stmt ss)) ) )
  in
  (decl, def)


let compile_spec
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (is_static : bool)
      (sym : Sym.t)
      (at : 'a AT.t)
  : A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition
  =
  let fsym =
    pred_sym
      (if is_static then
         Sym.fresh (Fulminate.Utils.static_prefix filename ^ "_" ^ Sym.pp_string sym)
       else
         sym)
  in
  let args =
    match List.assoc Sym.equal sym sigma.declarations with
    | _, _, Decl_function (_, _, args, _, _, _) ->
      let arg_names = AT.get_computational at in
      let arg_cts = List.map (fun (_, ct, _) -> ct) args in
      List.map (fun ((x, bt), ct) -> (x, (bt, ct))) (List.combine arg_names arg_cts)
    | _ -> failwith ("unreachable @ " ^ __LOC__)
  in
  let new_args = List.map (fun (x, _) -> (x, Sym.fresh (Sym.pp_string x ^ "_cn"))) args in
  let bs =
    List.map
      (fun (x, y) ->
         Utils.create_binding y (CtA.bt_to_ail_ctype (fst (List.assoc Sym.equal x args))))
      new_args
  in
  let ss =
    List.map
      (fun (x, y) ->
         A.AilSdeclaration
           [ ( y,
               Some
                 (mk_expr
                    (CtA.wrap_with_convert_to
                       (A.AilEident x)
                       (fst (List.assoc Sym.equal x args)))) )
           ])
      new_args
  in
  let lat =
    LAT.subst
      (fun _ x -> x)
      (IT.make_subst
         (List.map
            (fun (x, y) ->
               (x, IT.sym_ (y, fst (List.assoc Sym.equal x args), Locations.other __LOC__)))
            new_args))
      (AT.get_lat at)
  in
  (* Generate function *)
  let bs', ss' = compile_lat filename sigma prog5 lat in
  let decl : A.sigma_declaration =
    ( fsym,
      ( Locations.other __LOC__,
        Attrs [],
        Decl_function
          ( false,
            (C.no_qualifiers, C.void),
            List.map (fun (_, (_, ct)) -> (C.no_qualifiers, ct, false)) args,
            false,
            false,
            false ) ) )
  in
  let def : CF.GenTypes.genTypeCategory A.sigma_function_definition =
    ( fsym,
      ( Locations.other __LOC__,
        0,
        Attrs [],
        List.map fst args,
        A.(mk_stmt (AilSblock (bs @ bs', List.map mk_stmt (ss @ ss')))) ) )
  in
  (decl, def)


let synthesize
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (insts : (bool * Fulminate.Extract.instrumentation) list)
  : (A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition) list
  =
  (* Per type *)
  let type_analyzers =
    let module CtypeSet =
      Set.Make (struct
        type t = C.ctype

        let compare a b = compare (Hashtbl.hash a) (Hashtbl.hash b)
      end)
    in
    !CtA.ownership_ctypes
    |> CtypeSet.of_list
    |> CtypeSet.to_seq
    |> List.of_seq
    |> List.map (Sctypes.of_ctype_unsafe (Locations.other __LOC__))
    |> List.map compile_sct
  in
  (* Per predicate *)
  let pred_analyzers =
    prog5.resource_predicates
    |> List.map (fun (sym, pred) -> compile_pred filename sigma prog5 sym pred)
  in
  (* Per specification *)
  let spec_analyzers =
    insts
    |> List.filter_map
         (fun ((is_static, inst) : bool * Fulminate.Extract.instrumentation) ->
            Option.map
              (fun lat -> compile_spec filename sigma prog5 is_static inst.fn lat)
              inst.internal)
  in
  type_analyzers @ pred_analyzers @ spec_analyzers
