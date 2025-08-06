module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module CtA = Fulminate.Cn_to_ail
module Utils = Fulminate.Utils
module Records = Fulminate.Records
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

module Make (AD : GenTerms.Domain.T) = struct
  module Stage4 = Stage4.Make (AD)

  let mk_expr = Utils.mk_expr

  let mk_stmt = Utils.mk_stmt

  let bt_to_ctype (bt : BT.t) : C.ctype = CtA.bt_to_ail_ctype bt

  let name_of_bt (bt : BT.t) : string =
    let ct = bt_to_ctype bt in
    let ct' =
      match bt_to_ctype bt with Ctype (_, Pointer (_, ct')) -> ct' | _ -> failwith ""
    in
    let default =
      CF.Pp_utils.to_plain_string
        CF.Pp_ail.(with_executable_spec (pp_ctype C.no_qualifiers) ct')
    in
    Utils.get_typedef_string ct |> Option.value ~default


  let _str_name_of_bt (bt : BT.t) : string =
    name_of_bt bt |> String.split_on_char ' ' |> String.concat "_"


  let transform_it
        filename
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (name : Sym.t)
        (it : IT.t)
    =
    CtA.cn_to_ail_expr_toplevel filename sigma.cn_datatypes [] (Some name) None it


  let transform_lc filename (sigma : CF.GenTypes.genTypeCategory A.sigma) (lc : LC.t) =
    CtA.cn_to_ail_logical_constraint filename sigma.cn_datatypes [] None lc


  let string_ident (str : string) : CF.GenTypes.genTypeCategory A.expression_ =
    AilEident (Sym.fresh str)


  let string_call (str : string) (es : CF.GenTypes.genTypeCategory A.expression list)
    : CF.GenTypes.genTypeCategory A.expression_
    =
    A.AilEcall (mk_expr (string_ident str), es)


  let rec transform_term
            (filename : string)
            (sigma : CF.GenTypes.genTypeCategory A.sigma)
            (ctx : Stage4.Ctx.t)
            (name : Sym.t)
            (tm : Stage4.Term.t)
    : A.bindings
      * CF.GenTypes.genTypeCategory A.statement_ list
      * CF.GenTypes.genTypeCategory A.expression
    =
    let (Annot (tm_, (path_vars, last_var), bt, _)) = tm in
    match tm_ with
    | `Arbitrary ->
      let sign, bits = Option.get (BT.is_bits_bt bt) in
      let sign_str = match sign with Unsigned -> "UNSIGNED" | Signed -> "SIGNED" in
      ( [],
        [],
        mk_expr
          (string_call
             ("BENNET_ARBITRARY_" ^ sign_str)
             [ mk_expr
                 (AilEconst (ConstantInteger (IConstant (Z.of_int bits, Decimal, None))))
             ]) )
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
      (match Stage4.Ctx.find_opt fsym ctx with
       | Some _ -> ()
       | None -> failwith (Sym.pp_string fsym));
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
          if (Stage4.Ctx.find fsym ctx).recursive then
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
      (match Stage4.Ctx.find_opt fsym ctx with
       | Some _ -> ()
       | None -> failwith (Sym.pp_string fsym));
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
                                 (IT.cast_
                                    (BT.Loc ())
                                    (IT.sym_ (p_sym, p_bt, Locations.other __LOC__))
                                    (Locations.other __LOC__))
                             in
                             (b, List.map mk_stmt (s @ [ A.AilSexpr e ]))
                           in
                           A.AilEgcc_statement (b, s));
                        e_addr;
                        mk_expr
                          (string_ident
                             (CF.Pp_utils.to_plain_string
                                CF.Pp_ail.(
                                  with_executable_spec
                                    (pp_ctype C.no_qualifiers)
                                    (Sctypes.to_ctype sct))));
                        mk_expr
                          (CtA.wrap_with_convert_from ~sct e_value_ (IT.get_bt it_val));
                        mk_expr (AilEident last_var)
                      ]
                      @ List.map
                          (fun x -> mk_expr (AilEident x))
                          (List.of_seq (Sym.Set.to_seq (IT.free_vars it_addr)))
                      @ [ mk_expr (AilEconst ConstantNull) ] )))
          ]
      in
      let b_rest, s_rest, e_rest = transform_term filename sigma ctx name gt_rest in
      (b_addr @ b_value @ b_rest, s_addr @ s_value @ s_assign @ s_rest, e_rest)
    | `LetStar
        ((x, GenTerms.Annot (`Arbitrary, _, (Bits (sign, bits) as x_bt), _)), gt_rest) ->
      let func_name =
        match sign with
        | Unsigned -> "BENNET_LET_ARBITRARY_UNSIGNED"
        | Signed -> "BENNET_LET_ARBITRARY_SIGNED"
      in
      let b_let = [ Utils.create_binding x (bt_to_ctype x_bt) ] in
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
    | `LetStar ((x, GenTerms.Annot (`Arbitrary, _, (Loc () as x_bt), _)), gt_rest) ->
      let b_let = [ Utils.create_binding x (bt_to_ctype x_bt) ] in
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
    | `LetStar ((_, GenTerms.Annot (`Arbitrary, _, _, _)), _) ->
      failwith ("unreachable @ " ^ __LOC__)
    | `LetStar ((x, GenTerms.Annot (`Return it, _, x_bt, _)), gt_rest) ->
      let b_value, s_value, e_value = transform_it filename sigma name it in
      let s_let =
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
                     @ List.map
                         (fun x -> mk_expr (AilEident x))
                         (List.of_seq (Sym.Set.to_seq (IT.free_vars it)))
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
    | `ITE (it_if, gt_then, gt_else) ->
      let b_if, s_if, e_if = transform_it filename sigma name it_if in
      let b_then, s_then, e_then = transform_term filename sigma ctx name gt_then in
      let b_else, s_else, e_else = transform_term filename sigma ctx name gt_else in
      let res_sym = Sym.fresh_anon () in
      let res_expr = mk_expr (AilEident res_sym) in
      let res_binding = Utils.create_binding res_sym (bt_to_ctype bt) in
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
      let b_map = Utils.create_binding sym_map (bt_to_ctype bt) in
      let b_i = Utils.create_binding i (bt_to_ctype i_bt) in
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
                               (Sym.Set.to_seq (Sym.Set.remove i (IT.free_vars it_perm))))
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
        (ctx : Stage4.Ctx.t)
        ((name, gr) : Sym.t * Stage4.Def.t)
    : A.sigma_declaration * 'a A.sigma_function_definition
    =
    let loc = Locations.other __LOC__ in
    let bt_ret =
      BT.Record (List.map (fun (x, bt) -> (Id.make loc (Sym.pp_string x), bt)) gr.oargs)
    in
    let struct_tag = CtA.lookup_records_map_with_default bt_ret in
    let ct_ret = C.(mk_ctype_pointer no_qualifiers (Ctype ([], Struct struct_tag))) in
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
                   ([ s1 ]
                    @ s2
                    @ A.
                        [ AilSexpr
                            (mk_expr
                               (AilEcall
                                  (mk_expr (string_ident "bennet_decrement_depth"), [])))
                        ]
                    @ A.
                        [ AilSreturn
                            (mk_expr
                               (AilEcast
                                  ( C.no_qualifiers,
                                    C.(
                                      mk_ctype_pointer
                                        no_qualifiers
                                        (Ctype ([], Struct struct_tag))),
                                    e2 )))
                        ]) )) ) )
    in
    (sigma_decl, sigma_def)


  let transform (sigma : CF.GenTypes.genTypeCategory A.sigma) (ctx : Stage4.Ctx.t)
    : Pp.document
    =
    let defs =
      List.map
        (fun ((_, gr) : _ * Stage4.Def.t) -> (GenUtils.get_mangled_name gr.name, gr))
        ctx
    in
    let typedef_docs =
      defs
      |> List.map (fun ((name, def) : Sym.t * Stage4.Def.t) ->
        let loc = Locations.other __LOC__ in
        let bt =
          BT.Record
            (List.map (fun (x, bt) -> (Id.make loc (Sym.pp_string x), bt)) def.oargs)
        in
        let new_tag = Option.get (CtA.generate_record_tag name bt) in
        let typedef_doc tag =
          let open Pp in
          !^"typedef struct" ^^^ Sym.pp tag ^^^ Sym.pp new_tag ^^ semi
        in
        typedef_doc (CtA.lookup_records_map_with_default bt))
    in
    let declarations, function_definitions =
      List.split (List.map (transform_gen_def sigma ctx) defs)
    in
    let sigma : 'a A.sigma = { A.empty_sigma with declarations; function_definitions } in
    let record_defs = Records.generate_all_record_strs () in
    let include_guard_name =
      ctx
      |> List.hd
      |> (fun ((_, gr) : _ * Stage4.Def.t) -> gr.filename)
      |> Filename.basename
      |> Filename.remove_extension
      |> String.to_seq
      |> Seq.map (fun c -> match c with 'a' .. 'z' | 'A' .. 'Z' | '_' -> c | _ -> '_')
      |> String.of_seq
      |> String.capitalize_ascii
      |> fun x -> x ^ "_H" |> Pp.string
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
    ^^ !^"/* TYPEDEFS */"
    ^^ hardline
    ^^ separate hardline typedef_docs
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
    ^^ (!^"#endif //" ^^^ include_guard_name)
    ^^ hardline
end
