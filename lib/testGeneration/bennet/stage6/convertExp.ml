module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module CtA = Fulminate.Cn_to_ail
module Utils = Fulminate.Utils
module Records = Fulminate.Records
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

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
          (ctx : Stage5.Ctx.t)
          (name : Sym.t)
          (current_var : Sym.t)
          (tm : Stage5.Term.t)
  : A.bindings
    * CF.GenTypes.genTypeCategory A.statement_ list
    * CF.GenTypes.genTypeCategory A.expression
  =
  match tm with
  | Uniform { bt } ->
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
  | Alloc -> ([], [], mk_expr (string_call "BENNET_ARBITRARY_POINTER" []))
  | Pick { bt; choice_var; choices; last_var } ->
    let var = Sym.fresh_anon () in
    let bs, ss =
      List.split
        (List.mapi
           (fun i (_, gr) ->
              let bs, ss, e = transform_term filename sigma ctx name current_var gr in
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
           choices)
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
                                    (ConstantInteger
                                       (IConstant (Z.of_int w, Decimal, None)));
                                  AilEconst
                                    (ConstantInteger
                                       (IConstant (Z.of_int i, Decimal, None)))
                                ])
                           choices) )))
        ]
      @ List.flatten ss
      @ [ AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (string_ident "BENNET_PICK_END"),
                    [ mk_expr (AilEident choice_var) ] )))
        ],
      A.(mk_expr (AilEident var)) )
  | Call { fsym; iargs; oarg_bt; path_vars; last_var; sized } ->
    (match List.assoc_opt Sym.equal fsym ctx with
     | Some _ -> ()
     | None -> failwith (Sym.pp_string fsym));
    let sym = GenUtils.get_mangled_name fsym in
    let es = iargs |> List.map snd |> List.map (fun x -> A.(AilEident x)) in
    let sized_call =
      A.(
        match sized with
        | Some (n, _) when n <= 0 -> failwith "Invalid sized call"
        | Some (1, _) ->
          [ AilEbinary
              ( mk_expr (string_ident "bennet_rec_size"),
                Arithmetic Sub,
                mk_expr (AilEconst (ConstantInteger (IConstant (Z.one, Decimal, None))))
              )
          ]
        | Some (_, sym_size) when TestGenConfig.is_random_size_splits () ->
          [ AilEident sym_size ]
        | Some (n, _) ->
          [ AilEbinary
              ( mk_expr (string_ident "bennet_rec_size"),
                Arithmetic Div,
                mk_expr
                  (AilEconst (ConstantInteger (IConstant (Z.of_int n, Decimal, None)))) )
          ]
        | None when (List.assoc Sym.equal fsym ctx).recursive ->
          [ AilEcall (mk_expr (string_ident "bennet_get_size"), []) ]
        | None -> [])
    in
    let es = List.map mk_expr (es @ sized_call) in
    ( [],
      [],
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
                                  (List.map
                                     (fun x -> Sym.pp_string x)
                                     (List.of_seq (Sym.Set.to_seq path_vars))
                                   @ [ "NULL" ])
                              ^ " }")))));
               mk_stmt
                 (AilSexpr
                    (mk_expr
                       (AilEcall
                          ( mk_expr (string_ident "BENNET_CALL"),
                            [ mk_expr (string_ident (name_of_bt oarg_bt));
                              mk_expr (AilEident last_var);
                              mk_expr (AilEcall (mk_expr (AilEident sym), es))
                            ] ))))
             ] )) )
  | Asgn { backtrack_var = _; pointer = p_sym, p_bt; addr; sct; value; last_var; rest } ->
    let b_addr, s_addr, e_addr = transform_it filename sigma name addr in
    let b_value, s_value, AnnotatedExpression (_, _, _, e_value_) =
      transform_it filename sigma name value
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
                      mk_expr (CtA.wrap_with_convert_from ~sct e_value_ (IT.get_bt value));
                      mk_expr (AilEident last_var)
                    ]
                    @ List.map
                        (fun x -> mk_expr (AilEident x))
                        (List.of_seq (Sym.Set.to_seq (IT.free_vars addr)))
                    @ [ mk_expr (AilEconst ConstantNull) ] )))
        ]
    in
    let b_rest, s_rest, e_rest =
      transform_term filename sigma ctx name current_var rest
    in
    (b_addr @ b_value @ b_rest, s_addr @ s_value @ s_assign @ s_rest, e_rest)
  | LetStar { x; x_bt; value = Uniform { bt }; last_var; rest } ->
    let sign, bits = Option.get (BT.is_bits_bt bt) in
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
    let b_rest, s_rest, e_rest =
      transform_term filename sigma ctx name current_var rest
    in
    (b_let @ b_rest, s_let @ s_rest, e_rest)
  | LetStar { x; x_bt; value = Alloc; last_var; rest } ->
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
    let b_rest, s_rest, e_rest =
      transform_term filename sigma ctx name current_var rest
    in
    (b_let @ b_rest, s_let @ s_rest, e_rest)
  | LetStar { x; x_bt; value = Return { value }; last_var; rest } ->
    let b_value, s_value, e_value = transform_it filename sigma name value in
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
                       (List.of_seq (Sym.Set.to_seq (IT.free_vars value)))
                   @ [ mk_expr (AilEconst ConstantNull) ])))
        ]
    in
    let b_rest, s_rest, e_rest =
      transform_term filename sigma ctx name current_var rest
    in
    (b_value @ b_rest, s_value @ s_let @ s_rest, e_rest)
  | LetStar { x; x_bt; value = Call _ as value; last_var; rest }
  | LetStar { x; x_bt; value = Map _ as value; last_var; rest }
  | LetStar { x; x_bt; value = LetStar _ as value; last_var; rest } ->
    let b_value, s_value, e_value =
      transform_term filename sigma ctx name current_var value
    in
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
    let b_rest, s_rest, e_rest =
      transform_term filename sigma ctx name current_var rest
    in
    (b_value @ b_rest, s_value @ s_let @ s_rest, e_rest)
  | LetStar { value = Pick _; _ } ->
    failwith "Should be unreachable due to lifting of `pick`"
  | LetStar { value = ITE _; _ } ->
    failwith "Should be unreachable due to lifting of `if-else`"
  | LetStar { value = Assert _; _ } | LetStar { value = AssertDomain _; _ } ->
    failwith "Should be unreachable due to lifting of `assert`"
  | LetStar { value = Asgn _; _ } ->
    failwith "Should be unreachable due to lifting of `assign`"
  | LetStar { value = SplitSize _; _ } -> failwith "Should be unreachable due to lifting"
  | Return { value } ->
    let b, s, e = transform_it filename sigma name value in
    (b, s, e)
  | AssertDomain
      { sym; bt; domain = { lower_bound; upper_bound; multiple }; last_var; rest } ->
    let get_bound ty =
      let f : Stage5.bound_type -> string = function
        | Inclusive -> "I"
        | Exclusive -> "E"
      in
      ty
      |> Option.map_fst f
      |> Option.map_snd (transform_it filename sigma name)
      |> Option.value ~default:("I", ([], [], mk_expr (AilEconst ConstantNull)))
    in
    let ty_l, (b_lb, s_lb, e_lb) = get_bound lower_bound in
    let ty_u, (b_ub, s_ub, e_ub) = get_bound upper_bound in
    let b_m, s_m, e_m =
      multiple
      |> Option.map (transform_it filename sigma name)
      |> Option.value ~default:([], [], mk_expr (AilEconst ConstantNull))
    in
    let macro_str = "BENNET_ASSERT_DOMAIN_" ^ ty_l ^ ty_u in
    let s_assert =
      A.
        [ AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (string_ident macro_str),
                    [ mk_expr (string_ident (name_of_bt bt));
                      mk_expr (AilEident sym);
                      e_lb;
                      e_ub;
                      e_m;
                      mk_expr (AilEident last_var)
                    ]
                    @ List.map
                        (fun y -> mk_expr (AilEident y))
                        ([ Option.map snd lower_bound;
                           Option.map snd upper_bound;
                           multiple
                         ]
                         |> List.map (fun it ->
                           it
                           |> Option.map IT.free_vars
                           |> Option.value ~default:Sym.Set.empty)
                         |> List.fold_left Sym.Set.union Sym.Set.empty
                         |> Sym.Set.to_seq
                         |> List.of_seq
                         |> List.cons sym)
                    @ [ mk_expr (AilEconst ConstantNull) ] )))
        ]
    in
    let b_rest, s_rest, e_rest =
      transform_term filename sigma ctx name current_var rest
    in
    (b_lb @ b_ub @ b_m @ b_rest, s_lb @ s_ub @ s_m @ s_assert @ s_rest, e_rest)
  | Assert { prop; last_var; rest } ->
    let b1, s1, e1 = transform_lc filename sigma prop in
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
                        (List.of_seq (Sym.Set.to_seq (LC.free_vars prop)))
                    @ [ mk_expr (AilEconst ConstantNull) ] )))
        ]
    in
    let b2, s2, e2 = transform_term filename sigma ctx name current_var rest in
    (b1 @ b2, s1 @ s_assert @ s2, e2)
  | ITE { bt; cond; t; f } ->
    let b_if, s_if, e_if = transform_it filename sigma name cond in
    let b_then, s_then, e_then = transform_term filename sigma ctx name current_var t in
    let b_else, s_else, e_else = transform_term filename sigma ctx name current_var f in
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
  | Map { i; bt; min; max; perm; inner; last_var } ->
    let sym_map = Sym.fresh_anon () in
    let b_map = Utils.create_binding sym_map (bt_to_ctype bt) in
    let i_bt, _ = BT.map_bt bt in
    let b_i = Utils.create_binding i (bt_to_ctype i_bt) in
    let b_min, s_min, e_min = transform_it filename sigma name min in
    let b_max, s_max, e_max = transform_it filename sigma name max in
    let e_args =
      [ mk_expr (AilEident sym_map);
        mk_expr (AilEident i);
        mk_expr (string_ident (name_of_bt i_bt))
      ]
    in
    let e_perm =
      let b_perm, s_perm, e_perm = transform_it filename sigma name perm in
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
                             (Sym.Set.to_seq (Sym.Set.remove i (IT.free_vars perm))))
                      @ [ mk_expr (AilEconst ConstantNull) ] )))
          ])
    in
    let b_val, s_val, e_val = transform_term filename sigma ctx name current_var inner in
    let s_end =
      A.(
        s_val
        @ [ AilSexpr
              (mk_expr
                 (AilEcall
                    (mk_expr (string_ident "BENNET_MAP_END"), e_args @ [ e_min; e_val ])))
          ])
    in
    ([ b_map; b_i ] @ b_min @ b_max @ b_val, s_begin @ s_end, mk_expr (AilEident sym_map))
  | SplitSize { rest; _ } when not (TestGenConfig.is_random_size_splits ()) ->
    transform_term filename sigma ctx name current_var rest
  | SplitSize { marker_var; syms; path_vars; last_var; rest } ->
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
                        (List.of_seq (Sym.Set.to_seq path_vars))
                    @ [ mk_expr (AilEconst ConstantNull) ] )))
        ]
    in
    let b', s', e' = transform_term filename sigma ctx name current_var rest in
    (b @ b', s @ s', e')


let transform_gen_def
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (ctx : Stage5.Ctx.t)
      ((name, gr) : Sym.t * Stage5.Def.t)
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
  let b2, s2, e2 =
    transform_term gr.filename sigma ctx name (Sym.fresh "bennet") gr.body
  in
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


let transform (sigma : CF.GenTypes.genTypeCategory A.sigma) (ctx : Stage5.Ctx.t)
  : Pp.document
  =
  let defs =
    List.map
      (fun ((_, gr) : _ * Stage5.Def.t) -> (GenUtils.get_mangled_name gr.name, gr))
      ctx
  in
  let typedef_docs =
    defs
    |> List.map (fun ((name, def) : Sym.t * Stage5.Def.t) ->
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
    |> (fun ((_, gr) : _ * Stage5.Def.t) -> gr.filename)
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
  ^^^ angles !^"bennet-exp/prelude.h"
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
