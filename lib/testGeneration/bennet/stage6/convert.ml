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


let rec transform_term
          (filename : string)
          (sigma : CF.GenTypes.genTypeCategory A.sigma)
          (ctx : Stage5.Ctx.t)
          (name : Sym.t)
          (tm : Stage5.Term.t)
  : A.bindings
    * CF.GenTypes.genTypeCategory A.statement_ list
    * CF.GenTypes.genTypeCategory A.expression
  =
  let loc = Locations.other __LOC__ in
  match tm with
  | Uniform { bt } ->
    ( [],
      [],
      A.(
        mk_expr
          (AilEcall
             ( mk_expr (AilEident (Sym.fresh "BENNET_UNIFORM")),
               List.map mk_expr [ AilEident (Sym.fresh (name_of_bt bt)) ] ))) )
  | Pick { bt; choice_var; choices; last_var } ->
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
                            ( mk_expr (AilEident (Sym.fresh "BENNET_PICK_CASE_BEGIN")),
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
                              ( mk_expr (AilEident (Sym.fresh "BENNET_PICK_CASE_END")),
                                [ mk_expr (AilEident var); e ] )))
                    ]) ))
           choices)
    in
    ( List.flatten bs,
      A.
        [ AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (AilEident (Sym.fresh "BENNET_PICK_BEGIN")),
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
                  ( mk_expr (AilEident (Sym.fresh "BENNET_PICK_END")),
                    [ mk_expr (AilEident choice_var) ] )))
        ],
      A.(mk_expr (AilEident var)) )
  | Alloc ->
    let alloc_sym = Sym.fresh "BENNET_ALLOC" in
    let b, s, e =
      transform_it filename sigma name (IT.num_lit_ Z.zero Memory.size_bt loc)
    in
    (b, s, mk_expr (AilEcall (mk_expr (AilEident alloc_sym), [ e ])))
  | Call { fsym; iargs; oarg_bt; path_vars; sized } ->
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
              ( mk_expr (AilEident (Sym.fresh "bennet_rec_size")),
                Arithmetic Sub,
                mk_expr (AilEconst (ConstantInteger (IConstant (Z.one, Decimal, None))))
              )
          ]
        | Some (_, sym_size) when TestGenConfig.is_random_size_splits () ->
          [ AilEident sym_size ]
        | Some (n, _) ->
          [ AilEbinary
              ( mk_expr (AilEident (Sym.fresh "bennet_rec_size")),
                Arithmetic Div,
                mk_expr
                  (AilEconst (ConstantInteger (IConstant (Z.of_int n, Decimal, None)))) )
          ]
        | None when (List.assoc Sym.equal fsym ctx).recursive ->
          [ AilEcall (mk_expr (AilEident (Sym.fresh "bennet_get_size")), []) ]
        | None -> [])
    in
    let es = List.map mk_expr (es @ sized_call) in
    let x = Sym.fresh_anon () in
    let b = Utils.create_binding x (bt_to_ctype oarg_bt) in
    let wrap_to_string (sym : Sym.t) =
      A.(
        AilEcast
          ( C.no_qualifiers,
            C.pointer_to_char,
            mk_expr (AilEstr (None, [ (Locations.other __LOC__, [ Sym.pp_string sym ]) ]))
          ))
    in
    let from_vars = iargs |> List.map fst |> List.map wrap_to_string in
    let to_vars = iargs |> List.map snd |> List.map wrap_to_string in
    let macro_call name vars =
      A.AilSexpr
        (mk_expr (AilEcall (mk_expr (AilEident (Sym.fresh name)), List.map mk_expr vars)))
    in
    ( [ b ],
      ([ A.AilSdeclaration
           [ (x, Some (mk_expr (AilEcall (mk_expr (AilEident sym), es)))) ]
       ]
       @ (if List.is_empty from_vars then
            []
          else
            [ macro_call "BENNET_CALL_FROM" from_vars;
              macro_call "BENNET_CALL_TO" to_vars
            ])
       @
       if Sym.Set.is_empty path_vars then
         []
       else
         [ macro_call
             "BENNET_CALL_PATH_VARS"
             (path_vars |> Sym.Set.to_seq |> List.of_seq |> List.map wrap_to_string)
         ]),
      mk_expr (AilEident x) )
  | Asgn { pointer = p_sym, p_bt; addr; sct; value; last_var; rest } ->
    let tmp_sym = Sym.fresh_anon () in
    let b1, s1, e1 = transform_it filename sigma name addr in
    let b2, s2, AnnotatedExpression (_, _, _, e2_) =
      transform_it filename sigma name value
    in
    let b3 = [ Utils.create_binding tmp_sym C.(mk_ctype_pointer no_qualifiers void) ] in
    let s3 =
      A.
        [ AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (AilEident (Sym.fresh "BENNET_ASSIGN")),
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
                      e1;
                      mk_expr
                        (AilEident
                           (Sym.fresh
                              (CF.Pp_utils.to_plain_string
                                 CF.Pp_ail.(
                                   with_executable_spec
                                     (pp_ctype C.no_qualifiers)
                                     (Sctypes.to_ctype sct)))));
                      mk_expr (CtA.wrap_with_convert_from e2_ (IT.get_bt value));
                      mk_expr (AilEident (Sym.fresh_anon ()));
                      mk_expr
                        (AilEcast
                           ( C.no_qualifiers,
                             C.pointer_to_char,
                             mk_expr (AilEstr (None, [ (loc, [ Sym.pp_string name ]) ]))
                           ));
                      mk_expr (AilEident last_var)
                    ]
                    @ List.map
                        (fun x ->
                           mk_expr
                             (AilEcast
                                ( C.no_qualifiers,
                                  C.pointer_to_char,
                                  mk_expr
                                    (AilEstr
                                       ( None,
                                         [ (Locations.other __LOC__, [ Sym.pp_string x ])
                                         ] )) )))
                        (List.of_seq (Sym.Set.to_seq (IT.free_vars addr)))
                    @ [ mk_expr (AilEconst ConstantNull) ] )))
        ]
    in
    let b4, s4, e4 = transform_term filename sigma ctx name rest in
    (b1 @ b2 @ b3 @ b4, s1 @ s2 @ s3 @ s4, e4)
  | LetStar { x; x_bt; value; last_var; rest } ->
    let s1 =
      A.
        [ AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (AilEident (Sym.fresh "BENNET_LET_BEGIN")),
                    List.map
                      mk_expr
                      [ AilEconst
                          (ConstantInteger
                             (IConstant
                                ( (if Stage5.Term.is_return value then
                                     Z.zero
                                   else
                                     Z.of_int (TestGenConfig.get_max_backtracks ())),
                                  Decimal,
                                  None )));
                        AilEident x
                      ] )))
        ]
    in
    let b2, s2, e2 = transform_term filename sigma ctx name value in
    let s3 =
      A.(
        [ AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (AilEident (Sym.fresh "BENNET_LET_BODY")),
                    List.map
                      mk_expr
                      [ AilEident (Sym.fresh (name_of_bt x_bt)); AilEident x ]
                    @ [ e2 ] )))
        ]
        @ [ AilSexpr
              (mk_expr
                 (AilEcall
                    ( mk_expr (AilEident (Sym.fresh "BENNET_LET_END")),
                      List.map mk_expr [ AilEident x; AilEident last_var ]
                      @ List.map
                          (fun x ->
                             mk_expr
                               (AilEcast
                                  ( C.no_qualifiers,
                                    C.pointer_to_char,
                                    mk_expr
                                      (AilEstr
                                         ( None,
                                           [ (Locations.other __LOC__, [ Sym.pp_string x ])
                                           ] )) )))
                          (List.of_seq (Sym.Set.to_seq (Stage5.Term.free_vars value)))
                      @ [ mk_expr (AilEconst ConstantNull) ] )))
          ])
    in
    let b4, s4, e4 = transform_term filename sigma ctx name rest in
    (b2 @ [ Utils.create_binding x (bt_to_ctype x_bt) ] @ b4, s1 @ s2 @ s3 @ s4, e4)
  | Return { value } ->
    let b, s, e = transform_it filename sigma name value in
    (b, s, e)
  | Assert { prop; last_var; rest } ->
    let b1, s1, e1 = transform_lc filename sigma prop in
    let s_assert =
      A.
        [ AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (AilEident (Sym.fresh "BENNET_ASSERT")),
                    [ e1 ]
                    @ [ mk_expr (AilEident last_var) ]
                    @ List.map
                        (fun x ->
                           mk_expr
                             (AilEcast
                                ( C.no_qualifiers,
                                  C.pointer_to_char,
                                  mk_expr
                                    (AilEstr
                                       ( None,
                                         [ (Locations.other __LOC__, [ Sym.pp_string x ])
                                         ] )) )))
                        (List.of_seq (Sym.Set.to_seq (LC.free_vars prop)))
                    @ [ mk_expr (AilEconst ConstantNull) ] )))
        ]
    in
    let b2, s2, e2 = transform_term filename sigma ctx name rest in
    (b1 @ b2, s1 @ s_assert @ s2, e2)
  | ITE { bt; cond; t; f } ->
    let b_if, s_if, e_if = transform_it filename sigma name cond in
    let b_then, s_then, e_then = transform_term filename sigma ctx name t in
    let b_else, s_else, e_else = transform_term filename sigma ctx name f in
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
        mk_expr (AilEident (Sym.fresh (name_of_bt i_bt)))
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
                    ( mk_expr (AilEident (Sym.fresh "BENNET_MAP_BEGIN")),
                      e_args
                      @ [ e_perm; e_max; mk_expr (AilEident last_var) ]
                      @ List.map
                          (fun x ->
                             mk_expr
                               (AilEcast
                                  ( C.no_qualifiers,
                                    C.pointer_to_char,
                                    mk_expr
                                      (AilEstr
                                         ( None,
                                           [ (Locations.other __LOC__, [ Sym.pp_string x ])
                                           ] )) )))
                          (List.of_seq
                             (Sym.Set.to_seq (Sym.Set.remove i (IT.free_vars perm))))
                      @ [ mk_expr (AilEconst ConstantNull) ] )))
          ])
    in
    let b_val, s_val, e_val = transform_term filename sigma ctx name inner in
    let s_end =
      A.(
        s_val
        @ [ AilSexpr
              (mk_expr
                 (AilEcall
                    ( mk_expr (AilEident (Sym.fresh "BENNET_MAP_END")),
                      e_args @ [ e_min; e_val ] )))
          ])
    in
    ([ b_map; b_i ] @ b_min @ b_max @ b_val, s_begin @ s_end, mk_expr (AilEident sym_map))
  | SplitSize { rest; _ } when not (TestGenConfig.is_random_size_splits ()) ->
    transform_term filename sigma ctx name rest
  | SplitSize { marker_var; syms; path_vars; last_var; rest } ->
    let e_tmp = mk_expr (AilEident marker_var) in
    let syms_l = syms |> Sym.Set.to_seq |> List.of_seq in
    let b =
      syms_l |> List.map (fun x -> Utils.create_binding x (C.mk_ctype_integer Size_t))
    in
    let e_syms =
      syms_l |> List.map (fun x -> mk_expr (AilEunary (Address, mk_expr (AilEident x))))
    in
    let wrap_to_string (sym : Sym.t) =
      let open A in
      mk_expr
        (AilEcast
           ( C.no_qualifiers,
             C.pointer_to_char,
             mk_expr
               (AilEstr (None, [ (Locations.other __LOC__, [ Sym.pp_string sym ]) ])) ))
    in
    let s =
      let open A in
      List.map (fun x -> AilSdeclaration [ (x, None) ]) syms_l
      @ [ AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (AilEident (Sym.fresh "BENNET_SPLIT_BEGIN")),
                    [ e_tmp ] @ e_syms @ [ mk_expr (AilEconst ConstantNull) ] )));
          AilSexpr
            (mk_expr
               (AilEcall
                  ( mk_expr (AilEident (Sym.fresh "BENNET_SPLIT_END")),
                    [ e_tmp; mk_expr (AilEident last_var) ]
                    @ List.map wrap_to_string (List.of_seq (Sym.Set.to_seq path_vars))
                    @ [ mk_expr (AilEconst ConstantNull) ] )))
        ]
    in
    let b', s', e' = transform_term filename sigma ctx name rest in
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
                                ( mk_expr (AilEident (Sym.fresh "bennet_decrement_depth")),
                                  [] )))
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
  ^^ !^"#include <bennet/prelude.h>"
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
