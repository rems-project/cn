module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype

let getter filename (sym : Sym.t) (sct : Sctypes.t)
  : A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition
  =
  let fsym = Sym.fresh (Cn_to_ail.getter_str filename sym) in
  let ct = Sctypes.to_ctype sct in
  A.
    ( ( fsym,
        ( Locations.other __LOC__,
          CF.Annot.Attrs [],
          A.Decl_function
            ( false,
              (C.no_qualifiers, C.mk_ctype_pointer C.no_qualifiers ct),
              [],
              false,
              false,
              false ) ) ),
      ( fsym,
        ( Locations.other __LOC__,
          0,
          CF.Annot.Attrs [],
          [],
          Utils.mk_stmt
            (AilSblock
               ( [ Utils.create_binding sym ct ],
                 [ Utils.mk_stmt
                     (AilSreturn
                        (Utils.mk_expr
                           (AilEunary (Address, Utils.mk_expr (AilEident sym)))))
                 ] )) ) ) )


let setter filename (sym : Sym.t) (sct : Sctypes.t)
  : A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition
  =
  let fsym = Sym.fresh (Cn_to_ail.setter_str filename sym) in
  let ptr_sym = Sym.fresh "ptr" in
  let ct = Sctypes.to_ctype sct in
  let e_size_ =
    match sct with
    | Array (sct', len) ->
      let ct' = Sctypes.to_ctype sct' in
      A.AilEcall
        ( Utils.mk_expr (AilEident (Sym.fresh "sizeof")),
          [ Utils.mk_expr
              (AilEbinary
                 ( Utils.mk_expr
                     (AilEconst
                        (ConstantInteger
                           (IConstant (Z.of_int (Option.get len), Decimal, None)))),
                   Arithmetic Mul,
                   Utils.mk_expr
                     (AilEident
                        (Sym.fresh
                           (Pp.plain
                              (CF.Pp_ail.pp_ctype ~is_human:false C.no_qualifiers ct'))))
                 ))
          ] )
    | _ ->
      A.AilEcall
        ( Utils.mk_expr (AilEident (Sym.fresh "sizeof")),
          [ Utils.mk_expr
              (AilEident
                 (Sym.fresh
                    (Pp.plain (CF.Pp_ail.pp_ctype ~is_human:false C.no_qualifiers ct))))
          ] )
  in
  A.
    ( ( fsym,
        ( Locations.other __LOC__,
          CF.Annot.Attrs [],
          A.Decl_function
            ( false,
              (C.no_qualifiers, C.void),
              [ ( { C.no_qualifiers with const = true },
                  C.mk_ctype_pointer C.no_qualifiers ct,
                  false )
              ],
              false,
              false,
              false ) ) ),
      ( fsym,
        ( Locations.other __LOC__,
          0,
          CF.Annot.Attrs [],
          [ ptr_sym ],
          Utils.mk_stmt
            (AilSblock
               ( [ Utils.create_binding sym ct;
                   Utils.create_binding ptr_sym (C.mk_ctype_pointer C.no_qualifiers ct)
                 ],
                 [ Utils.mk_stmt
                     (AilSexpr
                        (Utils.mk_expr
                           (AilEcall
                              ( Utils.mk_expr (AilEident (Sym.fresh "memcpy")),
                                List.map
                                  Utils.mk_expr
                                  [ AilEunary (Address, Utils.mk_expr (AilEident sym));
                                    AilEident ptr_sym;
                                    e_size_
                                  ] ))))
                 ] )) ) ) )


let accessor filename (sym : Sym.t) (sct : Sctypes.t)
  : (A.sigma_declaration * CF.GenTypes.genTypeCategory A.sigma_function_definition) list
  =
  [ getter filename sym sct; setter filename sym sct ]


let accessors filename (cabs_tunit : CF.Cabs.translation_unit) (prog5 : unit Mucore.file)
  : A.sigma_declaration list
    * CF.GenTypes.genTypeCategory A.sigma_function_definition list
  =
  Cn_to_ail.extract_global_variables cabs_tunit prog5
  |> List.map (fun (sym, ct) ->
    let sct = Sctypes.of_ctype_unsafe (Locations.other __LOC__) ct in
    accessor filename sym sct)
  |> List.flatten
  |> List.split


let accessors_prototypes
      (filename : string)
      (cabs_tunit : CF.Cabs.translation_unit)
      (prog5 : unit Mucore.file)
  : string
  =
  let decls, _ = accessors filename cabs_tunit prog5 in
  Pp.(
    plain
      (separate
         hardline
         (CF.Pp_ail.(
            with_executable_spec (fun () ->
              List.map (fun (sym, (_, _, decl)) -> pp_function_prototype sym decl) decls))
            ())
       ^^ hardline))


let accessors_str
      (filename : string)
      (cabs_tunit : CF.Cabs.translation_unit)
      (prog5 : unit Mucore.file)
  : string
  =
  let declarations, function_definitions = accessors filename cabs_tunit prog5 in
  Pp.plain
    CF.Pp_ail.(
      with_executable_spec
        (pp_program ~show_include:true)
        (None, { A.empty_sigma with declarations; function_definitions }))
