[@@@landmark "auto"]

(* copy pasted from pp_core.ml and adapted *)
module Cn_Pp = Pp
open Cerb_frontend
module Pp = Cn_Pp
open Pp.Infix

let comma_list = Cerb_pp_prelude.comma_list

type budget = int option

let pp_ctype ty = Pp.squotes (Pp_core_ctype.pp_ctype ty)

module Loc = Cerb_location

module type CONFIG = sig
  val ansi_format : Cerb_colour.ansi_style list -> string -> string

  val show_locations : bool
end

let pp_symbol sym = !^(Pp_symbol.to_string sym)
(* NOTE: Used to distinguish struct/unions globally *)

module Pp_typ = struct
  let pp_bt = BaseTypes.pp

  let pp_ct = Sctypes.pp

  let pp_ft = ArgumentTypes.pp ReturnTypes.pp

  let pp_rt = ReturnTypes.pp

  let pp_st _ = Pp.string "todo: implement struct type printer"
end

let rec pp_core_object_type = function
  | Core.OTy_integer -> !^"integer"
  | OTy_floating -> !^"floating"
  | OTy_pointer -> !^"pointer"
  | OTy_array bty ->
    (* TODO: THIS IS NOT BEING PARSED CORRECTLY *)
    !^"array" ^^ Pp.parens (pp_core_object_type bty)
  | OTy_struct ident -> !^"struct" ^^^ pp_symbol ident
  | OTy_union ident -> !^"union" ^^^ pp_symbol ident


let rec pp_core_base_type = function
  | Core.BTy_storable -> !^"storable"
  | BTy_object bty -> pp_core_object_type bty
  | BTy_loaded bty -> !^"loaded" ^^^ pp_core_object_type bty
  | BTy_boolean -> !^"boolean"
  | BTy_ctype -> !^"ctype"
  | BTy_unit -> !^"unit"
  | BTy_list bTy -> Pp.brackets (pp_core_base_type bTy)
  | BTy_tuple bTys -> Pp.parens (Pp.separate_map Pp.comma pp_core_base_type bTys)


module Make (Config : CONFIG) = struct
  open Config
  open Mucore
  include Pp_typ

  let pp_keyword w = !^(ansi_format [ Bold; Magenta ] w)

  let pp_const c = !^(ansi_format [ Magenta ] c)

  let pp_control w = !^(ansi_format [ Bold; Blue ] w)

  let pp_raw_symbol a = !^(ansi_format [ Blue ] (Pp_symbol.to_string a))

  let pp_number n = !^(ansi_format [ Yellow ] n)

  let pp_impl i =
    Pp.angles
      !^(ansi_format [ Yellow ] (Implementation.string_of_implementation_constant i))


  let pp_undef ub =
    Pp.angles
      (Pp.angles !^(ansi_format [ Magenta ] (Undefined.stringFromUndefined_behaviour ub)))


  let maybe_print_location : Loc.t -> Pp.document =
    fun loc ->
    if not show_locations then
      Pp.empty
    else if Locations.is_unknown_location loc then
      !^"{--}"
    else
      !^"{-" ^^ Cerb_location.pp_location ~clever:true loc ^^ !^"-}"


  let precedence = function
    | PEop (OpExp, _, _) -> Some 1
    | PEop (OpMul, _, _) | PEop (OpDiv, _, _) | PEop (OpRem_t, _, _) | PEop (OpRem_f, _, _)
      ->
      Some 2
    | PEop (OpAdd, _, _) | PEop (OpSub, _, _) -> Some 3
    | PEop (OpLt, _, _) | PEop (OpLe, _, _) -> Some 4
    | PEop (OpGt, _, _) | PEop (OpGe, _, _) -> Some 4
    | PEop (OpEq, _, _) -> Some 5
    | PEop (OpAnd, _, _) -> Some 6
    | PEop (OpOr, _, _) -> Some 7
    | PEval _ | PEconstrained _ | PEsym _ | PEctor _ | PEarray_shift _ | PEmember_shift _
    | PEnot _ | PEstruct _ | PEunion _ | PEcfunction _ | PEmemberof _ | PEconv_int _
    | PEconv_loaded_int _ | PEwrapI _ | PElet _ | PEif _ | PEundef _ | PEerror _
    | PEbitwise_unop (_, _)
    | PEbitwise_binop (_, _, _)
    | Cfvfromint _
    | Civfromfloat (_, _)
    | PEapply_fun (_, _)
    | PEbool_to_integer _
    | PEcatch_exceptional_condition (_, _)
    | PEbounded_binop (_, _, _, _)
    | PEis_representable_integer (_, _) ->
      None


  let precedence_expr = function
    | Epure _ | Ememop _ | Eaction _ | Eskip | Eccall _ | Eunseq _ | CN_progs _ -> None
    | Ebound _ -> None
    | End _ -> None
    | Eif _ -> Some 1
    | Elet _ -> Some 2
    | Esseq _ -> Some 3
    | Ewseq _ -> Some 4
    | Erun _ -> None


  let compare_precedence p1 p2 =
    match (p1, p2) with Some n1, Some n2 -> n1 <= n2 | _ -> true


  let pp_function = Mucore.pp_function

  let pp_binop = function
    | Core.OpAdd -> Pp.plus
    | OpSub -> Pp.minus
    | OpMul -> Pp.star
    | OpDiv -> Pp.slash
    | OpRem_t -> pp_keyword "rem_t"
    | OpRem_f -> pp_keyword "rem_f"
    | OpExp -> Pp.caret
    | OpEq -> Pp.equals
    | OpLt -> Pp.langle ()
    | OpLe -> Pp.langle () ^^ Pp.equals
    | OpGt -> Pp.rangle ()
    | OpGe -> Pp.rangle () ^^ Pp.equals
    | OpAnd -> !^"/\\"
    | OpOr -> !^"\\/"


  let pp_iop = function
    | Core.IOpAdd -> Pp.plus
    | IOpSub -> Pp.minus
    | IOpMul -> Pp.star
    | IOpShl -> Pp.langle () ^^ Pp.langle ()
    | IOpShr -> Pp.rangle () ^^ Pp.rangle ()


  let pp_bound = function
    | Bound_Wrap act -> !^"wrap<" ^^ pp_ct act.ct ^^ !^">"
    | Bound_Except act -> !^"check<" ^^ pp_ct act.ct ^^ !^">"


  let pp_polarity = function
    | Core.Pos -> fun z -> z
    | Neg -> fun z -> pp_keyword "neg" ^^ Pp.parens z


  let pp_name = function Core.Sym a -> pp_symbol a | Impl i -> pp_impl i

  let pp_memory_order = function
    | Cmm_csem.NA -> !^"NA"
    | Cmm_csem.Seq_cst -> pp_keyword "seq_cst"
    | Cmm_csem.Relaxed -> pp_keyword "relaxed"
    | Cmm_csem.Release -> pp_keyword "release"
    | Cmm_csem.Acquire -> pp_keyword "acquire"
    | Cmm_csem.Consume -> pp_keyword "consume"
    | Cmm_csem.Acq_rel -> pp_keyword "acq_rel"


  let pp_linux_memory_order = function
    | Linux.Once -> pp_keyword "once"
    | Linux.LAcquire -> pp_keyword "acquire"
    | Linux.LRelease -> pp_keyword "release"
    | Linux.Rmb -> pp_keyword "rmb"
    | Linux.Wmb -> pp_keyword "wmb"
    | Linux.Mb -> pp_keyword "mb"
    | Linux.RbDep -> pp_keyword "rb-dep"
    | Linux.RcuLock -> pp_keyword "rcu-lock"
    | Linux.RcuUnlock -> pp_keyword "rcu-unlock"
    | Linux.SyncRcu -> pp_keyword "sync-rcu"


  let pp_mem_addr (pref, _addr) =
    Pp.at ^^ Pp.braces (Pp_symbol.pp_prefix pref ^^ Pp.colon ^^^ !^"TODO(addr)")


  let pp_actype (actype : act) = pp_ct actype.ct

  let pp_thread_id n = !^("th_" ^ string_of_int n)

  let rec pp_object_value (OV (_bty, ov)) =
    match ov with
    | OVinteger ival -> Impl_mem.pp_integer_value_for_core ival
    | OVfloating fval ->
      Impl_mem.case_fval
        fval
        (fun () -> !^"unspec(floating)")
        (fun fval -> !^(string_of_float fval))
    | OVpointer ptr_val -> Impl_mem.pp_pointer_value ptr_val
    | OVarray lvals ->
      pp_const "Array" ^^ Pp.parens (Pp.nest 1 (comma_list pp_object_value lvals))
    | OVstruct (tag_sym, xs) ->
      Pp.parens (pp_const "struct" ^^^ pp_raw_symbol tag_sym)
      ^^ Pp.braces
           (comma_list
              (fun (Symbol.Identifier (_, ident), _, mval) ->
                 Pp.dot ^^ !^ident ^^ Pp.equals ^^^ Impl_mem.pp_mem_value mval)
              xs)
    | OVunion (tag_sym, Symbol.Identifier (_, ident), mval) ->
      Pp.parens (pp_const "union" ^^^ pp_raw_symbol tag_sym)
      ^^ Pp.braces (Pp.dot ^^ !^ident ^^ Pp.equals ^^^ Impl_mem.pp_mem_value mval)


  and pp_value (V (_bty, v)) =
    match v with
    | Vunit -> pp_const "Unit"
    | Vtrue -> pp_const "True"
    | Vfalse -> pp_const "False"
    | Vlist (_, cvals) -> Pp.brackets (comma_list pp_value cvals)
    | Vtuple cvals -> Pp.parens (comma_list pp_value cvals)
    | Vobject oval -> pp_object_value oval
    | Vctype ct -> Pp.squotes (pp_ctype ct)
    | Vfunction_addr sym -> Pp.parens (Pp.ampersand ^^^ pp_symbol sym)


  let pp_ctor = function
    | Cnil _ -> !^"Nil"
    | Ccons -> !^"Cons"
    | Ctuple -> !^"Tuple"
    | Carray -> !^"Array"


  let rec pp_pattern (Pattern (_, _, _, pat)) =
    match pat with
    | CaseBase (None, bTy) -> Pp.underscore ^^ Pp.colon ^^^ pp_core_base_type bTy
    | CaseBase (Some sym, bTy) -> pp_symbol sym ^^ Pp.colon ^^^ pp_core_base_type bTy
    | CaseCtor (Ctuple, pats) -> Pp.parens (comma_list pp_pattern pats)
    | CaseCtor (ctor, pats) -> pp_ctor ctor ^^ Pp.parens (comma_list pp_pattern pats)


  let abbreviated = Pp.dot ^^ Pp.dot ^^ Pp.dot

  let rec pp_actype_or_pexpr budget = function
    | Either.Left ct -> pp_actype ct
    | Right sym -> pp_pexpr budget sym


  and pp_pexpr budget pe =
    let open PPrint in
    let rec pp budget prec (Pexpr (loc, _annots, _, pe)) =
      match budget with
      | Some 0 -> abbreviated
      | _ ->
        let budget' = match budget with Some n -> Some (n - 1) | None -> None in
        let prec' = precedence pe in
        let pp z = Pp.group (pp budget' prec' z) in
        let pp_pexpr pe = Pp.group (pp_pexpr budget' pe) in
        maybe_print_location loc
        ^^^ (if compare_precedence prec' prec then fun z -> z else Pp.parens)
              (match pe with
               | PEval cval -> pp_value cval
               | PEconstrained xs ->
                 pp_keyword "constrained"
                 ^^ Pp.parens
                      (comma_list
                         (fun (cs, pe) ->
                            Pp.brackets
                              (Pp_mem.pp_mem_constraint Impl_mem.pp_integer_value cs)
                            ^^^ Pp.equals
                            ^^ Pp.rangle ()
                            ^^ pp_pexpr pe)
                         xs)
               | PEsym sym -> pp_symbol sym
               | PEctor (Cnil _, _) -> Pp.brackets Pp.empty
               | PEctor (Ctuple, pes) -> Pp.parens (comma_list pp_pexpr pes)
               | PEctor (ctor, pes) -> pp_ctor ctor ^^ Pp.parens (comma_list pp_pexpr pes)
               | PEbitwise_unop (unop, p1) ->
                 let opnm =
                   match unop with
                   | BW_COMPL -> "IvCOMPL"
                   | BW_CTZ -> "builtin_ctz"
                   | BW_FFS -> "builtin_ffs"
                 in
                 !^opnm ^^ Pp.parens (pp_pexpr p1)
               | PEbitwise_binop (binop, p1, p2) ->
                 let opnm =
                   match binop with
                   | BW_OR -> "IvOR"
                   | BW_AND -> "IvAND"
                   | BW_XOR -> "IvXOR"
                 in
                 !^opnm ^^ Pp.parens (separate comma [ pp_pexpr p1; pp_pexpr p2 ])
               | Cfvfromint p1 -> !^"Cfvfromint" ^^ Pp.parens (pp_pexpr p1)
               | Civfromfloat (ct, p1) ->
                 !^"Civfromfloat"
                 ^^ Pp.parens (separate comma [ pp_actype ct; pp_pexpr p1 ])
               | PEarray_shift (pe1, ty, pe2) ->
                 pp_keyword "array_shift"
                 ^^ Pp.parens
                      (pp_pexpr pe1 ^^ Pp.comma ^^^ pp_ct ty ^^ Pp.comma ^^^ pp_pexpr pe2)
               | PEmember_shift (pe, tag_sym, Symbol.Identifier (_, memb_ident)) ->
                 pp_keyword "member_shift"
                 ^^ Pp.parens
                      (pp_pexpr pe
                       ^^ Pp.comma
                       ^^^ pp_raw_symbol tag_sym
                       ^^ Pp.comma
                       ^^^ Pp.dot
                       ^^ !^memb_ident)
               | PEnot pe -> pp_keyword "not" ^^ Pp.parens (pp_pexpr pe)
               | PEop (bop, pe1, pe2) -> pp_pexpr pe1 ^^^ pp_binop bop ^^^ pp_pexpr pe2
               | PEapply_fun (f, args) ->
                 Cn_Pp.c_app (pp_function f) (List.map pp_pexpr args)
               | PEstruct (tag_sym, xs) ->
                 Pp.parens (pp_const "struct" ^^^ pp_raw_symbol tag_sym)
                 ^^ Pp.braces
                      (comma_list
                         (fun (Symbol.Identifier (_, ident), pe) ->
                            Pp.dot ^^ !^ident ^^ Pp.equals ^^^ pp_pexpr pe)
                         xs)
               | PEunion (tag_sym, Symbol.Identifier (_, ident), pe) ->
                 Pp.parens (pp_const "union" ^^^ pp_raw_symbol tag_sym)
                 ^^ Pp.braces (Pp.dot ^^ !^ident ^^ Pp.equals ^^^ pp_pexpr pe)
               | PEcfunction pe -> pp_keyword "cfunction" ^^ Pp.parens (pp pe)
               | PEmemberof (tag_sym, memb_ident, pe) ->
                 pp_keyword "memberof"
                 ^^ Pp.parens
                      (pp_symbol tag_sym
                       ^^ Pp.comma
                       ^^^ Pp_symbol.pp_identifier memb_ident
                       ^^ Pp.comma
                       ^^^ pp_pexpr pe)
               | PEbool_to_integer asym ->
                 pp_keyword "bool_to_integer" ^^ Pp.parens (pp_pexpr asym)
               | PEconv_int (ct_expr, int_expr) ->
                 Cn_Pp.c_app !^"conv_int" [ pp_pexpr ct_expr; pp_pexpr int_expr ]
               | PEconv_loaded_int (ct_expr, int_expr) ->
                 Cn_Pp.c_app !^"conv_loaded_int" [ pp_pexpr ct_expr; pp_pexpr int_expr ]
               | PEwrapI (act, asym) ->
                 !^"wrapI" ^^ Pp.parens (pp_ct act.ct ^^ Pp.comma ^^^ pp_pexpr asym)
               | PEbounded_binop (bound, iop, arg1, arg2) ->
                 !^"bound_op"
                 ^^ Pp.parens
                      (Pp.flow
                         (Pp.comma ^^ Pp.break 1)
                         [ pp_bound bound;
                           Pp.squotes (pp_iop iop);
                           pp_pexpr arg1;
                           pp_pexpr arg2
                         ])
               | PEcatch_exceptional_condition (act, asym) ->
                 !^"catch_exceptional_condition"
                 ^^ Pp.parens (pp_ct act.ct ^^ Pp.comma ^^^ pp_pexpr asym)
               | PEis_representable_integer (asym, act) ->
                 !^"is_representable_integer"
                 ^^ Pp.parens (pp_pexpr asym ^^ Pp.comma ^^^ pp_ct act.ct)
               | PElet (pat, pe1, pe2) ->
                 pp_control "let"
                 ^^^ pp_pattern pat
                 ^^^ Pp.equals
                 ^^^ pp_pexpr pe1
                 ^^^ pp_control "in"
                 ^^ Pp.break 1
                 ^^ pp pe2
               | PEif (pe1, pe2, pe3) ->
                 Pp.group
                   (pp_control "if"
                    ^^^ pp_pexpr pe1
                    ^^^ pp_control "then"
                    ^^ Pp.nest 2 (Pp.break 1 ^^ pp pe2)
                    ^^ Pp.break 1
                    ^^ pp_control "else"
                    ^^ Pp.nest 2 (Pp.break 1 ^^ pp pe3))
               | PEundef (_, ub) -> pp_keyword "undef" ^^ Pp.parens (pp_undef ub)
               | PEerror (str, pe) ->
                 pp_keyword "error"
                 ^^ Pp.parens (Pp.dquotes !^str ^^ Pp.comma ^^^ pp_pexpr pe))
    in
    pp budget None pe


  let pp_action budget act =
    let pp_actype_or_pexpr = pp_actype_or_pexpr budget in
    let pp_pexpr = pp_pexpr budget in
    let pp_args args mo =
      Pp.parens
        (comma_list pp_actype_or_pexpr args
         ^^ (match mo with Cmm_csem.NA -> Pp.empty | _ -> Pp.comma)
         ^^^ pp_memory_order mo)
    in
    match act with
    | Create (al, ty, _) ->
      pp_keyword "create" ^^ Pp.parens (pp_pexpr al ^^ Pp.comma ^^^ pp_actype ty)
    | CreateReadOnly (al, ty, init, _) ->
      pp_keyword "create_readonly"
      ^^ Pp.parens (pp_pexpr al ^^ Pp.comma ^^^ pp_actype ty ^^ Pp.comma ^^^ pp_pexpr init)
    | Alloc (al, n, _) ->
      pp_keyword "alloc" ^^ Pp.parens (pp_pexpr al ^^ Pp.comma ^^^ pp_pexpr n)
    | Kill (Dynamic, e) -> pp_keyword "free" ^^ Pp.parens (pp_pexpr e)
    | Kill (Static ct, e) ->
      pp_keyword "kill" ^^ Pp.parens (pp_ct ct ^^ Pp.comma ^^^ pp_pexpr e)
    | Store (is_locking, ty, e1, e2, mo) ->
      pp_keyword (if is_locking then "store_lock" else "store")
      ^^ pp_args [ Left ty; Right e1; Right e2 ] mo
    | Load (ty, e, mo) -> pp_keyword "load" ^^ pp_args [ Left ty; Right e ] mo
    | RMW (ty, e1, e2, e3, mo1, mo2) ->
      pp_keyword "rmw"
      ^^ Pp.parens
           (pp_actype ty
            ^^ Pp.comma
            ^^^ pp_pexpr e1
            ^^ Pp.comma
            ^^^ pp_pexpr e2
            ^^ Pp.comma
            ^^^ pp_pexpr e3
            ^^ Pp.comma
            ^^^ pp_memory_order mo1
            ^^ Pp.comma
            ^^^ pp_memory_order mo2)
    | Fence mo -> pp_keyword "fence" ^^ Pp.parens (pp_memory_order mo)
    | CompareExchangeStrong (ty, e1, e2, e3, mo1, mo2) ->
      pp_keyword "compare_exchange_strong"
      ^^ Pp.parens
           (pp_actype ty
            ^^ Pp.comma
            ^^^ pp_pexpr e1
            ^^ Pp.comma
            ^^^ pp_pexpr e2
            ^^ Pp.comma
            ^^^ pp_pexpr e3
            ^^ Pp.comma
            ^^^ pp_memory_order mo1
            ^^ Pp.comma
            ^^^ pp_memory_order mo2)
    | CompareExchangeWeak (ty, e1, e2, e3, mo1, mo2) ->
      pp_keyword "compare_exchange_weak"
      ^^ Pp.parens
           (pp_actype ty
            ^^ Pp.comma
            ^^^ pp_pexpr e1
            ^^ Pp.comma
            ^^^ pp_pexpr e2
            ^^ Pp.comma
            ^^^ pp_pexpr e3
            ^^ Pp.comma
            ^^^ pp_memory_order mo1
            ^^ Pp.comma
            ^^^ pp_memory_order mo2)
    | LinuxFence mo -> pp_keyword "linux_fence" ^^ Pp.parens (pp_linux_memory_order mo)
    | LinuxStore (ty, e1, e2, mo) ->
      pp_keyword "linux_store"
      ^^ Pp.parens
           (comma_list pp_actype_or_pexpr [ Left ty; Right e1; Right e2 ]
            ^^ Pp.comma
            ^^^ pp_linux_memory_order mo)
    | LinuxLoad (ty, e, mo) ->
      pp_keyword "linux_load"
      ^^ Pp.parens
           (comma_list pp_actype_or_pexpr [ Left ty; Right e ]
            ^^ Pp.comma
            ^^^ pp_linux_memory_order mo)
    | LinuxRMW (ty, e1, e2, mo) ->
      pp_keyword "linux_rmw"
      ^^ Pp.parens
           (comma_list pp_actype_or_pexpr [ Left ty; Right e1; Right e2 ]
            ^^ Pp.comma
            ^^^ pp_linux_memory_order mo)


  let pp_str_label : Cerb_frontend.Annot.label_annot -> string = function
    | LAloop i -> "while_" ^ string_of_int i
    | LAloop_continue i -> "continue_" ^ string_of_int i
    | LAloop_break i -> "break_" ^ string_of_int i
    | LAreturn -> "return"
    | LAswitch -> "switch"
    | LAcase -> "case"
    | LAdefault -> "default"
    | LAactual_label -> "C-source"


  let pp_str_annot = function
    | Annot.Aloc _ -> []
    | Astd _str -> []
    | Auid _uid -> [ !^"TODO(uid)" ]
    | Amarker n -> [ !^("marker " ^ string_of_int n) ]
    | Amarker_object_types n -> [ !^("marker_object_types " ^ string_of_int n) ]
    | Abmc annot -> (match annot with Abmc_id id -> [ !^(string_of_int id) ])
    | Atypedef sym -> [ pp_symbol sym ]
    | Aattrs _ -> [ !^"TODO(Aattrs)" ]
    | Alabel label -> [ !^(pp_str_label label) ]
    | Acerb _ -> []
    | Avalue (Ainteger ity) -> [ !^"type" ^^^ Pp_core_ctype.pp_integer_ctype ity ]
    | Ainlined_label (_, s, _) -> [ !^"inlined" ^^^ pp_symbol s ]
    | Astmt -> [ !^"stmt" ]
    | Aexpr -> [ !^"expr" ]


  let pp_annots annots =
    Pp.flow_map
      (Pp.break 0)
      (fun annot -> !^"{-#" ^^ annot ^^ !^"#-}")
      (List.concat_map pp_str_annot annots)


  let do_annots annot doc = pp_annots annot ^^ doc

  let pp_expr budget (expr : 'ty expr) =
    let pp_pexpr = pp_pexpr budget in
    let pp_actype_or_pexpr = pp_actype_or_pexpr budget in
    let pp_action = pp_action budget in
    let rec pp budget (Expr (loc, annot, _, e) : 'ty expr) =
      match budget with
      | Some 0 -> abbreviated
      | _ ->
        let budget' = match budget with Some n -> Some (n - 1) | None -> None in
        let pp z = pp budget' z in
        do_annots
          annot
          (maybe_print_location loc
           ^^^
           match (e : 'ty expr_) with
           | Epure pe -> pp_keyword "pure" ^^ Pp.parens (pp_pexpr pe)
           | Ememop memop ->
             let aux memop =
               let open Mem_common in
               let mctype ct1 = Either.Left ct1 in
               let msym sym1 = Either.Right sym1 in
               match memop with
               | Mucore.PtrEq (sym1, sym2) -> (PtrEq, [ msym sym1; msym sym2 ])
               | PtrNe (sym1, sym2) -> (PtrNe, [ msym sym1; msym sym2 ])
               | PtrLt (sym1, sym2) -> (PtrLt, [ msym sym1; msym sym2 ])
               | PtrGt (sym1, sym2) -> (PtrGt, [ msym sym1; msym sym2 ])
               | PtrLe (sym1, sym2) -> (PtrLe, [ msym sym1; msym sym2 ])
               | PtrGe (sym1, sym2) -> (PtrGe, [ msym sym1; msym sym2 ])
               | Ptrdiff (ct1, sym1, sym2) ->
                 (Ptrdiff, [ mctype ct1; msym sym1; msym sym2 ])
               | IntFromPtr (ct1, ct2, sym1) ->
                 (IntFromPtr, [ mctype ct1; mctype ct2; msym sym1 ])
               | PtrFromInt (ct1, ct2, sym1) ->
                 (PtrFromInt, [ mctype ct1; mctype ct2; msym sym1 ])
               | PtrValidForDeref (ct1, sym1) ->
                 (PtrValidForDeref, [ mctype ct1; msym sym1 ])
               | PtrWellAligned (ct1, sym1) -> (PtrWellAligned, [ mctype ct1; msym sym1 ])
               | PtrArrayShift (sym1, ct1, sym2) ->
                 (PtrArrayShift, [ msym sym1; mctype ct1; msym sym2 ])
               | PtrMemberShift (tag_sym, memb_ident, sym) ->
                 (PtrMemberShift (tag_sym, memb_ident), [ msym sym ])
               | Memcpy (sym1, sym2, sym3) -> (Memcpy, [ msym sym1; msym sym2; msym sym3 ])
               | Memcmp (sym1, sym2, sym3) -> (Memcmp, [ msym sym1; msym sym2; msym sym3 ])
               | Realloc (sym1, sym2, sym3) ->
                 (Realloc, [ msym sym1; msym sym2; msym sym3 ])
               | Va_start (sym1, sym2) -> (Va_start, [ msym sym1; msym sym2 ])
               | Va_copy sym1 -> (Va_copy, [ msym sym1 ])
               | Va_arg (sym1, ct1) -> (Va_arg, [ msym sym1; mctype ct1 ])
               | Va_end sym1 -> (Va_end, [ msym sym1 ])
               | CopyAllocId (sym1, sym2) -> (Copy_alloc_id, [ msym sym1; msym sym2 ])
             in
             let memop, pes = aux memop in
             pp_keyword "memop"
             ^^ Pp.parens
                  (Pp_mem.pp_memop memop ^^ Pp.comma ^^^ comma_list pp_actype_or_pexpr pes)
           | Eaction (Paction (p, Action (_, act))) -> pp_polarity p (pp_action act)
           | Eskip -> pp_keyword "skip"
           | Eccall (pe_ty, pe, pes) ->
             pp_keyword "ccall"
             ^^ Pp.parens (pp_ct pe_ty.ct)
             ^^ Pp.parens
                  (comma_list
                     pp_actype_or_pexpr
                     (Right pe :: (List.map (fun pe -> Either.Right pe)) pes))
           | CN_progs (_, stmts) ->
             pp_keyword "cn_prog"
             ^^ Pp.parens
                  (* use the AST printer to at least print something, TODO improve *)
                  (Cn_Pp.list Pp_ast.pp_doc_tree (List.map Cnprog.dtree stmts))
           | Eunseq es -> pp_control "unseq" ^^ Pp.parens (comma_list pp es)
           | Elet (pat, pe1, e2) ->
             Pp.group
               (Pp.prefix
                  0
                  1
                  (pp_control "let"
                   ^^^ pp_pattern pat
                   ^^^ Pp.equals
                   ^^^ pp_pexpr pe1
                   ^^^ pp_control "in")
                  (pp e2))
           | Eif (pe1, e2, e3) ->
             pp_control "if"
             ^^^ pp_pexpr pe1
             ^^^ pp_control "then"
             ^^ Pp.nest 2 (Pp.break 1 ^^ pp e2)
             ^^ Pp.break 1
             ^^ pp_control "else"
             ^^ Pp.nest 2 (Pp.break 1 ^^ pp e3)
           | Ewseq (pat, e1, e2) ->
             Pp.group
               (pp_control "let weak"
                ^^^ pp_pattern pat
                ^^^ Pp.equals
                ^^^
                let doc_e1 = pp e1 in
                Pp.ifflat doc_e1 (Pp.nest 2 (Pp.break 1 ^^ doc_e1)) ^^^ pp_control "in")
             ^^ Pp.break 1
             ^^ pp e2
           | Esseq (pat, e1, e2) ->
             Pp.group
               (pp_control "let strong"
                ^^^ pp_pattern pat
                ^^^ Pp.equals
                ^^^
                let doc_e1 = pp e1 in
                Pp.ifflat doc_e1 (Pp.nest 2 (Pp.break 1 ^^ doc_e1)) ^^^ pp_control "in")
             ^^ Pp.break 1
             ^^ pp e2
           | End es -> pp_keyword "nd" ^^ Pp.parens (comma_list pp es)
           | Ebound e -> Cn_Pp.c_app (pp_keyword "bound") [ pp e ]
           | Erun (sym, pes) ->
             pp_keyword "run" ^^^ Cn_Pp.c_app (pp_symbol sym) (List.map pp_pexpr pes))
    in
    pp budget expr


  let symbol_compare = Symbol.instance_Basic_classes_Ord_Symbol_sym_dict.compare_method

  let pp_tagDefinitions tagDefs =
    let tagDefs = Pmap.bindings_list tagDefs in
    let pp (sym, tagDef) =
      match tagDef with
      | StructDef sd ->
        pp_keyword "def"
        ^^^ pp_keyword "struct"
        ^^^ pp_raw_symbol sym
        ^^^ Pp.colon
        ^^ Pp.equals
        ^^ pp_st sd
      | UnionDef -> pp_keyword "def" ^^^ pp_keyword "union" ^^^ pp_raw_symbol sym
    in
    Pp.separate_map (Pp.break 1 ^^ Pp.break 1) pp tagDefs


  let pp_argument (sym, bTy) = pp_symbol sym ^^ Pp.colon ^^^ pp_bt bTy

  let pp_params params = Pp.parens (comma_list pp_argument params)

  let rec pp_arguments_l ppf = function
    | Define ((s, it), _info, l) ->
      Pp.parens (!^"let" ^^^ pp_symbol s ^^^ Pp.equals ^^^ IndexTerms.pp it)
      ^^^ pp_arguments_l ppf l
    | Resource ((s, (re, _bt)), _info, l) ->
      Pp.parens (!^"let" ^^^ pp_symbol s ^^^ Pp.equals ^^^ Request.pp re)
      ^^^ pp_arguments_l ppf l
    | Constraint (lc, _info, l) ->
      Pp.parens (LogicalConstraints.pp lc) ^^^ pp_arguments_l ppf l
    | I i -> !^"->" ^^^ Pp.parens (ppf i)


  let rec pp_arguments ppf = function
    | Computational ((s, bt), _info, a) ->
      Pp.parens (Cn_Pp.typ (pp_symbol s) (pp_bt bt)) ^^^ pp_arguments ppf a
    | L l -> pp_arguments_l ppf l


  let pp_fun_map budget funs =
    let pp_cond loc d = if Cerb_location.from_main_file loc then d else Pp.empty in
    Pmap.fold
      (fun sym decl acc ->
         acc
         ^^ (match decl with
             | ProcDecl (loc, ft) ->
               pp_cond loc
               @@ pp_keyword "proc"
               ^^^ pp_symbol sym
               ^^ Pp.colon
               ^^^ Cn_Pp.option pp_ft "(no spec)" ft
             | Proc { loc; args_and_body; _ } ->
               pp_cond loc
               @@ pp_keyword "proc"
               ^^^ pp_symbol sym
               ^^^ Pp.equals
               ^^^ pp_arguments
                     (fun (body, labels, _rt) ->
                        Pp.equals
                        ^^^ Pp.hardline
                        ^^ Pp.nest
                             2
                             ((* pp label definitions *)
                              Pmap.fold
                                (fun sym def acc ->
                                   acc
                                   ^^ (match def with
                                       | Return _ ->
                                         Pp.break 1 ^^ !^"return label" ^^^ pp_symbol sym
                                       | Label
                                           ( _loc,
                                             label_args_and_body,
                                             _annots,
                                             _,
                                             _loop_info ) ->
                                         Pp.break 1
                                         ^^ !^"label"
                                         ^^^ pp_symbol sym
                                         ^^ Pp.equals
                                         ^^^ pp_arguments
                                               (fun label_body ->
                                                  (* label core function definition *)
                                                  Pp.nest
                                                    2
                                                    (Pp.break 1
                                                     ^^ pp_expr budget label_body))
                                               label_args_and_body)
                                   ^^ Pp.hardline)
                                labels
                                Pp.empty
                              (* pp body *)
                              ^^ Pp.break 1
                              ^^ !^"body"
                              ^^^ Pp.equals
                              ^^^ Pp.nest 2 (Pp.break 1 ^^ pp_expr budget body)))
                     args_and_body)
         ^^ Pp.hardline
         ^^ Pp.hardline)
      funs
      Pp.empty


  let pp_extern_symmap symmap =
    !^"-- Extern symbols map:" ^^ Pp.break 1
    |> Pmap.fold
         (fun sym_from sym_to acc ->
            acc
            ^^ pp_raw_symbol sym_from
            ^^^ !^"->"
            ^^^ pp_raw_symbol sym_to
            ^^ Pp.break 1)
         symmap


  let mk_comment doc =
    Cerb_colour.pp_ansi_format [ Red ] (fun () ->
      !^"{-" ^^ Pp.break 1 ^^ doc ^^ Pp.break 1 ^^ !^"-}")


  let pp_globs budget globs =
    List.fold_left
      (fun acc (sym, decl) ->
         match decl with
         | GlobalDef (gt, e) ->
           acc
           ^^ pp_keyword "glob"
           ^^^ pp_symbol sym
           ^^ Pp.colon
           ^^^ Pp.brackets (!^"ct" ^^^ Pp.equals ^^^ pp_ct gt)
           ^^^ Pp.colon
           ^^ Pp.equals
           ^^ Pp.nest 2 (Pp.break 1 ^^ pp_expr budget e)
           ^^ Pp.break 1
           ^^ Pp.break 1
         | GlobalDecl _ -> acc)
      Pp.empty
      globs


  let pp_file budget file =
    let show_aggregate = not @@ Pmap.is_empty file.tagDefs in
    let show_globs = file.globs != [] in
    let guard b doc = if b then doc else Pp.empty in
    guard
      show_aggregate
      (!^"-- Aggregates"
       ^^ Pp.break 1
       ^^ pp_tagDefinitions file.tagDefs
       ^^ Pp.break 1
       ^^ Pp.break 1)
    ^^ guard show_globs (!^"-- Globals" ^^ Pp.break 1 ^^ pp_globs budget file.globs)
    ^^ guard (show_aggregate || show_globs) (!^"-- Fun map" ^^ Pp.break 1)
    ^^ pp_fun_map budget file.funs


  let string_of_core_base_type bTy = Pp_utils.to_plain_string (pp_bt bTy)

  let string_of_value cval = Pp_utils.to_plain_string (pp_value cval)

  let string_of_action act = Pp_utils.to_plain_string (pp_action None act)

  let string_of_pexpr pe = Pp_utils.to_plain_string (pp_pexpr None pe)

  let string_of_expr e = Pp_utils.to_plain_string (pp_expr None e)

  let string_of_texpr e = Pp_utils.to_plain_string (pp_expr None e)

  let string_of_file f = Pp_utils.to_plain_string (pp_file None f)
end

module Pp_standard_typ = struct
  let pp_bt = pp_core_base_type

  let pp_ct ty = Pp.squotes (Pp_core_ctype.pp_ctype ty)

  let pp_ut membrs =
    let _ty, tags = ("union", membrs) in
    let pp_tag (Symbol.Identifier (_, name), (_, _, _, ct)) =
      !^name ^^ Pp.colon ^^^ pp_ct ct
    in
    Pp.nest 2 (Pp.break 1 ^^ Pp.separate_map (Pp.break 1) pp_tag tags)


  let pp_st (membrs_, flexible_opt) =
    let membrs =
      match flexible_opt with
      | None -> membrs_
      | _ -> failwith "have to implement that again"
    in
    let _ty, tags = ("struct", membrs) in
    let pp_tag (Symbol.Identifier (_, name), (_, _, _, ct)) =
      !^name ^^ Pp.colon ^^^ pp_ct ct
    in
    Pp.nest 2 (Pp.break 1 ^^ Pp.separate_map (Pp.break 1) pp_tag tags)


  let pp_rt ret_bt = pp_core_base_type ret_bt

  let pp_ft (ret_bt, params) =
    pp_core_base_type ret_bt ^^ Pp.parens (Pp.flow_map Pp.comma pp_bt params)


  let pp_lt params =
    comma_list
      (fun (_, (ty, by_pointer)) ->
         if by_pointer then pp_ctype ty else pp_ctype ty ^^^ Pp.parens (Pp.string "val"))
      params
end

module Basic = Make (struct
    let ansi_format _ s = s

    let show_locations = false
  end)

module WithLocations = Make (struct
    let ansi_format _ s = s

    let show_locations = true
  end)

let pp_budget () = Some (!Cerb_debug.debug_level + !Cn_Pp.print_level + 6)

let pp_pexpr_w b e = Basic.pp_pexpr b e

let pp_pexpr e = pp_pexpr_w (pp_budget ()) e

let pp_expr_w b e = Basic.pp_expr b e

let pp_expr e = pp_expr_w (pp_budget ()) e

let pp_file file =
  if !Cerb_debug.debug_level > 1 then
    WithLocations.pp_file None file
  else
    Basic.pp_file None file
