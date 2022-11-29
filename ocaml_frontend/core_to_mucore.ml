(*Generated by Lem from frontend/model/core_anormalise.lem.*)
open Lem_pervasives
open Ctype
open Lem_assert_extra
open Milicore

(* Can be removed if support for OCaml 4.07 is dropped. *)
module Option = struct
  type 'a t = 'a option

  let equal eq o1 o2 =
    match o1, o2 with
    | None   , None    -> true
    | Some _ , None
    | None   , Some _  -> false
    | Some v1, Some v2 -> eq v1 v2
end


module SymSet = 
  Set.Make(struct
      let compare = Symbol.symbol_compare
      type t = Symbol.sym
    end)


open Core
open Annot

open Debug_ocaml


module Mu = Mucore.Make(Mucore.SimpleTypes)
open Mu

module Loc = Mucore.Loc
type symbol = Symbol.sym


(* The a-normalisation should happen after some partial evaluation and
   rewrites that remove expressions passing ctypes and function
   pointers as values. The embedding into mucore then is partial in
   those places. *)


(* type bty = core_base_type *)
type value = Symbol.sym generic_value
type values = value list
type 'bty pexpr = ('bty, Symbol.sym) generic_pexpr
type 'bty pexprs = ('bty pexpr) list
type ('a, 'bty) expr = ('a, 'bty, Symbol.sym) generic_expr
type annot = Annot.annot
type annots = annot list
type outer_annots = annots
type ('a, 'bty) action1 = ('a, 'bty, Symbol.sym) generic_action
type ('a, 'bty) paction = ('a, 'bty, Symbol.sym) generic_paction

type mu_value = unit Mu.mu_value
type mu_values = mu_value list
type mu_pexpr = unit Mu.mu_pexpr
type mu_pexprs = mu_pexpr list
type mu_expr = unit Mu.mu_expr
type mu_pattern = Mu.mu_pattern
type mu_action = unit Mu.mu_action
type mu_paction = unit Mu.mu_paction
type mu_sym_or_pattern = unit Mu.mu_sym_or_pattern


let always_explode_eif:bool=  false


module type LocationCheck = sig

  val good_location : Location_ocaml.t -> bool

end





module Make (L : LocationCheck) = struct

(* include other things to ignore *)
let update_loc loc1 loc2 = 
  if L.good_location loc2 then loc2 else loc1


(* ... adapting the algorithm from
   http://matt.might.net/articles/a-normalization/ for core *)




let ensure_ctype__pexpr loc = function
  | Core.Pexpr (annots, bty, Core.PEval (Core.Vctype ct)) -> 
     Some (act_pack loc annots bty ct)
  | _ -> None


let loc_error loc msg = error (msg ^ " (" ^ Loc.location_to_string loc ^ ")")

let fensure_ctype__pexpr loc err pe : 'TY act = 
  match ensure_ctype__pexpr loc pe with
  | Some ctype -> ctype
  | None -> loc_error loc err









let rec core_to_mu__pattern loc (Core.Pattern (annots, pat_)) : mu_pattern = 
  let loc = update_loc loc (Annot.get_loc_ annots) in
  let wrap pat_ = M_Pattern(loc, annots, pat_) in
  match pat_ with
  | Core.CaseBase (msym, bt1) -> 
     wrap (M_CaseBase (msym, bt1))
  | Core.CaseCtor(ctor, pats) -> 
     let pats = map (core_to_mu__pattern loc) pats in
     match ctor with
     | Core.Cnil bt1 -> wrap (M_CaseCtor (M_Cnil bt1, pats))
     | Core.Ccons -> wrap (M_CaseCtor (M_Ccons, pats))
     | Core.Ctuple -> wrap (M_CaseCtor (M_Ctuple, pats))
     | Core.Carray -> wrap (M_CaseCtor (M_Carray, pats))
     | Core.Cspecified -> List.hd pats
     | _ -> loc_error loc ("core_to_mucore: unsupported pattern")




type ('bound, 'body) letbinder = 
  Loc.t -> Annot.annot list -> mu_sym_or_pattern -> 'bound -> 'body -> 'body

type 'd n_pexpr_domain = 
  { d_let : (mu_pexpr, 'd) letbinder;
    (* d_case: Loc.t -> Annot.annot list -> mu_pexpr -> (mu_pattern * 'd) list -> 'd; *)
    (* d_if: Loc.t -> Annot.annot list -> mu_pexpr -> 'd -> 'd -> 'd; *)
    (* d_undef : Loc.t -> Annot.annot list -> (Loc.t * Undefined.undefined_behaviour) -> 'd; *)
    (* d_error : Loc.t -> Annot.annot list -> (string * mu_pexpr) -> 'd; *)
  }


(* let letbind_pexpr_ sym domain pexpr ctxt : 'a =  *)
(*   let (M_Pexpr (loc, _, bty, _)) = pexpr in *)
(*   let body = ctxt (M_Pexpr (loc, [], bty, M_PEsym sym)) in *)
(*   domain.d_let loc [] (M_Symbol sym) pexpr body *)
 
(* let letbind_pexpr domain pexpr ctxt =  *)
(*   letbind_pexpr_ (Symbol.fresh ()) domain pexpr ctxt *)



let letbinder_pexpr_in_pexpr loc annots pat pexpr body : mu_pexpr = 
  M_Pexpr (loc, annots, (), M_PElet (pat, pexpr, body))

(* let case_switch_pexpr_in_pexpr loc annots asym cases : mu_tpexpr =  *)
(*   M_TPexpr (loc, annots, (), M_PEcase (asym, cases)) *)

(* let if_pexpr_in_pexpr loc annots asym pte1 pte2 : mu_pexpr =  *)
(*   M_Pexpr (loc, annots, (), M_PEif (asym, pte1, pte2)) *)

(* let undef_pexpr_in_pexpr loc annots (uloc, undef) : mu_tpexpr = *)
(*   M_TPexpr (loc, annots, (), M_PEundef (uloc, undef)) *)

(* let error_pexpr_in_pexpr loc annots (str, asym) : mu_tpexpr = *)
(*   M_TPexpr (loc, annots, (), M_PEerror (str, asym)) *)


let letbinder_pexpr_in_expr loc annots pat pexpr body : mu_expr = 
  M_Expr (loc, annots, M_Elet (pat, pexpr, body))

(* let case_switch_pexpr_in_expr loc annots asym cases : mu_texpr =  *)
(*   M_TExpr (loc, annots, M_Ecase (asym, cases)) *)

(* let if_pexpr_in_expr loc annots asym pte1 pte2 : mu_texpr =  *)
(*   M_TExpr (loc, annots, M_Eif (asym, pte1, pte2)) *)

(* let undef_pexpr_in_expr loc annots (uloc, undef) : mu_texpr = *)
(*   M_TExpr (loc, annots, M_Eundef (uloc, undef)) *)

(* let error_pexpr_in_expr loc annots (str, asym) : mu_texpr = *)
(*   M_TExpr (loc, annots, M_Eerror (str, asym)) *)



let pexpr_n_pexpr_domain = { 
    d_let = letbinder_pexpr_in_pexpr;
    (* d_case = case_switch_pexpr_in_pexpr; *)
    (* d_if = if_pexpr_in_pexpr; *)
    (* d_undef = undef_pexpr_in_pexpr; *)
    (* d_error = error_pexpr_in_pexpr; *)
  }

let expr_n_pexpr_domain = { 
    d_let = letbinder_pexpr_in_expr;
    (* d_case = case_switch_pexpr_in_expr; *)
    (* d_if = if_pexpr_in_expr; *)
    (* d_undef = undef_pexpr_in_expr; *)
    (* d_error = error_pexpr_in_expr; *)
  }





let rec n_ov loc v =
  match v with
  | Core.OVinteger iv -> M_OVinteger iv
  | Core.OVfloating fv -> M_OVfloating fv
  | Core.OVpointer pv -> M_OVpointer pv
  | Core.OVarray is -> M_OVarray (List.map (n_lv loc) is)
  | Core.OVstruct (sym1, is) -> M_OVstruct (sym1, is)
  | Core.OVunion (sym1, id1, mv) -> M_OVunion (sym1, id1, mv)

and n_lv loc v =
  match v with
  | LVspecified ov -> (* M_LVspecified *) (n_ov loc ov)
  | LVunspecified ct1 -> error "core_anormalisation: LVunspecified"


and n_val loc v =
  (* print_endline ("\n\n\n*******************************************************\nnormalising value\n");
   * PPrint.ToChannel.compact stdout (Pp_core_ast.pp_expr (Expr ([], (Epure (Pexpr ([], (), PEval v))))));
   * print_endline "\n";
   * flush stdout; *)
  match v with
  | Vobject ov -> M_Vobject (n_ov loc ov)
  | Vloaded lv -> M_Vobject (n_lv loc lv)
  | Vunit -> M_Vunit
  | Vtrue -> M_Vtrue
  | Vfalse -> M_Vfalse
  | Vctype ct1 -> 
     let err = "core_anormalisation: Vctype" in
     error (err ^ " (" ^ Loc.location_to_string loc ^ ")")
  | Vlist (cbt, vs) -> M_Vlist (cbt, (List.map (n_val loc) vs))
  | Vtuple vs -> M_Vtuple (List.map (n_val loc) vs)


let unit_pat loc annots = 
  M_Pattern (loc, annots, M_CaseBase (None, BTy_unit))



let rec n_pexpr loc (Pexpr (annots, bty, pe)) : mu_pexpr =
  let loc = update_loc loc (get_loc_ annots) in
  let annotate pe = M_Pexpr (loc, annots, bty, pe) in
  match pe with
  | PEsym sym1 -> 
     annotate (M_PEsym sym1)
  | PEimpl i -> 
     loc_error loc "PEimpl not inlined"
  | PEval v -> 
     annotate (M_PEval (n_val loc v))
  | PEconstrained l -> 
     let l = List.map (fun (c, e) -> (c, n_pexpr loc e)) l in
     annotate (M_PEconstrained l)
  | PEundef(l, u) -> 
     annotate (M_PEundef (l, u))
  | PEerror(err, e') ->
     annotate (M_PEerror (err, n_pexpr loc e'))
  | PEctor(ctor, args) ->
     begin match ctor, args with
     | Core.CivCOMPL, [ct; arg1] -> 
        let ct = fensure_ctype__pexpr loc "CivCOMPL: first argument not a ctype" ct in
        let arg1 = n_pexpr loc arg1 in
        annotate (M_CivCOMPL (ct, arg1))
     | Core.CivCOMPL, _ -> 
        loc_error loc "CivCOMPL applied to wrong number of arguments"
     | Core.CivAND, [ct; arg1; arg2] -> 
        let ct = fensure_ctype__pexpr loc "CivAND: first argument not a ctype" ct in
        let arg1 = n_pexpr loc arg1 in
        let arg2 = n_pexpr loc arg2 in
        annotate (M_CivAND (ct, arg1, arg2))
     | Core.CivAND, _ ->
        loc_error loc "CivAND applied to wrong number of arguments"
     | Core.CivOR, [ct; arg1; arg2] -> 
        let ct = fensure_ctype__pexpr loc "CivOR: first argument not a ctype" ct in
        let arg1 = n_pexpr loc arg1 in
        let arg2 = n_pexpr loc arg2 in
        annotate (M_CivOR (ct, arg1, arg2))
     | Core.CivOR, _ ->
        loc_error loc "CivOR applied to wrong number of arguments"
     | Core.CivXOR, [ct; arg1; arg2] -> 
        let ct = fensure_ctype__pexpr loc "CivXOR: first argument not a ctype" ct in
        let arg1 = n_pexpr loc arg1 in
        let arg2 = n_pexpr loc arg2 in
        annotate (M_CivXOR (ct, arg1, arg2))
     | Core.CivXOR, _ ->
        loc_error loc "CivXOR applied to wrong number of arguments"
     | Core.Cfvfromint, [arg1] -> 
        let arg1 = n_pexpr loc arg1 in
        annotate (M_Cfvfromint arg1)
     | Core.Cfvfromint, _ ->
        loc_error loc "Cfvfromint applied to wrong number of arguments"
     | Core.Civfromfloat, [ct; arg1] -> 
        let ct = fensure_ctype__pexpr loc "Civfromfloat: first argument not a ctype" ct in
        let arg1 = n_pexpr loc arg1 in
        annotate (M_Civfromfloat(ct, arg1))
     | Core.Civfromfloat, _ ->
        loc_error loc "Civfromfloat applied to wrong number of arguments"
     | Core.Cnil bt1, _ -> 
        annotate (M_PEctor (M_Cnil bt1, List.map (n_pexpr loc) args))
     | Core.Ccons, _ ->
        annotate (M_PEctor (M_Ccons, List.map (n_pexpr loc) args))
     | Core.Ctuple, _ -> 
        annotate (M_PEctor (M_Ctuple, List.map (n_pexpr loc) args))
     | Core.Carray, _ -> 
        annotate (M_PEctor (M_Carray, List.map (n_pexpr loc) args))
     | Core.Cspecified, _ -> 
        n_pexpr loc (List.hd args)
     | _ -> 
        loc_error loc ("core_to_mucore: unsupported ctor application")
     end
  | PEcase(e', pats_pes) ->
     loc_error loc "PEcase"
  | PEarray_shift(e', ctype1, e'') ->
     let e' = n_pexpr loc e' in
     let e'' = n_pexpr loc e'' in
     annotate (M_PEarray_shift(e', ctype1, e''))
  | PEmember_shift(e', sym1, id1) ->
     let e' = n_pexpr loc e' in
     annotate (M_PEmember_shift(e', sym1, id1))
  | PEnot e' -> 
     let e' = n_pexpr loc e' in
     annotate (M_PEnot e')
  | PEop(binop1, e', e'') ->
     let e' = n_pexpr loc e' in
     let e'' = n_pexpr loc e'' in
     annotate (M_PEop(binop1, e', e''))
  | PEstruct(sym1, fields) ->
     let fields = List.map (fun (m, e) -> (m, n_pexpr loc e)) fields in
     annotate (M_PEstruct(sym1, fields))
  | PEunion(sym1, id1, e') ->
     let e' = n_pexpr loc e' in
     annotate (M_PEunion(sym1, id1, e'))
  | PEcfunction e' ->
     loc_error loc "core_anormalisation: PEcfunction"
  | PEmemberof(sym1, id1, e') ->
     let e' = n_pexpr loc e' in
     annotate (M_PEmemberof(sym1, id1, e'))
  | PEcall(sym1, args) ->
     begin match sym1, args with
     | Sym (Symbol (_, _, SD_Id "conv_int")), 
       [arg1;arg2] ->
        let ct = (fensure_ctype__pexpr loc "PEcall(conv_int,_): not a ctype" arg1) in
        let arg2 = n_pexpr loc arg2 in
        annotate (M_PEconv_int(ct, arg2))
     | Sym (Symbol (_, _, SD_Id "conv_loaded_int")), 
       [arg1;arg2] ->
        let ct = (fensure_ctype__pexpr loc "PEcall(conv_loaded_int,_): not a ctype" arg1) in
        let arg2 = n_pexpr loc arg2 in
        annotate (M_PEconv_loaded_int(ct, arg2))
     | Sym (Symbol (_, _, SD_Id "wrapI")), 
       [arg1;arg2] ->
        let ct = (fensure_ctype__pexpr loc "PEcall(wrapI,_): not a ctype" arg1) in
        let arg2 = n_pexpr loc arg2 in
        annotate (M_PEwrapI(ct, arg2))
     | Sym (Symbol (_, _, SD_Id "catch_exceptional_condition")),
       [arg1; arg2] ->
        let ct = fensure_ctype__pexpr loc "PEcall(catch_exceptional_condition,_): not a ctype" arg1 in
        let arg2 = n_pexpr loc arg2 in
        annotate (M_PEcatch_exceptional_condition(ct, arg2))
     | Sym (Symbol (_, _, SD_Id "is_representable_integer")),
       [arg1; arg2] ->
        let arg1 = n_pexpr loc arg1 in
        let ct = fensure_ctype__pexpr loc "PEcall(is_representable_integer,_): not a ctype" arg2 in
        annotate (M_PEis_representable_integer(arg1, ct))
     | Sym sym, _ ->
        loc_error loc ("PEcall not inlined: " ^ Pp_symbol.to_string sym)
     | Impl impl, _ ->
        loc_error loc ("PEcall not inlined: " ^ Implementation.string_of_implementation_constant impl)
     end
  | PElet(pat, e', e'') ->
     begin match pat, e' with
     | Pattern (annots, CaseBase (Some sym, _)), 
       Pexpr (annots2, _, PEsym sym2) 
     | Pattern (annots, CaseCtor (Cspecified, [Pattern (_, CaseBase (Some sym, _))])), 
       Pexpr (annots2, _, PEsym sym2) 
       ->
        let e'' = Core_peval.subst_sym_pexpr2 sym 
                    (get_loc annots2, `SYM sym2) e'' in
        n_pexpr loc e''


     | Pattern (annots, CaseCtor (Ctuple, [Pattern (_, CaseBase (Some sym, _));
                                           Pattern (_, CaseBase (Some sym', _))])), 
       Pexpr (annots2, _, PEctor (Ctuple, [Pexpr (_, _, PEsym sym2);
                                           Pexpr (_, _, PEsym sym2')]))
     | Pattern (annots, CaseCtor (Ctuple, [Pattern (_, CaseCtor (Cspecified, [Pattern (_, CaseBase (Some sym, _))]));
                                           Pattern (_, CaseCtor (Cspecified, [Pattern (_, CaseBase (Some sym', _))]))])), 
       Pexpr (annots2, _, PEctor (Ctuple, [Pexpr (_, _, PEsym sym2);
                                           Pexpr (_, _, PEsym sym2')]))
       (* pairwise disjoint *)
       when (SymSet.cardinal (SymSet.of_list [sym; sym'; sym2; sym2']) = 4) ->
        let e'' = Core_peval.subst_sym_pexpr2 sym 
                   (get_loc annots2, `SYM sym2) e'' in
        let e'' = Core_peval.subst_sym_pexpr2 sym' 
                   (get_loc annots2, `SYM sym2') e'' in
        n_pexpr loc e''



     | _ ->
        let pat = core_to_mu__pattern loc pat in
        let e' = n_pexpr loc e' in
        let e'' = n_pexpr loc e'' in
        annotate (M_PElet (M_Pat pat, e', e''))
     end
  | PEif(e1, e2, e3) ->
     begin match e2, e3 with
     | Pexpr (_, _, PEval (Vloaded (LVspecified (OVinteger iv1)))), 
       Pexpr (_, _, PEval (Vloaded (LVspecified (OVinteger iv2))))
          when Option.equal Z.equal (Mem.eval_integer_value iv1) (Some Z.one) &&
               Option.equal Z.equal (Mem.eval_integer_value iv2) (Some Z.zero)
       ->
        let e1 = n_pexpr loc e1 in
        annotate (M_PEbool_to_integer e1)
     (* this should go away *)
     | Pexpr (_, _, PEval Vtrue), Pexpr (_, _, PEval Vfalse) ->
        n_pexpr loc e1
     | _ ->
        let e1 = n_pexpr loc e1 in
        let e2 = n_pexpr loc e2 in
        let e3 = n_pexpr loc e3 in
        annotate (M_PEif (e1, e2, e3))
     end
  | PEis_scalar e' ->
     loc_error loc "core_anormalisation: PEis_scalar"
  | PEis_integer e' ->
     loc_error loc "core_anormalisation: PEis_integer"
  | PEis_signed e' ->
     loc_error loc "core_anormalisation: PEis_signed"
  | PEis_unsigned e' ->
     loc_error loc "core_anormalisation: PEis_unsigned"
  | PEbmc_assume e' ->
     loc_error loc "core_anormalisation: PEbmc_assume"
  | PEare_compatible(e', e'') ->
     loc_error loc "core_anormalisation: PEare_compatible"


(* and n_pexpr_t =
 *   fun loc domain e k ->
 *   n_pexpr_name loc domain e (fun asym -> 
 *   M_TPexpr (loc, [], (), M_PEdone asym)) *)


let normalise_pexpr (loc : Loc.t) (e'' : unit pexpr) = n_pexpr loc e''



let n_kill_kind = function
  | Core.Dynamic -> M_Dynamic
  | Core.Static0 ct1 -> M_Static ct1


let n_action loc (action : ('a, unit) action1) =
  let (Action (loc', _, a1)) = action in
  let loc = update_loc loc loc' in
  let wrap a1 = M_Action(loc, a1) in
  match a1 with
  | Create(e1, e2, sym1) ->
     let ctype1 = (fensure_ctype__pexpr loc "Create: not a ctype" e2) in
     let e1 = n_pexpr loc e1 in
     wrap (M_Create(e1, ctype1, sym1))
  | CreateReadOnly(e1, e2, e3, sym1) ->
     let ctype1 = (fensure_ctype__pexpr loc "CreateReadOnly: not a ctype" e1) in
     let e1 = n_pexpr loc e1 in
     let e3 = n_pexpr loc e3 in
     wrap (M_CreateReadOnly(e1, ctype1, e3, sym1))
  | Alloc0(e1, e2, sym1) ->
     let e1 = n_pexpr loc e1 in
     let e2 = n_pexpr loc e2 in
     wrap (M_Alloc(e1, e2, sym1))
  | Kill(kind, e1) ->
     let e1 = n_pexpr loc e1 in
     wrap (M_Kill((n_kill_kind kind), e1))
  | Store0(b, e1, e2, e3, mo1) ->
     let ctype1 = (fensure_ctype__pexpr loc "Store: not a ctype" e1) in
     let e2 = n_pexpr loc e2 in
     let e3 = n_pexpr loc e3 in
     wrap (M_Store(b, ctype1, e2, e3, mo1))
  | Load0(e1, e2, mo1) ->
     let ctype1 = (fensure_ctype__pexpr loc "Load: not a ctype" e1) in
     let e2 = n_pexpr loc e2 in
     wrap (M_Load(ctype1, e2, mo1))
  | SeqRMW (b, e1, e2, sym, e3) ->
      failwith "TODO: SeqRMW"
(*
     let ctype1 = (fensure_ctype__pexpr loc "SeqRMW: not a ctype" e1) in
     n_pexpr_in_expr_name e2 (fun e2 ->
     n_pexpr_in_expr_name e3 (fun e3 ->
     k (wrap (M_SeqRMW(ctype1, e2, sym, e3)))))
*)
  | RMW0(e1, e2, e3, e4, mo1, mo2) ->
     let ctype1 = (fensure_ctype__pexpr loc "RMW: not a ctype" e1) in
     let e2 = n_pexpr loc e2 in
     let e3 = n_pexpr loc e3 in
     let e4 = n_pexpr loc e4 in
     wrap (M_RMW(ctype1, e2, e3, e4, mo1, mo2))
  | Fence0 mo1 -> 
     wrap (M_Fence mo1)
  | CompareExchangeStrong(e1, e2, e3, e4, mo1, mo2) ->
     let ctype1 = (fensure_ctype__pexpr loc "CompareExchangeStrong: not a ctype" e1) in
     let e2 = n_pexpr loc e2 in
     let e3 = n_pexpr loc e3 in
     let e4 = n_pexpr loc e4 in
     wrap (M_CompareExchangeStrong(ctype1, e2, e3, e4, mo1, mo2))
  | CompareExchangeWeak(e1, e2, e3, e4, mo1, mo2) ->
     let ctype1 = (fensure_ctype__pexpr loc "CompareExchangeWeak: not a ctype" e1) in
     let e2 = n_pexpr loc e2 in
     let e3 = n_pexpr loc e3 in
     let e4 = n_pexpr loc e4 in
     wrap (M_CompareExchangeWeak(ctype1, e2, e3, e4, mo1, mo2))
  | LinuxFence lmo ->
     wrap (M_LinuxFence lmo)
  | LinuxLoad(e1, e2, lmo) ->
     let ctype1 = (fensure_ctype__pexpr loc "LinuxLoad: not a ctype" e1) in
     let e2 = n_pexpr loc e2 in
     wrap (M_LinuxLoad(ctype1, e2, lmo))
  | LinuxStore(e1, e2, e3, lmo) ->
     let ctype1 = (fensure_ctype__pexpr loc "LinuxStore: not a ctype" e1) in
     let e2 = n_pexpr loc e2 in
     let e3 = n_pexpr loc e3 in
     wrap (M_LinuxStore(ctype1, e2, e3, lmo))
  | LinuxRMW(e1, e2, e3, lmo) ->
     let ctype1 = (fensure_ctype__pexpr loc "LinuxRMW: not a ctype" e1) in
     let e2 = n_pexpr loc e2 in
     let e3 = n_pexpr loc e3 in
     wrap (M_LinuxRMW(ctype1, e2, e3, lmo))

     

let n_paction loc (Paction(pol, a)) = 
  M_Paction (pol, n_action loc a)





let show_n_memop = 
  Mem_common.instance_Show_Show_Mem_common_memop_dict.show_method

let n_memop loc memop pexprs =
  match (memop, pexprs) with
  | (Mem_common.PtrEq, [pe1;pe2]) ->
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     M_PtrEq (pe1, pe2)
  | (Mem_common.PtrNe, [pe1;pe2]) ->
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     M_PtrNe (pe1, pe2)
  | (Mem_common.PtrLt, [pe1;pe2]) ->
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     M_PtrLt (pe1, pe2)
  | (Mem_common.PtrGt, [pe1;pe2]) ->
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     M_PtrGt (pe1, pe2)
  | (Mem_common.PtrLe, [pe1;pe2]) ->
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     M_PtrLe (pe1, pe2)
  | (Mem_common.PtrGe, [pe1;pe2]) ->
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     M_PtrGe (pe1, pe2)
  | (Mem_common.Ptrdiff, [ct1;pe1;pe2]) ->
     let ct1 = (fensure_ctype__pexpr loc "Ptrdiff: not a ctype" ct1) in
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     M_Ptrdiff (ct1, pe1, pe2)
  | (Mem_common.IntFromPtr, [ct1;ct2;pe]) ->
     let ct1 = (fensure_ctype__pexpr loc "IntFromPtr: not a ctype" ct1) in
     let ct2 = (fensure_ctype__pexpr loc "IntFromPtr: not a ctype" ct2) in
     let pe = n_pexpr loc pe in
     M_IntFromPtr (ct1, ct2, pe)
  | (Mem_common.PtrFromInt, [ct1;ct2;pe]) ->
     let ct1 = (fensure_ctype__pexpr loc "PtrFromInt: not a ctype" ct1) in
     let ct2 = (fensure_ctype__pexpr loc "PtrFromInt: not a ctype" ct2) in
     let pe = n_pexpr loc pe in
     M_PtrFromInt (ct1, ct2, pe)
  | (Mem_common.PtrValidForDeref, [ct1;pe]) ->
     let ct1 = (fensure_ctype__pexpr loc "PtrValidForDeref: not a ctype" ct1) in
     let pe = n_pexpr loc pe in
     M_PtrValidForDeref (ct1, pe)
  | (Mem_common.PtrWellAligned, [ct1;pe]) ->
     let ct1 = (fensure_ctype__pexpr loc "PtrWellAligned: not a ctype" ct1) in
     let pe = n_pexpr loc pe in
     M_PtrWellAligned (ct1, pe)
  | (Mem_common.PtrArrayShift, [pe1;ct1;pe2]) ->
     let ct1 = (fensure_ctype__pexpr loc "PtrArrayShift: not a ctype" ct1) in
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     M_PtrArrayShift (pe1 ,ct1, pe2)
  | (Mem_common.Memcpy, [pe1;pe2;pe3]) ->
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     let pe3 = n_pexpr loc pe3 in
     M_Memcpy (pe1 ,pe2, pe3)
  | (Mem_common.Memcmp, [pe1;pe2;pe3]) ->
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     let pe3 = n_pexpr loc pe3 in
     M_Memcmp (pe1 ,pe2, pe3)
  | (Mem_common.Realloc, [pe1;pe2;pe3]) ->
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     let pe3 = n_pexpr loc pe3 in
     M_Realloc (pe1 ,pe2, pe3)
  | (Mem_common.Va_start, [pe1;pe2]) ->
     let pe1 = n_pexpr loc pe1 in
     let pe2 = n_pexpr loc pe2 in
     M_Va_start (pe1 ,pe2)
  | (Mem_common.Va_copy, [pe]) ->
     let pe = n_pexpr loc pe in
     M_Va_copy pe
  | (Mem_common.Va_arg, [pe;ct1]) ->
     let ct1 = (fensure_ctype__pexpr loc "Va_arg: not a ctype" ct1) in
     let pe = n_pexpr loc pe in
     M_Va_arg (pe ,ct1)
  | (Mem_common.Va_end, [pe]) ->
     let pe = n_pexpr loc pe in
     M_Va_end pe
  | (memop, pexprs1) ->
     let err = 
       show_n_memop memop ^ 
         " applied to " ^ 
           string_of_int (List.length pexprs1) ^ 
             " arguments"
     in
     error err



let rec n_expr (loc : Loc.t) (returns : symbol Pset.set)
          (e : ('a, unit) expr) : mu_expr = 
  (* print_endline ("\n\n\n*******************************************************\nnormalising ");
   * PPrint.ToChannel.compact stdout (Pp_core_ast.pp_expr e);
   * print_endline "\n";
   * flush stdout; *)
  let (Expr (annots, pe)) = e in
  let loc = update_loc loc (get_loc_ annots) in
  let wrap pe = M_Expr (loc, annots, pe) in
  let n_pexpr = n_pexpr loc in
  let n_paction = (n_paction loc) in
  let n_memop = (n_memop loc) in
  let n_expr = (n_expr loc returns) in
  match pe with
  | Epure pexpr2 -> 
     wrap (M_Epure (n_pexpr pexpr2))
  | Ememop(memop1, pexprs1) -> 
     wrap (M_Ememop (n_memop memop1 pexprs1))
  | Eaction paction2 ->
     wrap (M_Eaction (n_paction paction2))
  | Ecase(pexpr, pats_es) ->
     failwith "Ecase"
     (* let pexpr = n_pexpr pexpr in *)
     (* let pats_es =  *)
     (*   (map (fun (pat,e) ->  *)
     (*       let pat = core_to_mu__pattern loc pat in *)
     (*       let pe = (n_expr e k) in *)
     (*       (pat, pe) *)
     (*    )  *)
     (*     pats_es)  *)
     (* in *)
     (* twrap (M_Ecase(pexpr, pats_es)) *)
  | Elet(pat, e1, e2) ->
     begin match pat, e1 with
     | Pattern (annots, CaseBase (Some sym, _)),
       Pexpr (annots2, _, PEsym sym2) 
     | Pattern (annots, CaseCtor (Cspecified, [Pattern (_, CaseBase (Some sym, _))])), 
       Pexpr (annots2, _, PEsym sym2) 
       ->
        let e2 = Core_peval.subst_sym_expr2 sym 
                   (get_loc annots2, `SYM sym2) e2 in
        n_expr e2
     | Pattern (annots, CaseCtor (Ctuple, [Pattern (_, CaseBase (Some sym, _));
                                           Pattern (_, CaseBase (Some sym', _))])), 
       Pexpr (annots2, _, PEctor (Ctuple, [Pexpr (_, _, PEsym sym2);
                                           Pexpr (_, _, PEsym sym2')]))
     | Pattern (annots, CaseCtor (Ctuple, [Pattern (_, CaseCtor (Cspecified, [Pattern (_, CaseBase (Some sym, _))]));
                                           Pattern (_, CaseCtor (Cspecified, [Pattern (_, CaseBase (Some sym', _))]))])), 
       Pexpr (annots2, _, PEctor (Ctuple, [Pexpr (_, _, PEsym sym2);
                                           Pexpr (_, _, PEsym sym2')]))
       (* pairwise disjoint *)
       when (SymSet.cardinal (SymSet.of_list [sym; sym'; sym2; sym2']) = 4) ->
        let e2 = Core_peval.subst_sym_expr2 sym 
                   (get_loc annots2, `SYM sym2) e2 in
        let e2 = Core_peval.subst_sym_expr2 sym' 
                   (get_loc annots2, `SYM sym2') e2 in
        n_expr e2

     | _ ->
        let e1 = n_pexpr e1 in
        let pat = core_to_mu__pattern loc pat in
        let e2 = n_expr e2 in
        wrap (M_Elet(M_Pat pat, e1, e2))
     end
  | Eif(e1, e2, e3) ->
     begin match e2, e3 with
     | Expr (_, Epure (Pexpr (_, _, PEval (Vloaded (LVspecified (OVinteger iv1)))))), 
       Expr (_, Epure (Pexpr (_, _, PEval (Vloaded (LVspecified (OVinteger iv2))))))
          when Option.equal Z.equal (Mem.eval_integer_value iv1) (Some Z.one) &&
                 Option.equal Z.equal (Mem.eval_integer_value iv2) (Some Z.zero)
       ->
        let e1 = n_pexpr e1 in
        wrap (M_Epure (M_Pexpr (loc, [], (), M_PEbool_to_integer e1)))
     | Expr (_, Epure (Pexpr (_, _, PEval Vtrue))), 
       Expr (_, Epure (Pexpr (_, _, PEval Vfalse))) ->
        let e1 = n_pexpr e1 in
        wrap (M_Epure e1)
     | _ ->
        let e1 = n_pexpr e1 in
        let e2 = n_expr e2 in
        let e3 = n_expr e3 in
        wrap (M_Eif(e1, e2, e3))
     end
  | Eccall(_a, ct1, e2, es) ->
     let ct1 = match ct1 with
       | Core.Pexpr(annots, bty, Core.PEval (Core.Vctype ct1)) -> 
          let loc = update_loc loc (get_loc_ annots) in
          act_pack loc annots bty ct1
       | _ -> loc_error loc "core_anormalisation: Eccall with non-ctype first argument"
     in
     let e2 = 
       let err () = 
         error "core_anormalisation: Eccall where function is not statically known" in
       match e2 with
       | Core.Pexpr(annots, bty, Core.PEval v) ->
          begin match v with
          | Vobject (OVpointer ptrval)
          | Vloaded (LVspecified (OVpointer ptrval)) ->
             Impl_mem.case_ptrval ptrval
               ( fun ct -> err ())
               ( fun sym -> M_Pexpr (loc, annots, bty, (M_PEsym sym)) )
               ( fun _prov _ -> err () )
          | _ -> err ()
          end
       | _ -> err ()
     in
     let es = List.map n_pexpr es in
     wrap (M_Eccall(ct1, e2, es))
  | Eproc(_a, name1, es) ->
     failwith "Eproc"
     (* n_pexpr_in_expr_names es (fun es -> *)
     (* k (wrap (M_Eproc(name1, es)))) *)
  | Eunseq es ->
     let es = List.map n_expr es in
     wrap (M_Eunseq es)
  | Ewseq(pat, e1, e2) ->
     let e1 = n_expr e1 in
     let pat = core_to_mu__pattern loc pat in
     let e2 = n_expr e2 in
     wrap (M_Ewseq(pat, e1, e2))
  | Esseq(pat, e1, e2) ->
     let e1 = n_expr e1 in
     let pat = core_to_mu__pattern loc pat in
     let e2 = n_expr e2 in
     wrap (M_Esseq(pat, e1, e2))
  | Ebound e ->
     wrap (M_Ebound (n_expr e))
  | End es ->
     let es = List.map n_expr es in
     wrap (M_End es)
  | Esave((sym1,bt1), syms_typs_pes, e) ->  
     error "core_anormalisation: Esave"
  (* DISCARDS CONTINUATION *)
  | Erun(_a, sym1, pes) ->
     let pes = List.map n_pexpr pes in
     (* begin match pes, Pset.mem sym1 returns with *)
     (* | [e], true -> *)
     (*    letbind_pexpr_ (Symbol.fresh_description Symbol.SD_Return)  *)
     (*      expr_n_pexpr_domain e (fun e -> *)
     (*        twrap (M_Erun(sym1, [e]))) *)
     (* | _ -> *)
        wrap (M_Erun(sym1, pes))
     (* end *)
  | Epar es -> 
     error "core_anormalisation: Epar"
  | Ewait tid1 ->
     error "core_anormalisation: Ewait"
  | Epack(id, pes) ->
     let pes = List.map n_pexpr pes in
     wrap (M_Erpredicate(Pack, id, pes))
  | Eunpack(id, pes) ->
     let pes = List.map n_pexpr pes in
     wrap (M_Erpredicate(Unpack, id, pes))
  | Ehave(id, pes) ->
     let pes = List.map n_pexpr pes in
     wrap (M_Elpredicate(Have, id, pes))
  | Eshow(id, pes) ->
     let pes = List.map n_pexpr pes in
     wrap (M_Elpredicate(Show, id, pes))
  | Einstantiate (id, pe) ->
     let pe = n_pexpr pe in
     wrap (M_Einstantiate (id, pe))
  | Eannot _ ->
      failwith "core_anormalisation: Eannot"
  | Eexcluded _ ->
      failwith "core_anormalisation: Eexcluded"

let normalise_expr (loc : Loc.t) (returns : symbol Pset.set) e =
  n_expr loc returns e


(* let normalise_impl_decl (i : unit generic_impl_decl) : unit mu_impl_decl = *)
(*   match i with *)
(*   | Def(bt, p) ->  *)
(*      let ict = bt in *)
(*      M_Def (ict, bt, normalise_pexpr Loc.unknown p) *)
(*   | IFun(bt, args, body) ->  *)
(*      let ift = (bt, map snd args) in *)
(*      M_IFun (ift, bt, args, normalise_pexpr Loc.unknown body) *)

(* let normalise_impl (i : unit generic_impl) : unit mu_impl= *)
(*    (Pmap.map normalise_impl_decl i) *)

let normalise_fun_map_decl (name1: symbol) d 
    : unit mu_fun_map_decl=
  match d with
  | Mi_Fun (bt, args, pe) -> 
     M_Fun(bt, args, normalise_pexpr Loc.unknown pe)
  | Mi_Proc (loc, bt, args, e, labels) -> 
     let returns = 
       Pmap.fold (fun sym label returns ->
           match label with
           | Mi_Return _ -> Pset.add sym returns
           | _ -> returns
         ) labels (Pset.empty Symbol.symbol_compare)
     in
     let labels' = 
       Pmap.map (function
           | Mi_Return loc (* (loc, lt) *) -> 
              M_Return loc (* (loc, lt) *)
           | Mi_Label (loc, lt, args, e, annots) ->
              let e = normalise_expr loc returns e in
              M_Label (loc, lt, args, (), e, annots)
         ) labels
     in
     M_Proc(loc, bt, (args, ()), normalise_expr loc returns e, labels')
  | Mi_ProcDecl(loc, bt, bts) -> M_ProcDecl(loc, bt, bts)
  | Mi_BuiltinDecl(loc, bt, bts) -> M_BuiltinDecl(loc, bt, bts)

let normalise_fun_map fmap : unit mu_fun_map= 
  (Pmap.mapi normalise_fun_map_decl fmap)
  



let normalise_globs sym (g : ('a, unit) generic_globs) : unit mu_globs = 

  let fresh_relative (s : symbol) (f : string -> string) : symbol =
    match Symbol.symbol_description s with
    | SD_ObjectAddress name -> 
       Symbol.fresh_object_address name
    | _ -> Symbol.fresh ()
  in

  match g with
  | GlobalDef((bt, ct), e) -> 
     M_GlobalDef (fresh_relative sym (fun s -> s^"_l"), (bt, ct), 
                  normalise_expr Loc.unknown (Pset.empty Symbol.symbol_compare) e)
  | GlobalDecl (bt, ct) -> 
     M_GlobalDecl (fresh_relative sym (fun s -> s^"_l"), (bt, ct))


let normalise_globs_list (gs : (Symbol.sym * ('a, unit) generic_globs) list)
    : (Symbol.sym * unit mu_globs) list= 
   (map (fun (sym1,g) -> (sym1, normalise_globs sym1 g)) gs)



let normalise_tag_definition = function
  | StructDef(l, mf) -> M_StructDef (l, mf)
  | UnionDef l -> M_UnionDef l


let normalise_tag_definitions tagDefs =
   (Pmap.map normalise_tag_definition tagDefs)

let normalise_funinfo (loc,annots2,ret,args,b1,b2) = 
  let args = 
    map (fun (osym, ct) -> 
        match osym with 
        | Some sym -> (sym, ct)
        | None -> (Symbol.fresh (), ct)
      ) args 
  in
  M_funinfo (loc, annots2, (ret,args,b1), Checked, b2)

let normalise_funinfos funinfos =
   (Pmap.map normalise_funinfo funinfos)


let rec ctype_contains_function_pointer (Ctype.Ctype (_, ct_)) = 
  match ct_ with
  | Void -> false
  | Basic _ -> false
  | Array (ct, _) -> ctype_contains_function_pointer ct
  | Function _
  | FunctionNoParams _ -> true
  | Pointer (_, ct) -> ctype_contains_function_pointer ct
  | Atomic ct -> ctype_contains_function_pointer ct
  | Struct _ -> false
  | Union _ -> false


let check_supported (file : ('a, 'TY) mi_file) =
  let _ = 
    Pmap.iter (fun _sym def -> 
        let (loc, _attrs, ret_ctype, args,  variadic, _) = def in
        if ctype_contains_function_pointer ret_ctype ||
             List.exists (fun (_,ct) -> ctype_contains_function_pointer ret_ctype) args
        then 
          let err = Errors.UNSUPPORTED "function pointers" in
          Pp_errors.fatal (Pp_errors.to_string (loc, err)); 
        else if variadic then
          let err = Errors.UNSUPPORTED "variadic functions" in
          Pp_errors.fatal (Pp_errors.to_string (loc, err)); 
        else
          ()
      ) file.mi_funinfo
  in
  (* let _ = 
   *   Pmap.iter (fun _sym def -> 
   *       match def with
   *       | Ctype.StructDef (members, flexible_array_members) ->
   *          if List.exists (fun (_,(_,_,ct)) -> ctype_contains_function_pointer ct) members 
   *          then 
   *            let err = Errors.UNSUPPORTED "function pointers" in
   *            Pp_errors.fatal (Pp_errors.to_string (Loc.unknown, err)); 
   *          else if flexible_array_members <> None then
   *            let err = Errors.UNSUPPORTED "function pointers" in
   *            Pp_errors.fatal (Pp_errors.to_string (Loc.unknown, err)); 
   *          else ()
   *       | Ctype.UnionDef members ->
   *          if List.exists (fun (_,(_,_,ct)) -> ctype_contains_function_pointer ct) members 
   *          then 
   *            let err = Errors.UNSUPPORTED "function pointers" in
   *            Pp_errors.fatal (Pp_errors.to_string (Loc.unknown, err)); 
   *          else ()
   *     ) file.tagDefs
   * in *)
  ()

let normalise_file (file : ('a, 'TY) Milicore.mi_file) : unit Mu.mu_file = 
  check_supported file;
   ({ mu_main = (file.mi_main)
   ; mu_tagDefs = (normalise_tag_definitions file.mi_tagDefs)
   (* ; mu_stdlib = (normalise_fun_map file.mi_stdlib) *)
   (* ; mu_impl = (normalise_impl file.mi_impl) *)
   ; mu_globs = (normalise_globs_list file.mi_globs)
   ; mu_funs = (normalise_fun_map file.mi_funs)
   ; mu_extern = (file.mi_extern)
   ; mu_funinfo = (normalise_funinfos file.mi_funinfo)
   ; mu_loop_attributes = file.mi_loop_attributes
   ; mu_resource_predicates = ()
   ; mu_logical_predicates = ()
  })


end
