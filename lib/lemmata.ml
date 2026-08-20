module BT = BaseTypes
module AT = ArgumentTypes
module Loc = Locations
module StringSet = Set.Make (String)
module CI = Coq_ir
module CC = Cn_to_coq

let ret_sym = "ν"

(* Printing headers for each module in the ITP file *)

let parse_directions directions = (directions, StringSet.singleton "all")

let header filename =
  let open Pp in
  !^"(*"
  ^^^ !^filename
  ^^ !^": generated lemma specifications from CN *)"
  ^^ hardline
  ^^ hardline
  ^^ !^"Require Import ZArith Bool."
  ^^ hardline
  ^^ !^"Require CN_Lemmas.CN_Lib."
  ^^ hardline
  ^^ !^"Require Import CN_Lemmas.CN_Lib_Iris."
  ^^ hardline
  ^^ !^"From iris.bi.lib Require Import fixpoint_mono."
  ^^ hardline
  ^^ !^"From iris.proofmode Require Import proofmode."
  ^^ hardline
  ^^ !^"Require Import CN_Lemmas.CN_Lib_Iris_Fixpoint."
  ^^ hardline
  ^^ hardline


let open_iris_mode iris_defs section_name =
  let open Pp in
  !^"  (* Opening Iris mode *)"
  ^^ hardline
  ^^ !^"  Section "
  ^^ !^section_name
  ^^ !^"."
  ^^ hardline
  ^^ !^"  Context `{!heapGS_gen Σ}."
  ^^ hardline
  ^^ hardline
  ^^ flow hardline iris_defs
  ^^ hardline
  ^^ hardline
  ^^ !^"  (* Closing Iris mode *)"
  ^^ hardline
  ^^ !^"  End "
  ^^ !^section_name
  ^^ !^"."


let types_spec types =
  let open Pp in
  !^"Module Types."
  ^^ hardline
  ^^ hardline
  ^^ (if List.length types == 0 then
        !^"  (* no type definitions required *)" ^^ hardline
      else
        flow hardline types)
  ^^ hardline
  ^^ !^"End Types."
  ^^ hardline
  ^^ hardline


let defs_module defs =
  let open Pp in
  !^"Module Defs (P : Parameters)."
  ^^ hardline
  ^^ !^"  (* Definitions of functions, structs, and struct ownership predicates *)"
  ^^ hardline
  ^^ !^"  Import Types P."
  ^^ hardline
  ^^ !^"  Open Scope Z."
  ^^ hardline
  ^^ hardline
  ^^ open_iris_mode defs "Defs"
  ^^ hardline
  ^^ hardline
  ^^ !^"End Defs."
  ^^ hardline
  ^^ hardline


let lemmas_module aux_defs lemma_tys =
  let open Pp in
  !^"Module Lemma_Defs (P : Parameters)."
  ^^ hardline
  ^^ !^"  Module D := Defs(P)."
  ^^ hardline
  ^^ !^"  Module R := ResourcePredicates(P)."
  ^^ hardline
  ^^ !^"  Import Types D P R."
  ^^ hardline
  ^^ !^"  Open Scope Z."
  ^^ hardline
  ^^ hardline
  ^^ flow hardline aux_defs
  ^^ hardline
  ^^ hardline
  ^^ open_iris_mode lemma_tys "Iris_Type_Defs"
  ^^ hardline
  ^^ !^"End Lemma_Defs."
  ^^ hardline
  ^^ hardline


let mod_spec lemma_nms =
  let open Pp in
  let lemma nm =
    !^"  Parameter"
    ^^^ typ (Sym.pp nm) (!^"⊢ " ^^ Sym.pp nm ^^ !^"_type")
    ^^ !^"."
    ^^ hardline
  in
  !^"Module Type Lemma_Spec (P : Parameters)."
  ^^ hardline
  ^^ hardline
  ^^ !^"  Module L := Lemma_Defs(P)."
  ^^ hardline
  ^^ !^"  Import L."
  ^^ hardline
  ^^ open_iris_mode
       ((!^"  Local Notation \"⊢ P\" := (⊢@{iPropI Σ} P)." ^^ hardline)
        :: List.map lemma lemma_nms)
       "Lemma_Defs"
  ^^ hardline
  ^^ !^"End Lemma_Spec."
  ^^ hardline
  ^^ hardline


let pred_spec preds =
  let open Pp in
  !^"Module ResourcePredicates (P : Parameters)."
  ^^ hardline
  ^^ !^"  Module D := Defs(P)."
  ^^ hardline
  ^^ !^"  Import Types P D."
  ^^ hardline
  ^^ !^"  Open Scope Z."
  ^^ hardline
  ^^ (if List.length preds == 0 then
        !^"  (* no resource predicates required *)" ^^ hardline
      else
        open_iris_mode preds "Iris_Pred_Defs")
  ^^ hardline
  ^^ !^"End ResourcePredicates."
  ^^ hardline
  ^^ hardline


let param_spec params =
  let open Pp in
  !^"Module Type Parameters."
  ^^ hardline
  ^^ !^"  Import Types."
  ^^ hardline
  ^^ !^"  Open Scope Z."
  ^^ hardline
  ^^ (if List.length params == 0 then
        !^"  (* no parameters required *)" ^^ hardline
      else
        flow hardline params)
  ^^ hardline
  ^^ !^"End Parameters."
  ^^ hardline
  ^^ hardline


(* Convenient printing functions *)

let fail msg details =
  let open Pp in
  print stdout (format [ Bold; Red ] msg ^^ colon ^^ space ^^ details);
  failwith msg


let build = function
  | [] -> fail "build" (Pp.string "empty")
  | xs ->
    let docs = List.map (fun x -> x) xs in
    Pp.flow (Pp.break 1) docs


let parensM x = Pp.parens x

let rets s = Pp.string s

let iris_pure x = build [ rets "⌜"; x; rets "⌝" ]

let rec intersperse (sep : string) (last : string) xs =
  let open Pp in
  match xs with
  | [] -> !^""
  | x :: [] -> x ^^ !^last
  | x :: xs -> x ^^ !^sep ^^ intersperse sep last xs


let print_ctype (ctyp : Sctypes.t) =
  match ctyp with
  | Void -> "unsupported ctype void"
  | Integer _ -> "Z"
  | Array _ -> "unsupported ctype array"
  | Pointer _ -> "Ptr"
  | Struct s -> Sym.pp_string s
  | Function _ -> "unsupported ctype function"
  | Byte -> "unsupported ctype function" (* TODO(HK): added for plumbing *)


let enc_z z =
  if Z.leq Z.zero z then
    rets (Z.to_string z)
  else
    parensM (rets (Z.to_string z))


let f_appM nm xs = parensM (build (rets nm :: xs))

let defn nm args opt_ty rhs fix =
  let open Pp in
  let if_fix = if fix then !^"  Fixpoint" else !^"  Definition" in
  let tyeq = match opt_ty with None -> [] | Some ty -> [ colon; ty ] in
  flow (break 1) ([ if_fix; !^nm ] @ args @ tyeq @ [ !^":=" ])
  ^^ hardline
  ^^ !^"    "
  ^^ rhs
  ^^ !^"."
  ^^ hardline


let binop s x y =
  let open Pp in
  parens (flow (break 1) [ x; !^s; y ])


let tuple_itp_ty doc fld_tys =
  let open Pp in
  let rec stars = function
    | [] -> fail "tuple_itp_ty: empty" doc
    | [ x ] -> [ x ]
    | x :: xs -> x :: star :: stars xs
  in
  parens (flow (break 1) (stars fld_tys))


(* Getter for tuples given an index and its dimensions *)
let gen_get_upd ((i, list_len) : int * int) (tm : PPrint.document) =
  let open Pp in
  let pp_fst a = parens (build [ rets "fst"; a ]) in
  let pp_snd a = parens (build [ rets "snd"; a ]) in
  let rec foldi i f acc = if i <= 0 then acc else foldi (pred i) f (f acc) in
  if i = 0 then
    foldi (list_len - 1) pp_fst tm
  else
    pp_snd (foldi (list_len - 1 - i) pp_fst tm)


(* CN BaseTypes to ITP *)
let rec bt_to_itp (bt : CI.itp_bt) =
  let open Pp in
  match bt with
  | CI.ITP_Bool -> !^"bool"
  | CI.ITP_Integer -> !^"Z"
  | CI.ITP_Bits _ -> !^"Z"
  | CI.ITP_Map (x, y) ->
    let enc_x = bt_to_itp x in
    let enc_y = bt_to_itp y in
    parens (binop "->" enc_x enc_y)
  | CI.ITP_Struct (CI.ITP_sym tag, _) -> Sym.pp tag
  | CI.ITP_Record mems ->
    let enc_mem_bts = List.map bt_to_itp mems in
    tuple_itp_ty !^"record" enc_mem_bts
  | CI.ITP_Loc -> !^"Ptr"
  | CI.ITP_Datatype (CI.ITP_sym tag) -> Sym.pp tag
  | CI.ITP_List _bt2 -> !^"list " ^^ bt_to_itp _bt2
  | CI.ITP_Unit -> !^"unsupported BT unit"
  | CI.ITP_Membyte -> !^"unsupported BT membyte"
  | CI.ITP_Real -> !^"unsupported BT real"
  | CI.ITP_Alloc_id -> !^"unsupported BT alloc_id"
  | CI.ITP_CType -> !^"unsupported BT ctype"
  | CI.ITP_Tuple fld_bts ->
    let enc_fld_bts = List.map bt_to_itp fld_bts in
    tuple_itp_ty !^"" enc_fld_bts
  | CI.ITP_Set _bt2 -> rets "unsupported BT set"


(* Let and forall occur in both pure and resource terms*)
let pp_let sym rhs_doc doc =
  let open Pp in
  !^"let" ^^^ Sym.pp sym ^^^ !^":=" ^^^ rhs_doc ^^^ !^"in" ^^^ doc


let pp_forall (sym : Sym.t) (bt : CI.itp_bt) (doc : Pp.document) =
  let open Pp in
  !^"∀" ^^^ parens (typ (Sym.pp sym) (bt_to_itp bt)) ^^ !^"," ^^ break 1 ^^ doc


let norm_bv_op bt doc_f =
  match bt with
  | CI.ITP_Bits (sign, sz) ->
    (match sign with
     | CI.ITP_Unsigned ->
       let minInt, maxInt = BT.bits_range (Unsigned, sz) in
       f_appM "CN_Lib.wrapI" [ enc_z minInt; enc_z maxInt; doc_f ]
     | CI.ITP_Signed ->
       let minInt, maxInt = BT.bits_range (Signed, sz) in
       f_appM "CN_Lib.wrapI" [ enc_z minInt; enc_z maxInt; doc_f ])
  | _ -> doc_f


let rec pat_to_itp (pat : CI.itp_pat) =
  match pat with
  | ITP_pSym (ITP_sym sym) -> Sym.pp sym
  | ITP_pWild -> rets "_"
  | ITP_pConstructor (ITP_sym s, l) ->
    parensM (build ([ Sym.pp s ] @ List.map pat_to_itp l))


let term_to_itp (global : Global.t) (t : CI.itp_pure_term) =
  let open Pp in
  let rec f (global : Global.t) t =
    let aux t = f global t in
    let abinop s x y = parensM (build [ aux x; rets s; aux y ]) in
    match t with
    | CI.ITP_sym_term (CI.ITP_sym s) -> Sym.pp s
    | ITP_const c ->
      (match c with
       | ITP_bool b -> rets (if b then "true" else "false")
       | ITP_bool_prop b -> f_appM "Is_true" [ rets (if b then "true" else "false") ]
       | ITP_Z z -> enc_z z
       | ITP_bits z -> parensM (rets (Z.to_string z)))
    | ITP_unop (op, x, bt) ->
      norm_bv_op
        bt
        (match op with
         | CI.ITP_neg -> f_appM "negb" [ aux x ]
         | CI.ITP_neg_prop -> f_appM "~" [ aux x ]
         | CI.ITP_BW_FFS -> f_appM "CN_Lib.find_first_set_z" [ aux x ]
         | CI.ITP_BW_CTZ -> f_appM "CN_Lib.count_trailing_zeroes_z" [ aux x ])
    | CI.ITP_binop (op, x, y, bt) ->
      norm_bv_op
        bt
        (match op with
         | CI.ITP_add -> abinop "+" x y
         | CI.ITP_sub -> abinop "-" x y
         | CI.ITP_mul -> abinop "*" x y
         | CI.ITP_div -> abinop "/" x y
         | CI.ITP_mod -> abinop "mod" x y
         (* todo: rem is definitely not right *)
         | CI.ITP_rem -> abinop "mod" x y
         | CI.ITP_lt -> abinop "<?" x y
         | CI.ITP_lt_prop -> abinop "<" x y
         | CI.ITP_le -> abinop "<=?" x y
         | CI.ITP_le_prop -> abinop "<=" x y
         | CI.ITP_exp -> abinop "^" x y
         | CI.ITP_bwxor -> f_appM "Z.lxor" [ aux x; aux y ]
         | CI.ITP_bwand -> f_appM "Z.land" [ aux x; aux y ]
         | CI.ITP_bwor -> f_appM "Z.lor" [ aux x; aux y ]
         | CI.ITP_eq -> parensM (build [ aux x; rets "=?"; aux y ])
         | CI.ITP_eq_prop -> parensM (build [ aux x; rets "="; aux y ])
         | CI.ITP_and -> abinop "&&" x y
         | CI.ITP_and_prop -> abinop "∧" x y
         | CI.ITP_or -> abinop "||" x y
         | CI.ITP_or_prop -> abinop "∨" x y
         | CI.ITP_impl -> abinop "implb" x y
         | CI.ITP_impl_prop -> abinop "-∗" x y)
    | CI.ITP_match (x, cases) ->
      let br (pat, rhs) = build [ rets "|"; pat_to_itp pat; rets "=>"; aux rhs ] in
      parensM
        (build
           ([ rets "match"; aux x; rets "with"; hardline ]
            @ List.map br cases
            @ [ rets "end" ]))
    | CI.ITP_ite (sw, x, y) ->
      parensM (build [ rets "if"; aux sw; rets "then"; aux x; rets "else"; aux y ])
    | CI.ITP_eachI ((i1, (CI.ITP_sym s, _), i2), x) ->
      let enc =
        pp_forall
          s
          CI.ITP_Integer
          (binop
             "->"
             (binop
                "/\\"
                (binop "<=" (Pp.int i1) (Sym.pp s))
                (binop "<=" (Sym.pp s) (Pp.int i2)))
             (aux x))
      in
      parens enc
    | CI.ITP_mapset (m, x, y) -> f_appM "fun_upd" [ rets "Z.eqb"; aux m; aux x; aux y ]
    | CI.ITP_mapget (m, x) ->
      (match x with
       (* case for array member accesses *)
       | CI.ITP_const (ITP_bits _) ->
         parensM (build [ rets "nth"; aux x; aux m; rets "0" ])
       | _ -> parensM (build [ aux m; aux x ]))
    | CI.ITP_recordmember (t, _, ix) -> gen_get_upd ix (aux t)
    | CI.ITP_recordupdate ((t, _), x, ix) ->
      let op_nm = gen_get_upd ix (aux t) in
      parensM (build [ op_nm; aux x ])
    | CI.ITP_record l ->
      let xs = List.map aux l in
      parensM (flow (comma ^^ break 1) xs)
    | CI.ITP_structmember (t, CI.ITP_id fieldnm, _) ->
      aux t ^^ !^"." ^^ parens !^(Id.get_string fieldnm)
    | CI.ITP_structupdate ((t, _), x, ix) ->
      let op_nm = gen_get_upd ix (aux t) in
      parensM (build [ op_nm; aux x ])
    | CI.ITP_cast (_, x) -> aux x
    | CI.ITP_apply (CI.ITP_sym name, args) ->
      parensM (build ([ Sym.pp name ] @ List.map aux args))
    | CI.ITP_apply_prop (CI.ITP_sym name, args) ->
      let r = parensM (build ([ Sym.pp name ] @ List.map aux args)) in
      f_appM "Is_true" [ r ]
    | CI.ITP_representable (CI.ITP_sym s, _, t) ->
      let op_nm = "representable_" ^ Sym.pp_string s in
      parensM (build [ rets op_nm; aux t ])
    | CI.ITP_constructor (CI.ITP_sym name, args) ->
      parensM (build ([ Sym.pp name ] @ List.map aux args))
    | CI.ITP_nthlist (n, xs, d) ->
      parensM (build [ rets "CN_Lib.nth_list_z"; aux n; aux xs; aux d ])
    | CI.ITP_arraytolist (arr, i, len) ->
      parensM (build [ rets "CN_Lib.array_to_list"; aux arr; aux i; aux len ])
    | CI.ITP_wrapI (z1, z2, t) -> f_appM "CN_Lib.wrapI" [ enc_z z1; enc_z z2; aux t ]
    | CI.ITP_let_pure (CI.ITP_sym nm, x, y) -> parensM (pp_let nm (aux x) (aux y))
    | CI.ITP_arrayshift (base, ct, index) ->
      f_appM "arrayshift" [ aux base; enc_z ct; aux index ]
    | CI.ITP_good -> rets ""
    | CI.ITP_retsym -> rets ret_sym
    | CI.ITP_unsupported_pure msg -> rets ("unsupported ITP_pure_term: " ^ msg)
  in
  f global t


let rec resource_to_itp (global : Global.t) (t : CI.itp_resource_term) =
  let open Pp in
  let mk_wand doc doc2 = doc ^^^ !^"-∗" ^^^ doc2 in
  let mk_star doc doc2 = doc ^^^ !^"∗" ^^^ doc2 in
  let aux t = term_to_itp global t in
  let aux' t = resource_to_itp global t in
  let map_split f = fun doc -> f (break 1 ^^ doc) in
  match t with
  | CI.ITP_Let_Resource (CI.ITP_sym nm, x, y) -> parensM (pp_let nm (aux x) (aux' y))
  | CI.ITP_Forall (CI.ITP_sym sym, bt, t) -> pp_forall sym bt (aux' t)
  | CI.ITP_Exists (CI.ITP_sym sym, bt, t) ->
    !^"∃" ^^^ parens (typ (Sym.pp sym) (bt_to_itp bt)) ^^ !^"," ^^ break 1 ^^ aux' t
  | CI.ITP_Star (t1, t2) -> mk_star (aux' t1) (aux' t2)
  | CI.ITP_Wand (t1, t2) -> mk_wand (aux' t1) (aux' t2)
  | CI.ITP_Pure t -> iris_pure (aux t)
  | CI.ITP_Define (CI.ITP_sym sym, x, y) -> map_split (pp_let sym (aux x)) (aux' y)
  | CI.ITP_Empty_Heap -> rets "emp"
  | CI.ITP_Block (CI.ITP_sym s, _, t, _) ->
    let op_nm = "Block_" ^ Sym.pp_string s in
    parensM (build [ rets op_nm; aux' t ])
  | CI.ITP_Owned (op_nm, ptr, CI.ITP_sym rt, t) ->
    build [ rets op_nm; aux ptr; rets (Sym.pp_string rt); aux' t ]
  | CI.ITP_PName (CI.ITP_sym nm, CI.ITP_sym pname, iargs, ptr) ->
    let args = List.map aux iargs in
    build ((Sym.pp pname :: aux ptr :: args) @ [ Sym.pp nm ])
  | CI.ITP_Each (ITP_sym nm, ptr, perm, pred) ->
    (match perm with
     | ITP_binop
         (ITP_and_prop, ITP_binop (_, min_term, _, _), ITP_binop (_, _, max_term, _), _)
       ->
       let min_doc a = parens (rets "Z.to_nat " ^^ a) in
       build
         [ rets "each_int ";
           min_doc (aux min_term);
           parens
             (rets "Z.to_nat "
              ^^ parens (aux max_term)
              ^^ rets " - "
              ^^ min_doc (aux min_term))
           ^^ rets "%nat";
           aux ptr;
           !^(Sym.pp_string nm);
           aux' pred
         ]
     | _ -> rets "unsupported ITP_Each_LAT perm")
  | CI.ITP_Good -> rets ""
  | CI.ITP_Unsupported_Resource msg -> rets msg


let convert_lemma_defs global (lemmas : CI.itp_lemma list) =
  let lemma_ty (CI.ITP_lemma (CI.ITP_sym nm, tm)) =
    Pp.progress_simple "converting lemma type" (Sym.pp_string nm);
    let rhs = resource_to_itp global tm in
    defn (Sym.pp_string nm ^ "_type") [] (Some (Pp.string "iProp Σ")) rhs false
  in
  let tys = List.map lemma_ty lemmas in
  tys


(* print datatypes *)
let translate_datatypes (dtys : CI.itp_dt list list) =
  let open Pp in
  let cons_line dt_tag (CI.ITP_constr (CI.ITP_sym nm, params)) =
    let argTs = List.map (fun bt -> bt_to_itp bt) params in
    !^"    | " ^^ Sym.pp nm ^^^ colon ^^^ flow !^" -> " (argTs @ [ Sym.pp dt_tag ])
  in
  let dt_eqs (CI.ITP_dt (CI.ITP_sym nm, _, constr)) =
    let c_lines = List.map (cons_line nm) constr in
    !^"    " ^^ Sym.pp nm ^^^ colon ^^^ !^"Type :=" ^^ hardline ^^ flow hardline c_lines
  in
  let print_dt dty_clump =
    flow
      hardline
      (List.mapi
         (fun i doc -> !^(if i = 0 then "  Inductive" else "    with") ^^ hardline ^^ doc)
         (List.map dt_eqs dty_clump))
    ^^ !^"."
    ^^ hardline
  in
  let rec f (dtys : CI.itp_dt list list) =
    match dtys with [] -> [] | x :: xs -> print_dt x :: f xs
  in
  f dtys


let rec scanl (f : 'b -> 'a -> 'b) (q : 'b) (ls : 'a list) =
  q :: (match ls with [] -> [] | x :: xs -> scanl f (f q x) xs)


let scanl1 f ls = match ls with x :: xs -> scanl f x xs | [] -> []

(* Generates `Ptr -> arg_ty1 -> ... -> ret_ty -> iProp Σ` *)
let make_pred_ty args ret_ty res_ty =
  let open Pp in
  (* add pointer arg *)
  let arg_bts = CI.ITP_Loc :: List.map snd args in
  let arg_types = List.map bt_to_itp arg_bts @ [ bt_to_itp ret_ty ] in
  List.fold_right (fun arg result -> infix 2 1 !^"->" arg result) arg_types !^res_ty


(* print resource predicate definitions *)
let translate_pred (gl : Global.t) (preds : CI.itp_resource_pred_group list) =
  let open Pp in
  let unpack_clauses (clauses : CI.itp_clause list) =
    let clause_to_itp (clause : CI.itp_clause) =
      match clause with
      | CI.ITP_clause (guard, body) ->
        (* assuming all guards are pure *)
        let guard_doc =
          match guard with
          | x :: xs ->
            iris_pure (term_to_itp gl x)
            ^^ intersperse
                 ""
                 ""
                 (List.map (fun y -> !^" ∧ " ^^ iris_pure (!^"~" ^^ term_to_itp gl y)) xs)
          | [] -> rets "True"
        in
        let body_doc = resource_to_itp gl body in
        parensM (build [ guard_doc; rets " ∧ "; body_doc ])
    in
    (* add all previous guards to the beginnig of each clause *)
    let clause_concat (c1 : CI.itp_clause) (c2 : CI.itp_clause) =
      match c1 with
      | CI.ITP_clause (guard, _) ->
        (match c2 with
         | CI.ITP_clause (guard2, body) -> CI.ITP_clause (guard2 @ guard, body))
    in
    List.map clause_to_itp (scanl1 clause_concat clauses)
  in
  let make_one_arg = function
    | CI.ITP_sym id, bt -> parens (typ (Sym.pp id) (bt_to_itp bt))
  in
  let unpack_sym (CI.ITP_sym sym) = sym in
  let get_pred_name (pred : CI.itp_resource_pred) = unpack_sym pred.CI.name in
  let make_formal_args (pred : CI.itp_resource_pred) =
    let ptr = unpack_sym pred.CI.ptr in
    let ptr_arg = parens (typ (Sym.pp ptr) (bt_to_itp CI.ITP_Loc)) in
    let ret_arg = parens (typ !^ret_sym (bt_to_itp pred.CI.ret_bt)) in
    (ptr_arg :: List.map make_one_arg pred.CI.args) @ [ ret_arg ]
  in
  let make_actual_args (pred : CI.itp_resource_pred) =
    let ptr = unpack_sym pred.CI.ptr in
    let args = List.map (fun (arg, _) -> Sym.pp (unpack_sym arg)) pred.CI.args in
    (Sym.pp ptr :: args) @ [ !^ret_sym ]
  in
  let make_args (group : CI.itp_resource_pred_group) (pred : CI.itp_resource_pred) =
    let make_rec_arg (arg : CI.itp_resource_pred) =
      let ty = make_pred_ty arg.args arg.ret_bt "iProp Σ" in
      parens (typ (Sym.pp (get_pred_name arg)) ty)
    in
    let rec_args = List.map make_rec_arg group in
    rec_args @ make_formal_args pred
  in
  let get_body_name (pred : CI.itp_resource_pred) =
    Sym.pp_string (get_pred_name pred) ^ "_body"
  in
  let unpack_body (group : CI.itp_resource_pred_group) (pred : CI.itp_resource_pred) =
    defn
      (get_body_name pred)
      (make_args group pred)
      (Some (Pp.string "iProp Σ"))
      (intersperse " ∨ " "" (unpack_clauses pred.CI.clauses))
      false
  in
  let get_constr_name (pred : CI.itp_resource_pred) =
    "CN_GROUP_" ^ Sym.pp_string (get_pred_name pred)
  in
  let get_group_type_name index = "cn_predicate_group_" ^ string_of_int index in
  let make_constr type_name pred =
    let constr_name = get_constr_name pred in
    typ (Pp.string constr_name) (make_pred_ty pred.args pred.ret_bt type_name)
  in
  let make_group_type index (predicates : CI.itp_resource_pred_group) =
    let type_name = get_group_type_name index in
    let constrs = List.map (make_constr type_name) predicates in
    let group_type =
      !^("Inductive " ^ type_name ^ " : Type :=")
      ^^ hardline
      ^^ flow hardline (List.map (fun c -> !^"  | " ^^ c) constrs)
      ^^ !^"."
      ^^ hardline
    in
    let ofe_type =
      !^("Canonical Structure " ^ type_name ^ "O := leibnizO " ^ type_name ^ ".")
      ^^ hardline
    in
    group_type ^^ ofe_type
  in
  (* (λ p ν, rec (CN_GROUP_IsForest p ν)) *)
  let make_closure (pred : CI.itp_resource_pred) =
    let args = make_actual_args pred in
    let call = parensM @@ build @@ (!^(get_constr_name pred) :: args) in
    let binders = build args in
    let body = parensM @@ build [ !^"rec"; call ] in
    parensM @@ (!^"λ" ^^^ binders ^^ comma) ^//^ body
  in
  (* CN_GROUP_IsForest p ν =>
      IsForest_body
        (λ p ν, (rec (CN_GROUP_IsForest p ν)))
        (λ p ν, (rec (CN_GROUP_IsTree p ν)))
        p ν *)
  let make_case predicates (pred : CI.itp_resource_pred) =
    let body_name = !^(get_body_name pred) in
    let args = make_actual_args pred in
    let closures = List.map make_closure predicates in
    let body = build @@ (body_name :: closures) @ args in
    let pattern = build @@ (!^(get_constr_name pred) :: args) in
    infix 2 1 !^"=>" pattern body
  in
  let get_pre_fixpoint_name index = "cn_predicate_group_pre_" ^ string_of_int index in
  let make_pre_fixpoint_body (predicates : CI.itp_resource_pred_group) =
    let cases =
      predicates
      |> List.map (make_case predicates)
      |> List.map (fun case -> !^"| " ^^ case)
      |> flow hardline
    in
    align @@ !^"match call with" ^^ nest 2 (hardline ^^ cases) ^^ hardline ^^ !^"end"
  in
  let make_pre_fixpoint_definition index (predicates : CI.itp_resource_pred_group) =
    let group_ofe_name = get_group_type_name index ^ "O" in
    let rec_type = flow !^" -> " [ !^group_ofe_name; !^"iProp Σ" ] in
    let rec_arg = parens @@ typ !^"rec" rec_type in
    let call_arg = parens @@ typ !^"call" !^group_ofe_name in
    defn
      (get_pre_fixpoint_name index)
      [ rec_arg; call_arg ]
      (Some !^"iProp Σ")
      (make_pre_fixpoint_body predicates)
      false
  in
  let make_monotonicity_instance index (predicates : CI.itp_resource_pred_group) =
    let pre_fixpoint = get_pre_fixpoint_name index in
    let instance_name = get_group_type_name index ^ "_mono" in
    let instance =
      blank 2
      ^^ align
           (infix
              2
              1
              colon
              (!^"Local Instance" ^^^ !^instance_name)
              (!^"BiMonoPred" ^^^ !^pre_fixpoint ^^ dot))
    in
    let prepare =
      !^"ltac:"
      ^^ parens
           (build
              [ !^"unfold";
                intersperse "," "" @@ List.map (fun p -> !^(get_body_name p)) predicates
              ])
    in
    let tactic =
      prefix
        2
        1
        (group (!^"solve_bi_mono_pred_with_prepare" ^/^ !^pre_fixpoint))
        (prepare ^^ dot)
    in
    let proof =
      blank 2 ^^ align (!^"Proof." ^^ nest 2 (hardline ^^ tactic) ^^ hardline ^^ !^"Qed.")
    in
    instance ^^ hardline ^^ proof ^^ hardline
  in
  let make_pre_fixpoint index (predicates : CI.itp_resource_pred_group) =
    let definition = make_pre_fixpoint_definition index predicates in
    let mono = make_monotonicity_instance index predicates in
    definition ^^ mono
  in
  let make_fixpoint index (pred : CI.itp_resource_pred) =
    let args = make_formal_args pred in
    let app = !^(get_constr_name pred) :: make_actual_args pred in
    let body =
      build
        [ !^"bi_least_fixpoint"; !^(get_pre_fixpoint_name index); parens @@ build app ]
    in
    defn (Sym.pp_string (get_pred_name pred)) args (Some !^"iProp Σ") body false
  in
  let make_fixpoints index (predicates : CI.itp_resource_pred_group) =
    let pre_fixpoint = make_pre_fixpoint index predicates in
    let fixpoints = List.map (make_fixpoint index) predicates in
    pre_fixpoint ^^ hardline ^^ flow hardline fixpoints
  in
  (* induction lemma *)
  let get_pred_prop_name (pred : CI.itp_resource_pred) =
    "Φ_" ^ Sym.pp_string (get_pred_name pred)
  in
  let make_induction_lemma_arg (pred : CI.itp_resource_pred) =
    let ty = make_pred_ty pred.args pred.ret_bt "iProp Σ" in
    let name = get_pred_prop_name pred in
    typ (Pp.string name) ty
  in
  let make_forall vars body =
    let binders = List.map (fun (nm, bt) -> parens @@ typ nm (bt_to_itp bt)) vars in
    group @@ !^"∀" ^^^ build binders ^^ comma ^^ nest 2 (break 1 ^^ body)
  in
  let get_pred_vars (pred : CI.itp_resource_pred) =
    let vars =
      List.map (fun (CI.ITP_sym sym, bt) -> (Sym.pp sym, bt)) pred.args
      @ [ (!^ret_sym, pred.ret_bt) ]
    in
    (Sym.pp (unpack_sym pred.ptr), CI.ITP_Loc) :: vars
  in
  let make_lemma proof_name args statement proof =
    let s =
      !^"Lemma"
      ^^^ proof_name
      ^^ nest 4 (hardline ^^ flow hardline args)
      ^^ space
      ^^ colon
      ^^ nest 2 (hardline ^^ statement)
      ^^ dot
      ^^ hardline
    in
    let proof =
      blank 2
      ^^ align
           (!^"Proof" ^^ dot ^^ nest 2 (hardline ^^ proof) ^^ hardline ^^ !^"Qed" ^^ dot)
    in
    s ^^ proof
  in
  let unfold_predicate_bodies predicates statement =
    let body_names =
      separate (comma ^^ space) (List.map (fun pred -> !^(get_body_name pred)) predicates)
    in
    !^"ltac:"
    ^^ parens
         (align
            (!^"let T := constr:"
             ^^ parens statement
             ^^^ !^"in"
             ^^ hardline
             ^^ !^"let T' := eval unfold"
             ^^^ body_names
             ^^^ !^"in T in"
             ^^ hardline
             ^^ !^"exact T'"))
  in
  let make_induction_lemma index (predicates : CI.itp_resource_pred_group) =
    let make_assumption predicates (pred : CI.itp_resource_pred) =
      (* □ (∀ (p : Ptr) (ν : forest), IsForest_body Φ_IsForest Φ_IsTree p ν -∗ Φ_IsForest p ν) -∗ *)
      let vars = get_pred_vars pred in
      let extended_vars =
        List.map (fun p -> !^(get_pred_prop_name p)) predicates @ List.map fst vars
      in
      let body1 = parensM @@ build @@ (!^(get_body_name pred) :: extended_vars) in
      let body2 =
        parensM @@ build @@ (!^(get_pred_prop_name pred) :: List.map fst vars)
      in
      let body = infix 2 1 !^"-∗" body1 body2 in
      !^"□" ^^^ parens @@ make_forall vars body
    in
    let make_conc (pred : CI.itp_resource_pred) =
      (* (∀ (p : Ptr) (ν : forest), IsForest p ν -∗ Φ_IsForest p ν) *)
      let vars = get_pred_vars pred in
      let body1 =
        parensM @@ build @@ (Sym.pp (get_pred_name pred) :: List.map fst vars)
      in
      let body2 =
        parensM @@ build @@ (!^(get_pred_prop_name pred) :: List.map fst vars)
      in
      let body = infix 2 1 !^"-∗" body1 body2 in
      make_forall vars body
    in
    let make_induction_lemma_statement predicates =
      let assumptions = List.map (make_assumption predicates) predicates in
      let concs = List.map make_conc predicates in
      let and_sep = space ^^ !^"∧" ^^ break 1 in
      let conclusion = flow and_sep (List.map parens concs) in
      separate (space ^^ !^"-∗" ^^ hardline) @@ assumptions @ [ conclusion ]
    in
    let make_induction_lemma_proof index (predicates : CI.itp_resource_pred_group) =
      let name = !^(get_pre_fixpoint_name index) in
      let cases =
        List.map
          (fun p ->
             let vars = List.map fst (get_pred_vars p) in
             let body = build @@ (!^(get_constr_name p) :: vars) in
             let head = build @@ (!^(get_pred_prop_name p) :: vars) in
             infix 2 1 !^"=>" body head)
          predicates
      in
      let body = cases |> List.map (fun case -> bar ^^^ case) |> flow hardline in
      let arg1 =
        parens
        @@ align
        @@ !^"fun call =>"
        ^^ nest
             2
             (hardline
              ^^ !^"match call with"
              ^^ nest 2 (hardline ^^ body)
              ^^ hardline
              ^^ !^"end")
      in
      let cases =
        List.map
          (fun p ->
             !^"iApply"
             ^^^ parens
                   (!^"\"" ^^ !^"H_" ^^ Sym.pp (get_pred_name p) ^^ !^"\" with \"Hbody\""))
          predicates
      in
      let body = flow (hardline ^^ bar ^^ space) cases in
      let arg2 =
        !^"ltac" ^^ colon ^^ parens (!^"first" ^^^ brackets (nest 2 (hardline ^^ body)))
      in
      let arg3 =
        !^"ltac"
        ^^ colon
        ^^ parens
             (!^"unfold"
              ^^^ separate
                    (comma ^^ space)
                    (List.map (fun p -> Sym.pp (get_pred_name p)) predicates))
        ^^ dot
      in
      !^"iIntros \""
      ^^ build (List.map (fun p -> !^"#H_" ^^ Sym.pp (get_pred_name p)) predicates)
      ^^ !^"\""
      ^^ dot
      ^^^ hardline
      ^^^ !^"solve_cn_predicate_induction"
      ^^^ nest 2 (hardline ^^ flow hardline [ name; arg1; arg2; arg3 ])
    in
    let args = List.map (fun p -> p |> make_induction_lemma_arg |> parens) predicates in
    let statement =
      make_induction_lemma_statement predicates |> unfold_predicate_bodies predicates
    in
    let proof = make_induction_lemma_proof index predicates in
    let names = List.map (fun p -> Sym.pp (get_pred_name p)) predicates in
    let proof_name = separate underscore names ^^ underscore ^^ !^"induction" in
    make_lemma proof_name args statement proof
  in
  let make_unfold_lemma
        index
        (predicates : CI.itp_resource_pred_group)
        (pred : CI.itp_resource_pred)
    =
    let proof_name = Sym.pp (get_pred_name pred) ^^ underscore ^^ !^"unfold" in
    let args = get_pred_vars pred in
    (* IsForest p ν ⊣⊢ IsForest_body IsForest IsTree p ν. *)
    let statement =
      let body1 =
        parensM @@ build @@ (Sym.pp (get_pred_name pred) :: List.map fst args)
      in
      let body2 =
        parensM
        @@ build
        @@ (!^(get_body_name pred)
            :: List.map (fun p -> Sym.pp (get_pred_name p)) predicates)
        @ List.map fst args
      in
      infix 2 1 !^"⊣⊢" body1 body2 |> unfold_predicate_bodies [ pred ]
    in
    let proof =
      let rewrites = List.map (fun p -> !^"/" ^^ Sym.pp (get_pred_name p)) predicates in
      let rem =
        [ "least_fixpoint_unfold"; "/" ^ get_pre_fixpoint_name index; "/="; "//" ]
      in
      (build @@ (!^"rewrite" :: rewrites) @ List.map ( !^ ) rem) ^^ dot
    in
    make_lemma
      proof_name
      (List.map (fun (nm, bt) -> parens (typ nm (bt_to_itp bt))) args)
      statement
      proof
  in
  let make_opaque (predicates : CI.itp_resource_pred_group) =
    !^"Global Opaque"
    ^^^ build (List.map (fun pred -> Sym.pp (get_pred_name pred)) predicates)
    ^^ dot
    ^^ hardline
  in
  let unpack_group index (predicates : CI.itp_resource_pred_group) =
    let group_type = make_group_type index predicates in
    let pred_defs = List.map (unpack_body predicates) predicates in
    let fixpoint = make_fixpoints index predicates in
    let induction_lemma = make_induction_lemma index predicates in
    let unfold_lemmata = List.map (make_unfold_lemma index predicates) predicates in
    let opaque = make_opaque predicates in
    (group_type, pred_defs @ (fixpoint :: induction_lemma :: unfold_lemmata) @ [ opaque ])
  in
  let groups = List.mapi unpack_group preds in
  (List.map fst groups, List.concat_map snd groups)


let translate_uninterp_pred =
  let open Pp in
  List.map (fun (CI.ITP_sym nm, _, args, ret_ty) ->
    let ty = make_pred_ty args ret_ty "iProp Σ" in
    (!^"  Parameter" ^^^ typ (Sym.pp nm) ty ^^ !^"." ^^ hardline) ^^ hardline)


(* translate functions to ITP *)
let translate_fun (gl : Global.t) (funs : CI.itp_fun list list * CI.itp_fun list list) =
  let open Pp in
  let translate_one cf =
    match cf with
    | CI.ITP_fun_def (CI.ITP_sym nm, logical_fun, args, _) ->
      (match logical_fun with
       | CI.ITP_def body ->
         let itp_body = term_to_itp gl body in
         let itp_args =
           List.map
             (fun (CI.ITP_sym arg, bt) ->
                let itp_bt = bt_to_itp bt in
                Pp.parens (Pp.typ (Sym.pp arg) itp_bt))
             args
         in
         defn (Sym.pp_string nm) itp_args None itp_body false
       | CI.ITP_recdef body ->
         let itp_body = term_to_itp gl body in
         let itp_args =
           List.map
             (fun (CI.ITP_sym arg, bt) ->
                let itp_bt = bt_to_itp bt in
                Pp.parens (Pp.typ (Sym.pp arg) itp_bt))
             args
         in
         defn (Sym.pp_string nm) itp_args None itp_body true)
    | CI.ITP_fun_uninterp (CI.ITP_sym nm, logical_fun, args, ret_typ) ->
      (match logical_fun with
       | CI.ITP_uninterp ->
         let itp_arg_typs = List.map (fun (_, bt) -> bt_to_itp bt) args in
         let itp_rt = bt_to_itp ret_typ in
         let ty =
           List.fold_right (fun at rt -> at ^^^ !^"->" ^^^ rt) itp_arg_typs itp_rt
         in
         !^"  Parameter" ^^^ typ (Sym.pp nm) ty ^^ !^"." ^^ hardline
       | CI.ITP_uninterp_prop ->
         let itp_arg_typs = List.map (fun (_, bt) -> bt_to_itp bt) args in
         let itp_rt = !^"Prop" in
         let ty =
           List.fold_right (fun at rt -> at ^^^ !^"->" ^^^ rt) itp_arg_typs itp_rt
         in
         !^"  Parameter" ^^^ typ (Sym.pp nm) ty ^^ !^"." ^^ hardline)
  in
  let print clump =
    flow
      hardline
      (List.mapi
         (fun i doc -> !^(if i = 0 then "" else "    with") ^^ doc)
         (List.map translate_one clump))
  in
  (List.map print (fst funs), List.map print (snd funs))


(* generate records and Owned_Structname predicates for all structs*)
let translate_structs (struct_decls : Memory.struct_decls) =
  let open Pp in
  let piece_to_owned (piece : Memory.struct_piece) =
    let make_owned (nm : string) (id : Id.t) =
      !^(nm ^ " ")
      ^^ parens
           !^("CN_Lib_Iris.shift l "
              ^ string_of_int piece.offset
              ^ " "
              ^ string_of_int piece.size)
      ^^ !^" v."
      ^^ parens !^(Id.get_string id)
    in
    match piece.member_or_padding with
    | Some (id, ctyp) ->
      (match ctyp with
       | Void -> rets "unsupported ctype void"
       | Integer _ -> make_owned "Owned_int" id
       | Array _ -> rets "unsupported ctype array"
       (* todo: probably not right? *)
       | Pointer _ -> make_owned "Owned_int" id
       | Struct s -> make_owned ("Owned_" ^ Sym.pp_string s) id
       | Function _ -> rets "unsupported ctype function"
       | Byte -> rets "unsupported ctype function")
      (* TODO(HK): added for plumbing *)
    | None ->
      !^"padding "
      ^^ parens (!^"arrayshift " ^^ !^"l " ^^ !^(string_of_int piece.offset) ^^ !^" 1")
      ^^ !^(" " ^ string_of_int piece.size)
  in
  let rec decl_to_pieces (pieces : Memory.struct_piece list) =
    match pieces with
    | [] -> rets ""
    | x :: [] -> piece_to_owned x ^^ !^"."
    | x :: xs -> piece_to_owned x ^^ !^" ∗ " ^^ decl_to_pieces xs
  in
  let get_struct_field (piece : Memory.struct_piece) =
    match piece.member_or_padding with
    | Some (id, ctype) ->
      !^("  " ^ Id.get_string id ^ " : " ^ print_ctype ctype ^ "; ") ^^ hardline
    | None -> rets ""
  in
  let unpack_decls (decl : Sym.t * Memory.struct_layout) =
    let nm = !^(Sym.pp_string (fst decl)) in
    !^"  Record "
    ^^ nm
    ^^ !^" : Type := { "
    ^^ hardline
    ^^ build (List.map get_struct_field (snd decl))
    ^^ !^" }."
    ^^ hardline
    ^^ hardline
    ^^ !^"  Definition "
    ^^ !^"Owned_"
    ^^ nm
    ^^ !^" (l: Ptr) (v : "
    ^^ nm
    ^^ !^") : iProp Σ := "
    ^^ decl_to_pieces (snd decl)
    ^^ hardline
  in
  List.map unpack_decls (Sym.Map.bindings struct_decls)


(* main generate function, makes everything happen *)
let generate (global : Global.t) directions (lemmata : (Sym.t * (Loc.t * AT.lemmat)) list)
  =
  let f =
    let filename, _kinds = parse_directions directions in
    let channel = open_out filename in
    Pp.print channel (header filename);
    (* translate everything to itp AST*)
    let (CI.ITP_gl (dtys, funs, preds, uninterp_preds, lemmas)) =
      CC.cn_to_itp_ir global lemmata
    in
    (* print datatypes *)
    let dtypes = translate_datatypes dtys in
    let structs =
      if global.struct_decls == Sym.Map.empty then
        [ Pp.string "(* no struct definitions required *)" ]
      else
        translate_structs global.struct_decls
    in
    let translated_funs = translate_fun global funs in
    let translated_uninterp_preds = translate_uninterp_pred uninterp_preds in
    let pred_group_tys, translated_preds = translate_pred global preds in
    (* print datatypes *)
    Pp.print channel (types_spec (dtypes @ pred_group_tys));
    (* print uninterpreted logical functions and resource predicates as parameters *)
    Pp.print channel (param_spec (fst translated_funs));
    (* print structs and function definitions *)
    Pp.print
      channel
      (defs_module (structs @ translated_uninterp_preds @ snd translated_funs));
    (* print resource predicates *)
    Pp.print channel (pred_spec translated_preds);
    (* print function definitions *)
    (* print lemmas *)
    let translated_lemmas = convert_lemma_defs global lemmas in
    Pp.print channel (lemmas_module [] translated_lemmas);
    Pp.print
      channel
      (mod_spec (List.map (fun (CI.ITP_lemma (CI.ITP_sym nm, _)) -> nm) lemmas))
  in
  Result.Ok f
