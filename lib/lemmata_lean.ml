module BT = BaseTypes
module AT = ArgumentTypes
module Loc = Locations
module StringSet = Set.Make (String)
module CI = Coq_ir
module CC = Cn_to_coq

let ret_sym = "ν"

let parse_directions directions = (directions, StringSet.singleton "all")

let header filename =
  let open Pp in
  !^"-- "
  ^^^ !^filename
  ^^ !^": generated lemma specifications from CN *)"
  ^^ hardline
  ^^ hardline
  ^^ !^"import Playground.CN_Lib"
  ^^ hardline
  ^^ !^"import Playground.CN_Lib_Iris"
  ^^ hardline
  ^^ !^"import Playground.CN_Lib_Iris_Fixpoint"
  ^^ hardline
  ^^ !^"import Iris.ProofMode"
  ^^ hardline
  ^^ hardline
  ^^ hardline
  ^^ !^"namespace Gen_Spec"
  ^^ hardline
  ^^ hardline
  ^^ !^"open Iris CN_Lib_Iris ProofMode"
  ^^ hardline
  ^^ hardline
  ^^ !^"variable {hlc GF} [MyHeap hlc GF]"
  ^^ hardline
  ^^ hardline


let print_section section_name comment section_body =
  let open Pp in
  !^"section "
  ^^^ !^section_name
  ^^ hardline
  ^^ hardline
  ^^^ !^"-- "
  ^^ !^comment
  ^^ hardline
  ^^ hardline
  ^^ flow hardline section_body
  ^^ hardline
  ^^ hardline
  ^^ !^"end "
  ^^^ !^section_name
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
  | Integer _ -> "Int"
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

let defn nm args opt_ty rhs =
  let open Pp in
  let tyeq = match opt_ty with None -> [] | Some ty -> [ colon; ty ] in
  flow (break 1) ([ !^"def"; !^nm ] @ args @ tyeq @ [ !^":=" ])
  ^^ hardline
  ^^ !^"  "
  ^^ rhs
  ^^ hardline


let binop s x y =
  let open Pp in
  parens (flow (break 1) [ x; !^s; y ])


let tuple_itp_ty doc fld_tys =
  let open Pp in
  let rec times = function
    | [] -> fail "tuple_itp_ty: empty" doc
    | [ x ] -> [ x ]
    | x :: xs -> x :: !^" × " :: times xs
  in
  parens (flow (break 1) (times fld_tys))


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


(* CN BaseTypes to Lean *)
let rec bt_to_itp (bt : CI.itp_bt) =
  let open Pp in
  match bt with
  | CI.ITP_Bool -> !^"Bool"
  | CI.ITP_Integer -> !^"Int"
  | CI.ITP_Bits _ -> !^"Int"
  | CI.ITP_Map (x, y) ->
    let enc_x = bt_to_itp x in
    let enc_y = bt_to_itp y in
    parens (binop "->" enc_x enc_y)
  | CI.ITP_Struct (CI.ITP_sym tag, _) -> Sym.pp tag
  | CI.ITP_Record mems ->
    let enc_mem_bts = List.map bt_to_itp mems in
    tuple_itp_ty !^"structure" enc_mem_bts
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
let pp_let sym rhs_doc doc is_resource =
  let open Pp in
  !^"let" ^^^ Sym.pp sym ^^^ !^":=" ^^ rhs_doc ^^ !^";" ^^ (if is_resource then !^" iprop%" else !^"")  ^^^ doc


let pp_forall (sym : Sym.t) (bt : CI.itp_bt) (doc : Pp.document) =
  let open Pp in
  !^"∀" ^^^ parens (typ (Sym.pp sym) (bt_to_itp bt)) ^^ !^"," ^^ break 1 ^^ doc


let norm_bv_op bt doc_f =
  match bt with
  | CI.ITP_Bits (sign, sz) ->
    (match sign with
     | CI.ITP_Unsigned ->
       let minInt, maxInt = BT.bits_range (Unsigned, sz) in
       f_appM "wrapI" [ enc_z minInt; enc_z maxInt; doc_f ]
     | CI.ITP_Signed ->
       let minInt, maxInt = BT.bits_range (Signed, sz) in
       f_appM "wrapI" [ enc_z minInt; enc_z maxInt; doc_f ])
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
    let bool_binop t = parensM (build [ rets "decide"; t ]) in
    match t with
    | CI.ITP_sym_term (CI.ITP_sym s) -> Sym.pp s
    | ITP_const c ->
      (match c with
       | ITP_bool b -> rets (if b then "true" else "false")
       | ITP_bool_prop b -> rets (if b then "True" else "False")
       | ITP_Z z -> enc_z z
       | ITP_bits z -> parensM (rets (Z.to_string z)))
    | ITP_unop (op, x, bt) ->
      norm_bv_op
        bt
        (match op with
         | CI.ITP_neg -> f_appM "!" [ aux x ]
         | CI.ITP_neg_prop -> f_appM "¬" [ aux x ]
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
         | CI.ITP_lt -> bool_binop (abinop "<" x y)
         | CI.ITP_lt_prop -> abinop "<" x y
         | CI.ITP_le -> bool_binop (abinop "<=" x y)
         | CI.ITP_le_prop -> abinop "<=" x y
         | CI.ITP_exp -> abinop "^" x y
         (* TODO: what are these?? *)
         | CI.ITP_bwxor -> abinop "^^" x y
         | CI.ITP_bwand -> abinop "&&" x y
         | CI.ITP_bwor -> abinop "||" x y
         | CI.ITP_eq -> abinop "==" x y
         | CI.ITP_eq_prop -> abinop "=" x y
         | CI.ITP_and -> abinop "&&" x y
         | CI.ITP_and_prop -> abinop "∧" x y
         | CI.ITP_or -> abinop "||" x y
         | CI.ITP_or_prop -> abinop "∨" x y
         (* Apparently Lean doesn't have builtin boolean implication *)
         | CI.ITP_impl ->
           f global (CI.ITP_binop (CI.ITP_or, CI.ITP_unop (CI.ITP_neg, x, bt), y, bt))
         | CI.ITP_impl_prop -> abinop "-∗" x y)
    | CI.ITP_match (x, cases) ->
      let br (pat, rhs) = build [ rets "|"; pat_to_itp pat; rets "=>"; aux rhs ] in
      parensM (build ([ rets "match"; aux x; rets "with"; hardline ] @ List.map br cases))
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
    (* TODO: figure these ones out *)
    | CI.ITP_mapset _ -> rets "unsupported mapset"
    | CI.ITP_mapget _ -> rets "unsupported mapget"
    | CI.ITP_recordmember (t, _, ix) -> gen_get_upd ix (aux t)
    | CI.ITP_recordupdate ((t, _), x, ix) ->
      let op_nm = gen_get_upd ix (aux t) in
      parensM (build [ op_nm; aux x ])
    | CI.ITP_record l ->
      let xs = List.map aux l in
      parensM (flow (comma ^^ break 1) xs)
    | CI.ITP_structmember (t, CI.ITP_id fieldnm, _) ->
      aux t ^^ !^"." ^^ !^(Id.get_string fieldnm)
    | CI.ITP_structupdate ((t, _), x, ix) ->
      let op_nm = gen_get_upd ix (aux t) in
      parensM (build [ op_nm; aux x ])
    | CI.ITP_cast (_, x) -> aux x
    | CI.ITP_apply (CI.ITP_sym name, args) ->
      parensM (build ([ Sym.pp name ] @ List.map aux args))
    | CI.ITP_apply_prop (CI.ITP_sym name, args) ->
      parensM (build ([ Sym.pp name ] @ List.map aux args))
    | CI.ITP_representable (CI.ITP_sym s, _, t) ->
      let op_nm = "representable_" ^ Sym.pp_string s in
      parensM (build [ rets op_nm; aux t ])
    | CI.ITP_constructor (CI.ITP_sym name, args) ->
      parensM (build ([ Sym.pp name ] @ List.map aux args))
    | CI.ITP_nthlist (n, xs, d) ->
      parensM (build [ rets "CN_Lib.nth_list_int"; aux n; aux xs; aux d ])
    | CI.ITP_arraytolist (arr, i, len) ->
      parensM (build [ rets "CN_Lib.array_to_list"; aux arr; aux i; aux len ])
    | CI.ITP_wrapI (z1, z2, t) -> f_appM "CN_Lib.wrapI" [ enc_z z1; enc_z z2; aux t ]
    | CI.ITP_let_pure (CI.ITP_sym nm, x, y) -> parensM (pp_let nm (aux x) (aux y) false)
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
  | CI.ITP_Let_Resource (CI.ITP_sym nm, x, y) -> parensM (pp_let nm (aux x) (aux' y) true)
  | CI.ITP_Forall (CI.ITP_sym sym, bt, t) -> pp_forall sym bt (aux' t)
  | CI.ITP_Exists (CI.ITP_sym sym, bt, t) ->
    !^"∃" ^^^ parens (typ (Sym.pp sym) (bt_to_itp bt)) ^^ !^"," ^^ break 1 ^^ aux' t
  | CI.ITP_Star (t1, t2) -> mk_star (aux' t1) (aux' t2)
  | CI.ITP_Wand (t1, t2) -> mk_wand (aux' t1) (aux' t2)
  | CI.ITP_Pure t -> iris_pure (aux t)
  | CI.ITP_Define (CI.ITP_sym sym, x, y) -> map_split (pp_let sym (aux x)) (aux' y) true
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
       build
         [ rets "each_int ";
           aux min_term;
           parens (rets "Z.to_nat " ^^ parens (aux max_term) ^^ rets " - " ^^ aux min_term)
           ^^ rets "%nat";
           aux ptr;
           !^(Sym.pp_string nm);
           aux' pred
         ]
     | _ -> rets "unsupported ITP_Each_LAT perm")
  | CI.ITP_Good -> rets ""
  | CI.ITP_Unsupported_Resource msg -> rets msg


let convert_lemma_defs global (lemmas : CI.itp_lemma list) =
  let open Pp in
  let lemma_ty (CI.ITP_lemma (CI.ITP_sym nm, tm)) =
    Pp.progress_simple "converting lemma type" (Sym.pp_string nm);
    let rhs = resource_to_itp global tm in
    defn (Sym.pp_string nm ^ "_type") [] (Some (Pp.string "IProp GF")) (!^"iprop% " ^^ rhs)
  in
  let tys = List.map lemma_ty lemmas in
  tys


(* print datatypes *)
let translate_datatypes (dtys : CI.itp_dt list list) =
  let open Pp in
  let cons_line dt_tag (CI.ITP_constr (CI.ITP_sym nm, params)) =
    let argTs = List.map (fun bt -> bt_to_itp bt) params in
    !^"  | " ^^ Sym.pp nm ^^^ colon ^^^ flow !^" -> " (argTs @ [ Sym.pp dt_tag ])
  in
  let dt_eqs (CI.ITP_dt (CI.ITP_sym nm, _, constr)) =
    let c_lines = List.map (cons_line nm) constr in
    !^"inductive " ^^ Sym.pp nm ^^^ !^" where" ^^ hardline ^^ flow hardline c_lines
  in
  (* Print different header if in mutually recursive group *)
  let print_dt dty_clump =
    if List.length dty_clump > 1 then
      flow hardline [ !^"mutual" ^^ hardline ] :: List.map dt_eqs dty_clump
    else
      List.map dt_eqs dty_clump
  in
  let rec f (dtys : CI.itp_dt list list) =
    match dtys with [] -> [] | x :: xs -> print_dt x @ f xs
  in
  f dtys


(* print a line that says `open dty1 dty2...` after all datatypes are defined *)
let open_dtypes (dtys : CI.itp_dt list list) =
  let open Pp in
  let get_dt_name (CI.ITP_dt (CI.ITP_sym nm, _, _)) = Sym.pp nm in
  let dt_names = List.map get_dt_name (List.flatten dtys) in
  if List.length dt_names > 0 then
    !^"open" ^^^ flow (break 1) dt_names ^^ hardline
  else
    !^""


(* print a line that says `open pred1 pred2...` too *)
let open_predtypes (predtys : CI.itp_resource_pred_group list) =
  let open Pp in
  if List.length predtys > 0 then
    !^"open"
    ^^^ flow
          (break 1)
          (List.mapi (fun i _ -> !^"cn_predicate_group_" ^^ !^(string_of_int i)) predtys)
    ^^ hardline
  else
    !^""


(* Generates `Ptr -> arg_ty1 -> ... -> ret_ty -> IProp GF` *)
let make_pred_ty args ret_ty res_ty =
  let open Pp in
  (* add pointer arg *)
  let arg_bts = CI.ITP_Loc :: List.map snd args in
  let arg_types = List.map bt_to_itp arg_bts @ [ bt_to_itp ret_ty ] in
  List.fold_right (fun arg result -> infix 2 1 !^"->" arg result) arg_types !^res_ty


let rec scanl (f : 'b -> 'a -> 'b) (q : 'b) (ls : 'a list) =
  q :: (match ls with [] -> [] | x :: xs -> scanl f (f q x) xs)


let scanl1 f ls = match ls with x :: xs -> scanl f x xs | [] -> []

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
                 (List.map (fun y -> !^" ∧ " ^^ iris_pure (!^"¬" ^^ term_to_itp gl y)) xs)
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
  let simp t = !^"@[simp]" ^^ hardline ^^ t in
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
      let ty = make_pred_ty arg.args arg.ret_bt "IProp GF" in
      parens (typ (Sym.pp (get_pred_name arg)) ty)
    in
    let rec_args = List.map make_rec_arg group in
    rec_args @ make_formal_args pred
  in
  let get_body_name (pred : CI.itp_resource_pred) =
    Sym.pp_string (get_pred_name pred) ^ "_body"
  in
  let unpack_body (group : CI.itp_resource_pred_group) (pred : CI.itp_resource_pred) =
    simp
      (defn
         (get_body_name pred)
         (make_args group pred)
         (Some (Pp.string "IProp GF"))
         (!^"iprop% " ^^ intersperse " ∨ " "" (unpack_clauses pred.CI.clauses)))
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
      !^("inductive " ^ type_name ^ " where")
      ^^ hardline
      ^^ flow hardline (List.map (fun c -> !^"  | " ^^ c) constrs)
      ^^ hardline
    in
    let ofe_type =
      !^("abbrev " ^ type_name ^ "O := DiscreteO " ^ type_name) ^^ hardline
    in
    group_type ^^ ofe_type
  in
  let to_ofe t = !^"⟨" ^^ t ^^ !^"⟩" in
  (* In lean, `rec` is a reserved keyword so we use `rec_cn` instead *)
  (* (λ p ν, rec_cn ⟨CN_GROUP_IsForest p ν⟩) *)
  let make_closure (pred : CI.itp_resource_pred) =
    let args = make_actual_args pred in
    let call = to_ofe @@ build @@ (!^(get_constr_name pred) :: args) in
    let binders = build args in
    let body = parensM @@ build [ !^"rec_cn"; call ] in
    parensM @@ (!^"fun" ^^^ binders ^^ !^" =>") ^//^ body
  in
  (* CN_GROUP_IsForest p ν =>
      IsForest_body
        (λ p ν, (rec_cn (CN_GROUP_IsForest p ν)))
        (λ p ν, (rec_cn (CN_GROUP_IsTree p ν)))
        p ν *)
  let make_case predicates (pred : CI.itp_resource_pred) =
    let body_name = !^(get_body_name pred) in
    let args = make_actual_args pred in
    let closures = List.map make_closure predicates in
    let body = build @@ (body_name :: closures) @ args in
    let pattern = to_ofe (build @@ (!^(get_constr_name pred) :: args)) in
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
    align @@ !^"match call with" ^^ nest 2 (hardline ^^ cases) ^^ hardline
  in
  let make_pre_fixpoint_definition index (predicates : CI.itp_resource_pred_group) =
    let group_ofe_name = get_group_type_name index ^ "O" in
    let rec_type = flow !^" -> " [ !^group_ofe_name; !^"IProp GF" ] in
    let rec_arg = parens @@ typ !^"rec_cn" rec_type in
    let call_arg = parens @@ typ !^"call" !^group_ofe_name in
    simp
      (defn
         (get_pre_fixpoint_name index)
         [ rec_arg; call_arg ]
         (Some !^"IProp GF")
         (make_pre_fixpoint_body predicates))
  in
  let make_monotonicity_instance index (predicates : CI.itp_resource_pred_group) =
    let pre_fixpoint = get_pre_fixpoint_name index in
    let instance_name = get_group_type_name index ^ "_mono" in
    let instance =
      align
        (infix
           2
           1
           colon
           (!^"instance" ^^^ !^instance_name)
           (!^"BIMonoPred"
            ^^^ parens !^(pre_fixpoint ^ " (hlc := hlc) (GF := GF) ")
            ^^ !^":="))
    in
    let prepare =
      brackets
        (build
           [ intersperse "," "" @@ List.map (fun p -> !^(get_body_name p)) predicates ])
    in
    let tactic =
      prefix 2 1 (group (!^"solve_bi_mono_pred_with_prepare" ^/^ !^pre_fixpoint)) prepare
    in
    let proof = blank 2 ^^ align (!^"by " ^^ nest 2 (hardline ^^ tactic) ^^ hardline) in
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
      build [ !^"bi_least_fixpoint"; !^(get_pre_fixpoint_name index); to_ofe (build app) ]
    in
    simp (defn (Sym.pp_string (get_pred_name pred)) args (Some !^"IProp GF") body)
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
    let ty = make_pred_ty pred.args pred.ret_bt "IProp GF" in
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
      !^"theorem"
      ^^^ proof_name
      ^^ nest 4 (hardline ^^ flow hardline args)
      ^^ space
      ^^ colon
      ^^ nest 2 (hardline ^^ statement)
      ^^ hardline
      ^^ nest 2 !^" := by"
      ^^ hardline
    in
    s ^^ proof
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
             infix 2 1 !^"=>" (to_ofe body) head)
          predicates
      in
      let body = cases |> List.map (fun case -> bar ^^^ case) |> flow hardline in
      let arg1 =
        parens
        @@ align
        @@ !^"fun (call : "
        ^^ !^(get_group_type_name index ^ "O")
        ^^ !^") =>"
        ^^ nest
             2
             (hardline ^^ !^"match call with" ^^ nest 2 (hardline ^^ body) ^^ hardline)
      in
      let arg2 =
        brackets
          (build
             [ intersperse "," ""
               @@ List.map (fun p -> !^"H_" ^^ Sym.pp (get_pred_name p)) predicates
             ])
      in
      !^"  iintro "
      ^^ build (List.map (fun p -> !^"#H_" ^^ Sym.pp (get_pred_name p)) predicates)
      ^^^ hardline
      ^^^ !^" solve_cn_predicate_induction"
      ^^^ nest
            2
            (hardline ^^ flow hardline [ name ^^ !^","; arg1 ^^ !^","; arg2 ^^ hardline ])
    in
    let args = List.map (fun p -> p |> make_induction_lemma_arg |> parens) predicates in
    let statement = make_induction_lemma_statement predicates |> parens in
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
        parensM
        @@ build
        @@ ((Sym.pp (get_pred_name pred) ^^ !^" (hlc := hlc) (GF := GF) ")
            :: List.map fst args)
      in
      let body2 =
        parensM
        @@ build
        @@ (!^(get_body_name pred)
            :: List.map (fun p -> Sym.pp (get_pred_name p)) predicates)
        @ List.map fst args
      in
      infix 2 1 !^"⊣⊢" body1 body2 |> parens
    in
    let proof =
      let rewrites = List.map (fun p -> Sym.pp (get_pred_name p)) predicates in
      let rem = [ "least_fixpoint_unfold"; get_pre_fixpoint_name index ] in
      !^"  apply Iris.BI.BiEntails.of_eq"
      ^^ hardline
      ^^ !^"  rw ["
      ^^ intersperse "," "" (rewrites @ List.map ( !^ ) rem)
      ^^ !^"]"
      ^^ hardline
      ^^ !^"  rfl"
      ^^ hardline
    in
    make_lemma
      proof_name
      (List.map (fun (nm, bt) -> parens (typ nm (bt_to_itp bt))) args)
      statement
      proof
  in
  let unpack_group index (predicates : CI.itp_resource_pred_group) =
    let group_type = make_group_type index predicates in
    let pred_defs = List.map (unpack_body predicates) predicates in
    let fixpoint = make_fixpoints index predicates in
    let induction_lemma = make_induction_lemma index predicates in
    let unfold_lemmata = List.map (make_unfold_lemma index predicates) predicates in
    (group_type, pred_defs @ (fixpoint :: induction_lemma :: unfold_lemmata))
  in
  let groups = List.mapi unpack_group preds in
  (List.map fst groups, List.concat_map snd groups)


let translate_uninterp_pred =
  let open Pp in
  List.map (fun (CI.ITP_sym nm, _, args, ret_ty) ->
    let ty = make_pred_ty args ret_ty "IProp GF" in
    (!^"def" ^^^ typ (Sym.pp nm) ty ^^ !^":= sorry") ^^ hardline)


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
         defn (Sym.pp_string nm) itp_args None itp_body
       | CI.ITP_recdef body ->
         let itp_body = term_to_itp gl body in
         let itp_args =
           List.map
             (fun (CI.ITP_sym arg, bt) ->
                let itp_bt = bt_to_itp bt in
                Pp.parens (Pp.typ (Sym.pp arg) itp_bt))
             args
         in
         defn (Sym.pp_string nm) itp_args None itp_body)
    | CI.ITP_fun_uninterp (CI.ITP_sym nm, logical_fun, args, ret_typ) ->
      (match logical_fun with
       | CI.ITP_uninterp ->
         let itp_arg_typs = List.map (fun (_, bt) -> bt_to_itp bt) args in
         let itp_rt = bt_to_itp ret_typ in
         let ty =
           List.fold_right (fun at rt -> at ^^^ !^"->" ^^^ rt) itp_arg_typs itp_rt
         in
         !^"  Parameter" ^^^ typ (Sym.pp nm) ty ^^ hardline
       | CI.ITP_uninterp_prop ->
         let itp_arg_typs = List.map (fun (_, bt) -> bt_to_itp bt) args in
         let itp_rt = !^"Prop" in
         let ty =
           List.fold_right (fun at rt -> at ^^^ !^"->" ^^^ rt) itp_arg_typs itp_rt
         in
         !^"  Parameter" ^^^ typ (Sym.pp nm) ty ^^ hardline)
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
           !^("arrayshift l "
              ^ string_of_int piece.offset
              ^ " "
              ^ string_of_int piece.size)
      ^^ !^" v."
      ^^ !^(Id.get_string id)
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
    | x :: [] -> piece_to_owned x
    | x :: xs -> piece_to_owned x ^^ !^" ∗ " ^^ decl_to_pieces xs
  in
  let get_struct_field (piece : Memory.struct_piece) =
    match piece.member_or_padding with
    | Some (id, ctype) ->
      !^("  " ^ Id.get_string id ^ " : " ^ print_ctype ctype) ^^ hardline
    | None -> rets ""
  in
  let unpack_decls (decl : Sym.t * Memory.struct_layout) =
    let nm = !^(Sym.pp_string (fst decl)) in
    !^"@[ext]"
    ^^ hardline
    ^^ !^"structure "
    ^^ nm
    ^^ !^" where"
    ^^ hardline
    ^^ build (List.map get_struct_field (snd decl))
    ^^ hardline
    ^^ hardline
    ^^ !^"def "
    ^^ !^"Owned_"
    ^^ nm
    ^^ !^" (l: Ptr) (v : "
    ^^ nm
    ^^ !^") : IProp GF := "
    ^^ hardline
    ^^ !^"iprop% "
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
    let open_dtypes = open_dtypes dtys in
    let open_predtypes = open_predtypes preds in
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
    Pp.print channel (print_section "Types" "Dtypes comment" (dtypes @ pred_group_tys));
    (* open datatype/predicate namespaces *)
    Pp.print channel open_dtypes;
    Pp.print channel open_predtypes;
    (* print uninterpreted logical functions and resource predicates as parameters *)
    Pp.print channel (print_section "Params" "Params comment" (fst translated_funs));
    (* print structs and function definitions *)
    Pp.print
      channel
      (print_section
         "Defs"
         "Defs comment"
         (structs @ translated_uninterp_preds @ snd translated_funs));
    (* print resource predicates *)
    Pp.print channel (print_section "ResourcePredicates" "RPred comment" translated_preds);
    (* print function definitions *)
    (* print lemmas *)
    let translated_lemmas = convert_lemma_defs global lemmas in
    Pp.print channel (print_section "Lemma_Defs" "Lemma comment" translated_lemmas)
  in
  f
