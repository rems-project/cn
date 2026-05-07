module SBT = BaseTypes.Surface
module BT = BaseTypes
module IT = IndexTerms

type builtin_fn_def = string * Sym.t * Definition.Function.t

let loc = Cerb_location.other "<builtin>"

type message =
  | Number_arguments of
      { has : int;
        expect : int
      }
  | Array_to_list of IndexTerms.Surface.t

type err =
  { loc : Locations.t;
    msg : message
  }

let return = Result.ok

let fail = Result.error

let fail_number_args loc ~has ~expect =
  fail { loc; msg = Number_arguments { has; expect } }


(* builtin function symbols *)

let mk_arg1 mk args loc =
  match args with
  | [ x ] -> return (mk x loc)
  | xs -> fail_number_args loc ~has:(List.length xs) ~expect:1


let mk_arg2_err mk args loc =
  match args with
  | [ x; y ] -> mk (x, y) loc
  | xs -> fail_number_args loc ~has:(List.length xs) ~expect:2


let mk_arg2 mk = mk_arg2_err (fun tup loc -> return (mk tup loc))

let mk_arg3_err mk args loc =
  match args with
  | [ x; y; z ] -> mk (x, y, z) loc
  | xs -> fail_number_args loc ~has:(List.length xs) ~expect:3


let mk_arg3 mk = mk_arg3_err (fun tup loc -> return (mk tup loc))

let var_binop op ty ~left:(sym1, bt1) ~right:(sym2, bt2) =
  IT.binop op (IT.sym_ (sym1, bt1, loc), IT.sym_ (sym2, bt2, loc)) loc ty


let definition name args body =
  ( name,
    Sym.fresh name,
    Definition.Function.
      { loc; emit_coq = false; args; body = Def body; return_bt = IT.get_bt body } )


let mk_builtin_arg0 name = definition name []

let mk_builtin_arg1 name bt mk : builtin_fn_def =
  let arg = (Sym.fresh "arg", bt) in
  mk arg |> definition name [ arg ]


let mk_builtin_arg2 name (bt1, bt2) mk : builtin_fn_def =
  let left = (Sym.fresh "arg1", bt1) in
  let right = (Sym.fresh "arg2", bt2) in
  mk ~left ~right |> definition name [ left; right ]


let min_bits_def (sign, n) =
  let num, letter =
    match sign with
    | BT.Unsigned -> (Z.zero, "u")
    | Signed -> (Z.(neg @@ shift_left one (Int.sub n 1)), "i")
  in
  let name = "MIN" ^ letter ^ Int.to_string n in
  IT.num_lit_ num (BT.Bits (sign, n)) loc |> mk_builtin_arg0 name


let max_bits_def (sign, n) =
  let num, letter =
    match sign with
    | BT.Unsigned -> (Z.(shift_left one n - one), "u")
    | Signed -> (Z.(shift_left one (Int.sub n 1) - one), "i")
  in
  let name = "MAX" ^ letter ^ Int.to_string n in
  IT.num_lit_ num (BT.Bits (sign, n)) loc |> mk_builtin_arg0 name


let max_min_bits =
  let sizes = [ 8; 16; 32; 64 ] in
  let signs = [ BT.Unsigned; Signed ] in
  List.fold_left
    (fun acc sign ->
       List.fold_left
         (fun acc size -> max_bits_def (sign, size) :: min_bits_def (sign, size) :: acc)
         acc
         sizes)
    []
    signs


let not_def =
  mk_builtin_arg1 "not" BT.Bool (fun (sym, bt) -> IT.not_ (IT.sym_ (sym, bt, loc)) loc)


let is_null_def : builtin_fn_def =
  mk_builtin_arg1 "is_null" (BT.Loc ()) (fun (sym, bt) ->
    (IT.eq__ (IT.sym_ (sym, bt, loc)) (IT.null_ loc)) loc)


(* Cannot translate this to a logical function until the TODO in `cn_to_ail_expr_aux_internal` in `cn_internal_to_ail.ml` is resolved*)
let has_alloc_id_def =
  ( "has_alloc_id",
    Sym.fresh "has_alloc_id",
    mk_arg1 (fun p loc' -> IT.Surface.inj @@ IT.hasAllocId_ (IT.Surface.proj p) loc') )


let ptr_eq_def : builtin_fn_def =
  var_binop EQ BT.Bool |> mk_builtin_arg2 "ptr_eq" (BT.Loc (), BT.Loc ())


let prov_eq_def : builtin_fn_def =
  let left = (Sym.fresh "arg1", BT.Loc ()) in
  let right = (Sym.fresh "arg2", BT.Loc ()) in
  let left_cast = IT.allocId_ (IT.sym_ (fst left, BT.Loc (), loc)) loc in
  let right_cast = IT.allocId_ (IT.sym_ (fst right, BT.Loc (), loc)) loc in
  let body = IT.binop EQ (left_cast, right_cast) loc BT.Bool in
  definition "prov_eq" [ left; right ] body


let addr_eq_def : builtin_fn_def =
  let left = (Sym.fresh "arg1", BT.Loc ()) in
  let right = (Sym.fresh "arg2", BT.Loc ()) in
  let left_cast = IT.addr_ (IT.sym_ (fst left, BT.Loc (), loc)) loc in
  let right_cast = IT.addr_ (IT.sym_ (fst right, BT.Loc (), loc)) loc in
  let body = IT.binop EQ (left_cast, right_cast) loc BT.Bool in
  definition "addr_eq" [ left; right ] body


(* The remaining functions in this file, from here until array_to_list_def cannot yet be translated to
   LogicalFunction.definition types because they implicitly require basetype polymorphism.
   For example, the `mod` function allows inputs of any sign and size, but such a function cannot be defined
   yet with an index term *)
let mul_uf_def =
  ( "mul_uf",
    Sym.fresh "mul_uf",
    mk_arg2 (fun (it, it') loc -> IT.binop MulNoSMT (it, it') loc (IT.get_bt it)) )


let div_uf_def =
  ( "div_uf",
    Sym.fresh "div_uf",
    mk_arg2 (fun (it, it') loc -> IT.binop DivNoSMT (it, it') loc (IT.get_bt it)) )


let power_uf_def =
  ( "power_uf",
    Sym.fresh "power_uf",
    mk_arg2 (fun (it, it') loc -> IT.binop ExpNoSMT (it, it') loc (IT.get_bt it)) )


let rem_uf_def =
  ( "rem_uf",
    Sym.fresh "rem_uf",
    mk_arg2 (fun (it, it') loc -> IT.binop RemNoSMT (it, it') loc (IT.get_bt it)) )


let mod_uf_def =
  ( "mod_uf",
    Sym.fresh "mod_uf",
    mk_arg2 (fun (it, it') loc -> IT.binop ModNoSMT (it, it') loc (IT.get_bt it)) )


let xor_uf_def =
  ( "xor_uf",
    Sym.fresh "xor_uf",
    mk_arg2 (fun (it, it') loc -> IT.binop BW_Xor (it, it') loc (IT.get_bt it)) )


let bw_and_uf_def =
  ( "bw_and_uf",
    Sym.fresh "bw_and_uf",
    mk_arg2 (fun (it, it') loc -> IT.binop BW_And (it, it') loc (IT.get_bt it)) )


let bw_or_uf_def =
  ( "bw_or_uf",
    Sym.fresh "bw_or_uf",
    mk_arg2 (fun (it, it') loc -> IT.binop BW_Or (it, it') loc (IT.get_bt it)) )


let bw_clz_uf_def =
  ("bw_clz_uf", Sym.fresh "bw_clz_uf", mk_arg1 (IT.arith_unop BW_CLZ_NoSMT))


let bw_ctz_uf_def =
  ("bw_ctz_uf", Sym.fresh "bw_ctz_uf", mk_arg1 (IT.arith_unop BW_CTZ_NoSMT))


let bw_ffs_uf_def =
  ("bw_ffs_uf", Sym.fresh "bw_ffs_uf", mk_arg1 (IT.arith_unop BW_FFS_NoSMT))


let bw_fls_uf_def =
  ("bw_fls_uf", Sym.fresh "bw_fls_uf", mk_arg1 (IT.arith_unop BW_FLS_NoSMT))


let shift_left_def =
  ( "shift_left",
    Sym.fresh "shift_left",
    mk_arg2 (fun (it, it') loc -> IT.binop ShiftLeft (it, it') loc (IT.get_bt it)) )


let shift_right_def =
  ( "shift_right",
    Sym.fresh "shift_right",
    mk_arg2 (fun (it, it') loc -> IT.binop ShiftRight (it, it') loc (IT.get_bt it)) )


let power_def =
  ( "power",
    Sym.fresh "power",
    mk_arg2 (fun (it, it') loc -> IT.binop Exp (it, it') loc (IT.get_bt it)) )


let rem_def =
  ( "rem",
    Sym.fresh "rem",
    mk_arg2 (fun (it, it') loc -> IT.binop Rem (it, it') loc (IT.get_bt it)) )


let mod_def =
  ( "mod",
    Sym.fresh "mod",
    mk_arg2 (fun (it, it') loc -> IT.binop Mod (it, it') loc (IT.get_bt it)) )


let is_some_def = ("is_some", Sym.fresh "is_some", mk_arg1 IT.isSome_)

let is_none_def = ("is_none", Sym.fresh "is_none", mk_arg1 IT.isNone_)

let get_opt_def = ("get_opt", Sym.fresh "get_opt", mk_arg1 IT.getOpt_)

let builtin_funs
  : (string
    * Sym.t
    * (BaseTypes.Surface.t IT.annot list ->
      Locations.t ->
      (BaseTypes.Surface.t IT.annot, err) result))
      list
  =
  [ mul_uf_def;
    div_uf_def;
    power_uf_def;
    rem_uf_def;
    mod_uf_def;
    xor_uf_def;
    bw_and_uf_def;
    bw_or_uf_def;
    bw_clz_uf_def;
    bw_ctz_uf_def;
    bw_ffs_uf_def;
    bw_fls_uf_def;
    shift_left_def;
    shift_right_def;
    power_def;
    rem_def;
    mod_def;
    has_alloc_id_def;
    is_some_def;
    is_none_def;
    get_opt_def
  ]


let builtin_fun_defs =
  max_min_bits @ [ not_def; is_null_def; ptr_eq_def; prov_eq_def; addr_eq_def ]


let apply_builtin_funs fsym args loc =
  match List.find_opt (fun (_, fsym', _) -> Sym.equal fsym fsym') builtin_funs with
  | None -> return None
  | Some (_, _, mk) -> Result.bind (mk args loc) (fun t -> return (Some t))


let apply_builtin_fun_defs fsym args _loc =
  match List.find_opt (fun (_, fsym', _) -> Sym.equal fsym fsym') builtin_fun_defs with
  | None -> None
  | Some (_, _, fn_def) ->
    let body =
      match fn_def.Definition.Function.body with
      | Definition.Function.Def body -> body
      | Definition.Function.Rec_Def body -> body
      | Definition.Function.Uninterp -> failwith "Builtin function must have a body"
    in
    let formal_args = fn_def.Definition.Function.args in
    (* Create list of (formal_sym, actual_arg) pairs *)
    let subst_list =
      List.map2
        (fun (formal_sym, _) actual_arg -> (formal_sym, actual_arg))
        formal_args
        args
    in
    (* Create and apply substitution *)
    let subst = IT.make_subst subst_list in
    Some (IT.subst subst body)


(* This list of names is later passed to the frontend in bin/main.ml so that
 * these are available in the elaboration, so it should include all builtin
 * function names *)
let fun_names =
  List.map (fun (str, sym, _) -> (str, sym)) builtin_funs
  @ List.map (fun (str, sym, _) -> (str, sym)) builtin_fun_defs
