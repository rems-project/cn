module BT = BaseTypes
open Terms
open Terms.Normal

(* shorthands *)

let use_vip = ref true

(* lit *)
let sym_ (sym, bt, loc) = IT (Sym sym, bt, loc)

let z_ n loc = IT (Const (Z n), BT.Integer, loc)

let alloc_id_ n loc = IT (Const (Alloc_id n), BT.Alloc_id, loc)

let num_lit_ n bt loc =
  match bt with
  | BT.Bits (sign, sz) ->
    assert (BT.fits_range (sign, sz) n);
    IT (Const (Bits ((sign, sz), n)), bt, loc)
  | BT.Integer -> z_ n loc
  | _ -> failwith ("num_lit_: not a type with numeric literals: " ^ Pp.plain (BT.pp bt))


let q_ (n, n') loc = IT (Const (Q (Q.make (Z.of_int n) (Z.of_int n'))), BT.Real, loc)

let q1_ q loc = IT (Const (Q q), BT.Real, loc)

let pointer_ ~alloc_id ~addr loc =
  let alloc_id = if !use_vip then alloc_id else Z.zero in
  IT (Const (Pointer { alloc_id; addr }), BT.Loc (), loc)


let bool_ b loc = IT (Const (Bool b), BT.Bool, loc)

let unit_ loc = IT (Const Unit, BT.Unit, loc)

let int_ n loc = z_ (Z.of_int n) loc

let int_lit_ n bt loc = num_lit_ (Z.of_int n) bt loc

let default_ bt loc = IT (Const (Default bt), bt, loc)

let const_ctype_ ct loc = IT (Const (CType_const ct), BT.CType, loc)

(* cmp_op *)
let lt_ (it, it') loc =
  if BT.equal (get_bt it) (get_bt it') then
    ()
  else
    failwith ("lt_: type mismatch: " ^ Pp.plain (Pp.list pp_with_typ [ it; it' ]));
  IT (Binop (LT, it, it'), BT.Bool, loc)


let le_ (it, it') loc =
  if BT.equal (get_bt it) (get_bt it') then
    ()
  else
    failwith ("le_: type mismatch: " ^ Pp.plain (Pp.list pp_with_typ [ it; it' ]));
  IT (Binop (LE, it, it'), BT.Bool, loc)


let gt_ (it, it') = lt_ (it', it)

let ge_ (it, it') = le_ (it', it)

(* bool_op *)
let vargs_binop basevalue binop = function
  | [] -> basevalue
  | it :: its -> List.fold_left binop it its


let and2_ (it, it') loc = IT (Binop (And, it, it'), BT.Bool, loc)

let or2_ (it, it') loc = IT (Binop (Or, it, it'), BT.Bool, loc)

let and_ its loc = vargs_binop (bool_ true loc) (Tools.curry (fun p -> and2_ p loc)) its

let or_ its loc = vargs_binop (bool_ false loc) (Tools.curry (fun p -> or2_ p loc)) its

let impl_ (it, it') loc = IT (Binop (Implies, it, it'), BT.Bool, loc)

let not_ it loc = IT (Unop (Not, it), get_bt it, loc)

let bw_compl_ it loc = IT (Unop (BW_Compl, it), get_bt it, loc)

let ite_ (it, it', it'') loc = IT (ITE (it, it', it''), get_bt it', loc)

let eq_ (it, it') loc =
  if BT.equal (get_bt it) (get_bt it') then
    ()
  else
    failwith ("eq_: type mismatch: " ^ Pp.plain (Pp.list pp_with_typ [ it; it' ]));
  IT (Binop (EQ, it, it'), BT.Bool, loc)


let eq__ it it' = eq_ (it, it')

let ne_ (it, it') loc = not_ (eq_ (it, it') loc) loc

let ne__ it it' = ne_ (it, it')

let let_ ((nm, x), y) loc = IT (Let ((nm, x), y), get_bt y, loc)

let eachI_ (i1, (s, bt), i2) t loc = IT (EachI ((i1, (s, bt), i2), t), BT.Bool, loc)

(* arith_op *)
let negate it loc = IT (Unop (Negate, it), get_bt it, loc)

let arith_binop op (it, it') loc =
  assert (BT.equal (get_bt it) (get_bt it'));
  assert (match get_bt it with Integer | Real | Bits _ -> true | _ -> false);
  IT (Binop (op, it, it'), get_bt it, loc)


let add_ = arith_binop Add

let sub_ = arith_binop Sub

let mul_ = arith_binop Mul

let mul_no_smt_ = arith_binop MulNoSMT

let div_ = arith_binop Div

let div_no_smt_ = arith_binop DivNoSMT

let exp_ = arith_binop Exp

let exp_no_smt_ = arith_binop ExpNoSMT

let rem_ = arith_binop Rem

let rem_no_smt_ = arith_binop RemNoSMT

let mod_ = arith_binop Mod

let mod_no_smt_ = arith_binop ModNoSMT

let divisible_ (it, it') loc = eq_ (mod_ (it, it') loc, int_lit_ 0 (get_bt it) loc) loc

let rem_f_ (it, it') loc = mod_ (it, it') loc

let min_ = arith_binop Min

let max_ = arith_binop Max

let binop op (it, it') loc ret = IT (Binop (op, it, it'), ret, loc)

let arith_unop op it loc = IT (Unop (op, it), get_bt it, loc)

(* tuple_op *)
let tuple_ its loc = IT (Tuple its, BT.Tuple (List.map get_bt its), loc)

let nthTuple_ ~item_bt (n, it) loc = IT (NthTuple (n, it), item_bt, loc)

(* struct_op *)
let struct_ (tag, members) loc = IT (Struct (tag, members), BT.Struct tag, loc)

let member_ ~member_bt (it, member) loc = IT (StructMember (it, member), member_bt, loc)

let record_ members loc =
  let sorted_members =
    List.sort_uniq (fun (id1, _) (id2, _) -> Id.compare id1 id2) members
  in
  if List.length sorted_members <> List.length members then
    failwith
      ("Record members are not allowed to include duplicates: "
       ^ String.concat
           ", "
           (members |> List.map fst |> List.map Id.pp |> List.map Pp.plain));
  IT
    ( Record sorted_members,
      BT.Record (List.map (fun (s, t) -> (s, get_bt t)) sorted_members),
      loc )


let recordMember_ ~member_bt (t, member) loc =
  IT (RecordMember (t, member), member_bt, loc)


(* pointer_op *)
let null_ loc = IT (Const Null, BT.Loc (), loc)

let ltPointer_ (it, it') loc = IT (Binop (LTPointer, it, it'), BT.Bool, loc)

let lePointer_ (it, it') loc = IT (Binop (LEPointer, it, it'), BT.Bool, loc)

let gtPointer_ (it, it') loc = ltPointer_ (it', it) loc

let gePointer_ (it, it') loc = lePointer_ (it', it) loc

let cast_ bt' it loc =
  if BT.equal bt' (get_bt it) then it else IT (Cast (bt', it), bt', loc)


(* let uintptr_const_ n loc = num_lit_ n Memory.uintptr_bt loc *)

(* let uintptr_int_ n loc = uintptr_const_ (Z.of_int n) loc *)
(* for integer-mode: z_ n *)

let addr_ it loc =
  assert (BT.equal (get_bt it) (Loc ()));
  cast_ Memory.uintptr_bt it loc


let upper_bound addr ct loc =
  let range_size =
    let size = Memory.size_of_ctype ct in
    num_lit_ (Z.of_int size) Memory.uintptr_bt loc
  in
  assert (BT.equal (get_bt addr) (get_bt range_size));
  add_ (addr, range_size) loc


(* for integer-mode: cast_ Integer it *)

let allocId_ it loc = cast_ Alloc_id it loc

let memberShift_ (base, tag, member) loc =
  IT (MemberShift (base, tag, member), BT.Loc (), loc)


let right_integer_type_for_mode bt =
  let open BT in
  match bt with Bits _ -> !cnBV | Integer -> not !cnBV | _ -> assert false


(* invariant of ArrayShift: index must have type uintptr_bt *)
(* TODO: some call sites explicitly put the uintptr_bt cast *)
let arrayShift_ ~base ~index ct loc =
  assert (right_integer_type_for_mode (get_bt index));
  let index = cast_ Memory.uintptr_bt index loc in
  IT (ArrayShift { base; ct; index }, BT.Loc (), loc)


let copyAllocId_ ~addr ~loc:ptr loc =
  assert (right_integer_type_for_mode (get_bt addr));
  IT (CopyAllocId { addr; loc = ptr }, BT.Loc (), loc)


let hasAllocId_ ptr loc =
  (* Futzing seems to be necessary because given the current SMT sovler mapping,
     the solver can't conclude `has_alloc_id(&p[x]) ==> has_alloc_id(p)` and
     similarly for &p->x. This may be avoidable with a different solver mapping. *)
  let rec futz = function
    | IT ((MemberShift (base, _, _) | ArrayShift { base; _ }), _, _) -> futz base
    | it -> it
  in
  IT (HasAllocId (futz ptr), BT.Bool, loc)


let sizeOf_ ct loc = IT (SizeOf ct, Memory.size_bt, loc)

let isIntegerToPointerCast = function
  | IT (Cast (BT.Loc (), IT (_, BT.Integer, _)), _, _) -> true
  | IT (Cast (BT.Loc (), IT (_, BT.Bits _, _)), _, _) -> true
  | _ -> false


let pointer_offset_ (base, offset) loc =
  arrayShift_ ~base (Sctypes.Integer Char) ~index:offset loc


(* list_op *)
let nil_ ~item_bt loc = IT (Nil item_bt, BT.List item_bt, loc)

let cons_ (it, it') loc = IT (Cons (it, it'), get_bt it', loc)

let list_ ~item_bt its ~nil_loc =
  let rec aux = function
    | [] -> IT (Nil item_bt, BT.List item_bt, nil_loc)
    | x :: xs -> IT (Cons (x, aux xs), BT.List item_bt, get_loc x)
  in
  aux its


let head_ ~item_bt it loc = IT (Head it, item_bt, loc)

let tail_ it loc = IT (Tail it, get_bt it, loc)

let rec dest_list it =
  match get_term it with
  | Nil _bt -> Option.Some []
  | Cons (x, xs) -> Option.map (fun ys -> x :: ys) (dest_list xs)
  (* TODO: maybe include Tail, if we ever actually use it? *)
  | _ -> None


(* ct_pred *)
let representable_ (t, it) loc = IT (Representable (t, it), BT.Bool, loc)

let good_ (sct, it) loc = IT (Good (sct, it), BT.Bool, loc)

let wrapI_ (ity, arg) loc =
  assert (right_integer_type_for_mode (get_bt arg));
  IT (WrapI (ity, arg), Memory.bt_of_sct (Sctypes.Integer ity), loc)


let alignedI_ ~t ~align loc =
  assert (BT.equal (get_bt t) (Loc ()));
  assert (BT.equal Memory.uintptr_bt (get_bt align));
  IT (Aligned { t; align }, BT.Bool, loc)


let aligned_ (t, ct) loc =
  alignedI_ ~t ~align:(int_lit_ (Memory.align_of_ctype ct) Memory.uintptr_bt loc) loc


let const_map_ index_bt t loc =
  IT (MapConst (index_bt, t), BT.Map (index_bt, get_bt t), loc)


let map_set_ t1 (t2, t3) loc = IT (MapSet (t1, t2, t3), get_bt t1, loc)

let map_get_ v arg loc =
  match get_bt v with
  | BT.Map (dt, rbt) ->
    if BT.equal dt (get_bt arg) then
      ()
    else
      failwith ("mag_get_: type mismatch: " ^ Pp.plain (Pp.list pp_with_typ [ v; arg ]));
    IT (MapGet (v, arg), rbt, loc)
  | _ -> Cerb_debug.error "illtyped index term"


let none_ bt loc = IT (CN_None bt, BT.Option bt, loc)

let some_ t loc = IT (CN_Some t, BT.Option (get_bt t), loc)

let isNone_ t loc =
  match get_bt t with
  | BT.Option _ -> IT (Unop (Not, IT (IsSome t, Bool, loc)), Bool, loc)
  | _ -> Cerb_debug.error "illtyped index term"


let isSome_ t loc =
  match get_bt t with
  | BT.Option _ -> IT (IsSome t, Bool, loc)
  | _ -> Cerb_debug.error "illtyped index term"


let getOpt_ t loc =
  match get_bt t with
  | BT.Option bt -> IT (GetOpt t, bt, loc)
  | _ -> Cerb_debug.error "illtyped index term"


let make_array_ ~index_bt ~item_bt items (* assumed all of item_bt *) loc =
  let base_value = const_map_ index_bt (default_ item_bt loc) loc in
  let _, value =
    List.fold_left
      (fun (index, value) item ->
         let index_it = num_lit_ (Z.of_int index) index_bt loc in
         (index + 1, map_set_ value (index_it, item) loc))
      (0, base_value)
      items
  in
  value


let apply_ name args rbt loc = IT (Apply (name, args), rbt, loc)

let fresh_anon bt loc =
  let symbol = Sym.fresh_anon () in
  (symbol, sym_ (symbol, bt, loc))


let fresh_named bt name loc =
  let symbol = Sym.fresh name in
  (symbol, sym_ (symbol, bt, loc))


let fresh_same bt symbol' loc =
  let symbol = Sym.fresh_same symbol' in
  (symbol, sym_ (symbol, bt, loc))


let def_ sym e loc = eq_ (sym_ (sym, get_bt e, loc), e) loc

let in_range within (min, max) loc =
  and_ [ le_ (min, within) loc; le_ (within, max) loc ] loc


let rec in_z_range within (min_z, max_z) loc =
  let the_bt = get_bt within in
  match the_bt with
  | BT.Integer -> in_range within (z_ min_z loc, z_ max_z loc) loc
  | BT.Bits (sign, sz) ->
    let min_possible, max_possible = BT.bits_range (sign, sz) in
    let min_c =
      if Z.leq min_z min_possible then
        bool_ true loc
      else if Z.leq min_z max_possible then
        le_ (num_lit_ min_z the_bt loc, within) loc
      else
        bool_ false loc
    in
    let max_c =
      if Z.leq max_possible max_z then
        bool_ true loc
      else if Z.leq min_possible max_z then
        le_ (within, num_lit_ max_z the_bt loc) loc
      else
        bool_ false loc
    in
    and_ [ min_c; max_c ] loc
  | Loc () ->
    (* §6.3.2.3#6 allows converting pointers to any integer type so long as the value of
       the pointer fits. If uintptr_t and intptr_t exist, then they are guaranteed to be
       big enough to fit any valid pointer (to void). From there, it's just a matter of
       checking the bits fit. *)
    or_
      [ in_z_range (cast_ Memory.uintptr_bt within loc) (min_z, max_z) loc;
        in_z_range (cast_ Memory.intptr_bt within loc) (min_z, max_z) loc
      ]
      loc
  | _ -> failwith ("in_z_range: unsupported type: " ^ Pp.plain (pp_with_typ within))


let const_of_c_sig (c_sig : Sctypes.c_concrete_sig) loc =
  (* ideally the ctypes would have location information attached *)
  let open Option in
  let@ ret_ct = Sctypes.of_ctype c_sig.sig_return_ty in
  let@ arg_cts = ListM.mapM Sctypes.of_ctype c_sig.sig_arg_tys in
  let arg_cts = List.map (Fun.flip const_ctype_ loc) arg_cts in
  let arg_v = list_ ~item_bt:BT.CType arg_cts ~nil_loc:loc in
  return
    (tuple_
       [ const_ctype_ ret_ct loc;
         arg_v;
         bool_ c_sig.sig_variadic loc;
         bool_ c_sig.sig_has_proto loc
       ]
       loc)


(* let _non_vip_constraint about loc =  *)
(*   eq_ (allocId_ about loc, alloc_id_ Z.zero loc) loc *)

(* TODO: are the constraints `0<about` and `about+pointee_size-1 <= max-pointer`
   required? *)
let value_check_pointer mode ~pointee_ct about loc =
  let good = match mode with `Good -> true | `Representable -> false in
  and_
    (List.concat
       [ (* if !use_vip then None else Some (non_vip_constraint about loc); *)
         (if BT.(!cnBV) then
            []
          else
            [ in_z_range (addr_ about loc) (Z.zero, Memory.max_pointer) loc ]);
         (if good then [ aligned_ (about, pointee_ct) loc ] else [])
       ])
    loc


let value_check_array_size_warning = ref 100

let value_check mode (struct_layouts : Memory.struct_decls) ct about loc =
  let open Sctypes in
  let open Memory in
  let rec aux (ct_ : Sctypes.t) about =
    match ct_ with
    | Void -> bool_ true loc
    | Byte -> if BT.(!cnBV) then bool_ true loc else failwith "todo: Byte value_check"
    | Integer it ->
      in_z_range about (Memory.min_integer_type it, Memory.max_integer_type it) loc
    | Array (_, None) ->
      Cerb_debug.error "todo: 'representable' for arrays with unknown length"
    | Array (item_ct, Some n) ->
      if n > !value_check_array_size_warning then (
        (* FIXME: handle this better rather than just warning *)
        Pp.warn
          loc
          (Pp.string ("good/value_check of array of large size: " ^ Int.to_string n));
        value_check_array_size_warning := n)
      else
        ();
      let ix_bt =
        match BT.is_map_bt (get_bt about) with
        | Some (abt, _) -> abt
        | _ ->
          failwith ("value_check: argument not a map: " ^ Pp.plain (pp_with_typ about))
      in
      let () =
        if BT.equal ix_bt Memory.uintptr_bt then
          ()
        else
          Pp.warn
            (Locations.other __LOC__)
            (Pp.item "unexpected type of array arg" (pp_with_typ about))
      in
      let i_s, i = fresh_named ix_bt "i" loc in
      eachI_ (0, (i_s, ix_bt), n - 1) (aux item_ct (map_get_ about i loc)) loc
    | Pointer pointee_ct -> value_check_pointer mode ~pointee_ct about loc
    | Struct tag ->
      and_
        (List.filter_map
           (fun piece ->
              match piece.member_or_padding with
              | Some (member, mct) ->
                let member_bt = Memory.bt_of_sct mct in
                let member_it = member_ ~member_bt (about, member) loc in
                Some (aux mct member_it)
              | None -> None)
           (Sym.Map.find tag struct_layouts))
        loc
    | Function _ -> Cerb_debug.error "todo: function types"
  in
  aux ct about


let good_value = value_check `Good

let representable = value_check `Representable

let good_pointer = value_check_pointer `Good
