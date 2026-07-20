module MT = MakeTerm
module BT = BaseTypes
module LRT = LogicalReturnTypes
module RT = ReturnTypes
module AT = ArgumentTypes
module LAT = LogicalArgumentTypes
module TE = TypeErrors
module Loc = Locations
module LF = Definition.Function
module LC = LogicalConstraints
module IdSet = Set.Make (Id)
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
module CI = Coq_ir

let rec pat_to_coq_ir pat =
  match pat with
  | Terms.Pat (Terms.PSym sym, _, _) -> CI.Coq_pSym (CI.Coq_sym sym)
  | Terms.Pat (Terms.PWild, _, _) -> CI.Coq_pWild
  | Terms.Pat (Terms.PConstructor (c_nm, id_ps), _, _) ->
    CI.Coq_pConstructor (CI.Coq_sym c_nm, List.map (fun x -> pat_to_coq_ir (snd x)) id_ps)


(* assuming here that the id's are in canonical order *)

(* set of functions with boolean return type that we want to use
   as toplevel propositions, i.e. return Prop rather than bool
   (computational) in Coq. *)
let prop_funs = StringSet.of_list [ "page_group_ok" ]

let fun_prop_ret (global : Global.t) nm =
  match Sym.Map.find_opt nm global.logical_functions with
  (* todo: the None case shouldn't be possible since the CN code must be well-formed *)
  | None -> true
  | Some def ->
    let open Definition.Function in
    BaseTypes.equal BaseTypes.Bool def.return_bt
    && StringSet.mem (Sym.pp_string nm) prop_funs


let struct_layout_field_bts xs =
  let open Memory in
  let xs2 =
    List.filter (fun x -> Option.is_some x.member_or_padding) xs
    |> List.sort (fun (x : struct_piece) y -> Int.compare x.offset y.offset)
    |> List.filter_map (fun x -> x.member_or_padding)
  in
  (List.map fst xs2, List.map (fun x -> Memory.bt_of_sct (snd x)) xs2)


let get_struct_xs struct_decls tag =
  match Sym.Map.find_opt tag struct_decls with
  | Some def -> struct_layout_field_bts def
  (* the none case should be impossible *)
  | None -> ([], [])


(* CN basetypes to Coq_IR *)
let rec bt_to_coq_ir (gl : Global.t) (bt : BT.t) =
  match bt with
  | BaseTypes.Bool -> CI.Coq_Bool
  | BaseTypes.Integer -> CI.Coq_Integer
  | BaseTypes.Bits (sign, i) ->
    (match sign with
     | BT.Signed -> CI.Coq_Bits (CI.Coq_Signed, i)
     | BT.Unsigned -> CI.Coq_Bits (CI.Coq_Unsigned, i))
  | BaseTypes.Map (x, y) ->
    let enc_x = bt_to_coq_ir gl x in
    let enc_y = bt_to_coq_ir gl y in
    CI.Coq_Map (enc_x, enc_y)
  | BaseTypes.Struct tag ->
    let _, fld_bts = get_struct_xs gl.struct_decls tag in
    let enc_fld_bts = List.map (bt_to_coq_ir gl) fld_bts in
    CI.Coq_Struct (CI.Coq_sym tag, enc_fld_bts)
  | BaseTypes.Record mems ->
    let enc_mem_bts = List.map (bt_to_coq_ir gl) (List.map snd mems) in
    CI.Coq_Record enc_mem_bts
  | BaseTypes.Loc () -> CI.Coq_Loc
  (* todo: probably not right *)
  | BaseTypes.Datatype tag -> CI.Coq_Datatype (CI.Coq_sym tag)
  | BaseTypes.List _bt2 -> CI.Coq_List (bt_to_coq_ir gl bt)
  | BaseTypes.Unit -> CI.Coq_Unit
  | BaseTypes.MemByte -> CI.Coq_Membyte
  | BaseTypes.Real -> CI.Coq_Real
  | BaseTypes.Alloc_id -> CI.Coq_Alloc_id
  | BaseTypes.CType -> CI.Coq_CType
  | BaseTypes.Tuple bts -> CI.Coq_Tuple (List.map (bt_to_coq_ir gl) bts)
  | BaseTypes.Set _ -> CI.Coq_Set (bt_to_coq_ir gl bt)
  | BaseTypes.Option _ -> failwith "unsupported" (* TODO(HK): added just for plumbing *)


(* map each mutually recursive list of datatypes to mutually recursive lists of coq_ir *)
let dt_to_coq_ir (gl : Global.t) nm =
  (* find the info of a datatype *)
  let dt_info = Sym.Map.find nm gl.datatypes in
  (* get its params and translate them*)
  let dt_args = List.map (fun (_, bt) -> bt_to_coq_ir gl bt) dt_info.all_params in
  (* find the info of a constructor, given its name*)
  let constr_info constr = Sym.Map.find constr gl.datatype_constrs in
  (* get the argument types of the constructor *)
  let constr_argTs constrs_nms =
    List.map (fun (_, bt) -> bt_to_coq_ir gl bt) constrs_nms
  in
  let constrs =
    List.map
      (fun c -> CI.Coq_constr (CI.Coq_sym c, constr_argTs (constr_info c).params))
      dt_info.constrs
  in
  CI.Coq_dt (CI.Coq_sym nm, dt_args, constrs)


let dtypes_to_coq_ir (gl : Global.t) (dtyps : Sym.t list list) =
  (* translate one particular clump of mutually recursive definitoins *)
  let dtype_clump_to_coq_ir (gl : Global.t) (nms : Sym.t list) =
    List.map (dt_to_coq_ir gl) nms
  in
  (* wrap them all together into a big list of lists *)
  List.map (dtype_clump_to_coq_ir gl) dtyps


let find_tuple_element (eq : 'a -> 'a -> bool) (x : 'a) (ys : 'a list) =
  let n_ys = List.mapi (fun i y -> (i, y)) ys in
  match List.find_opt (fun (_i, y) -> eq x y) n_ys with
  | None -> (0, 0)
  | Some (i, _) -> (i, List.length ys)


(* translate index terms to coq_ir *)
let it_to_coq_ir global it b =
  let rec f comp_bool it =
    let aux t = f comp_bool t in
    let enc_prop = Option.is_none comp_bool in
    let with_is_true =
      enc_prop && BaseTypes.equal (Terms.Normal.get_bt it) BaseTypes.Bool
    in
    let bt = bt_to_coq_ir global (Terms.Normal.get_bt it) in
    match Terms.Normal.get_term it with
    | Terms.Sym s -> CI.Coq_sym_term (CI.Coq_sym s)
    | Terms.Const l ->
      (match l with
       | Terms.Bool b ->
         if with_is_true then
           CI.Coq_const (CI.Coq_bool_prop b)
         else
           CI.Coq_const (CI.Coq_bool b)
       | Terms.Z z -> CI.Coq_const (CI.Coq_Z z)
       | Terms.Bits (info, z) -> CI.Coq_const (CI.Coq_bits (BT.normalise_to_range info z))
       | Terms.Q _ -> CI.Coq_unsupported "Unsupported const Q"
       | Terms.MemByte _ -> CI.Coq_unsupported "Unsupported const membyte"
       | Terms.Pointer _ -> CI.Coq_unsupported "Unsupported const pointer"
       | Terms.Alloc_id _ -> CI.Coq_unsupported "Unsupported const alloc_id"
       | Terms.Unit -> CI.Coq_unsupported "Unsupported const unit"
       | Terms.Null -> CI.Coq_const (CI.Coq_Z Z.zero)
       | Terms.CType_const _ -> CI.Coq_unsupported "Unsupported const ctype"
       | Terms.Default _ -> CI.Coq_unsupported "Unsupported const default")
    | Terms.Unop (op, a) ->
      let x = aux a in
      (match op with
       | Terms.Not ->
         if enc_prop then
           CI.Coq_unop (CI.Coq_neg_prop, x, bt)
         else
           CI.Coq_unop (CI.Coq_neg, x, bt)
       | Terms.BW_FFS_NoSMT -> CI.Coq_unop (CI.Coq_BW_FFS_NoSMT, x, bt)
       | Terms.BW_CTZ_NoSMT -> CI.Coq_unop (CI.Coq_BW_CTZ_NoSMT, x, bt)
       | _ -> CI.Coq_unsupported "Unsupported unop")
    | Terms.Binop (op, a, b) ->
      let x = aux a in
      let y = aux b in
      (match op with
       | Add -> CI.Coq_binop (CI.Coq_add, x, y, bt)
       | Sub -> CI.Coq_binop (CI.Coq_sub, x, y, bt)
       | Mul -> CI.Coq_binop (CI.Coq_mul, x, y, bt)
       | MulNoSMT -> CI.Coq_binop (CI.Coq_mul, x, y, bt)
       | Div -> CI.Coq_binop (CI.Coq_div, x, y, bt)
       | DivNoSMT -> CI.Coq_binop (CI.Coq_div, x, y, bt)
       | Mod -> CI.Coq_binop (CI.Coq_mod, x, y, bt)
       | ModNoSMT -> CI.Coq_binop (CI.Coq_mod, x, y, bt)
       (* TODO: this can't be right: mod and rem aren't the same
      - maybe they have the same semantics as Coq Z.modulo/Z.rem *)
       | Rem -> CI.Coq_binop (CI.Coq_rem, x, y, bt)
       | RemNoSMT -> CI.Coq_binop (CI.Coq_mod, x, y, bt)
       | LT ->
         if enc_prop then
           CI.Coq_binop (CI.Coq_lt_prop, x, y, bt)
         else
           CI.Coq_binop (CI.Coq_lt, x, y, bt)
       | LE ->
         if enc_prop then
           CI.Coq_binop (CI.Coq_le_prop, x, y, bt)
         else
           CI.Coq_binop (CI.Coq_le, x, y, bt)
       | Exp -> CI.Coq_binop (CI.Coq_exp, x, y, bt)
       | ExpNoSMT -> CI.Coq_binop (CI.Coq_exp, x, y, bt)
       | BW_Xor -> CI.Coq_binop (CI.Coq_bwxor, x, y, bt)
       | BW_And -> CI.Coq_binop (CI.Coq_bwand, x, y, bt)
       | BW_Or -> CI.Coq_binop (CI.Coq_bwor, x, y, bt)
       | EQ ->
         let comp = Some (it, "argument of equality") in
         if enc_prop then
           CI.Coq_binop (CI.Coq_eq_prop, f comp a, f comp b, bt)
         else
           CI.Coq_binop (CI.Coq_eq, f comp a, f comp b, bt)
       | LEPointer ->
         if enc_prop then
           CI.Coq_binop (CI.Coq_le_prop, x, y, bt)
         else
           CI.Coq_binop (CI.Coq_le, x, y, bt)
       | LTPointer ->
         if enc_prop then
           CI.Coq_binop (CI.Coq_lt_prop, x, y, bt)
         else
           CI.Coq_binop (CI.Coq_lt, x, y, bt)
       | And ->
         if enc_prop then
           CI.Coq_binop (CI.Coq_and_prop, x, y, bt)
         else
           CI.Coq_binop (CI.Coq_and, x, y, bt)
       | Or ->
         if enc_prop then
           CI.Coq_binop (CI.Coq_or_prop, x, y, bt)
         else
           CI.Coq_binop (CI.Coq_or, x, y, bt)
       | Implies ->
         if enc_prop then
           CI.Coq_binop (CI.Coq_impl_prop, x, y, bt)
         else
           CI.Coq_binop (CI.Coq_impl, x, y, bt)
       | _ -> CI.Coq_unsupported "Unsupported binop")
    | Terms.Match (x, cases) ->
      let comp = Some (it, "case-discriminant") in
      let br (pat, rhs) = (pat_to_coq_ir pat, aux rhs) in
      CI.Coq_match (f comp x, List.map br cases)
    | Terms.ITE (sw, x, y) ->
      let comp = Some (it, "if-condition") in
      CI.Coq_ite (f comp sw, aux x, aux y)
    | Terms.EachI ((i1, (s, t), i2), x) ->
      CI.Coq_eachI ((i1, (CI.Coq_sym s, bt_to_coq_ir global t), i2), aux x)
    | Terms.MapSet (m, x, y) -> CI.Coq_mapset (aux m, aux x, aux y)
    | Terms.MapGet (m, x) -> CI.Coq_mapget (aux m, aux x)
    | Terms.RecordMember (t, m) ->
      let flds = BT.record_bt (Terms.Normal.get_bt t) in
      let ix = find_tuple_element Id.equal m (List.map fst flds) in
      CI.Coq_recordmember (aux t, CI.Coq_id m, ix)
    | Terms.RecordUpdate ((t, m), x) ->
      let flds = BT.record_bt (Terms.Normal.get_bt t) in
      let ix = find_tuple_element Id.equal m (List.map fst flds) in
      CI.Coq_recordupdate ((aux t, CI.Coq_id m), aux x, ix)
    | Terms.Record mems -> CI.Coq_record (List.map aux (List.map snd mems))
    | Terms.StructMember (t, m) ->
      let tag = BaseTypes.struct_bt (Terms.Normal.get_bt t) in
      let mems, _bts = get_struct_xs global.struct_decls tag in
      let ix = find_tuple_element Id.equal m mems in
      CI.Coq_structmember (aux t, CI.Coq_id m, ix)
    | Terms.StructUpdate ((t, m), x) ->
      let tag = BaseTypes.struct_bt (Terms.Normal.get_bt t) in
      let mems, _bts = get_struct_xs global.struct_decls tag in
      let ix = find_tuple_element Id.equal m mems in
      CI.Coq_structupdate ((aux t, CI.Coq_id m), aux x, ix)
    | Terms.Cast (cbt, t) -> CI.Coq_cast (bt_to_coq_ir global cbt, aux t)
    | Terms.Apply (name, args) ->
      let prop_ret = fun_prop_ret global name in
      let body_aux = f (if prop_ret then None else Some (it, "fun-arg")) in
      if prop_ret then
        CI.Coq_apply_prop (CI.Coq_sym name, List.map body_aux args)
      else
        CI.Coq_apply (CI.Coq_sym name, List.map body_aux args)
    | Terms.Good (_, t) -> CI.Coq_good (aux t)
    | Terms.Representable (ct, t2) when Option.is_some (Sctypes.is_struct_ctype ct) ->
      (match Sctypes.is_struct_ctype ct with
       | Some s ->
         CI.Coq_representable (CI.Coq_sym s, CI.Coq_Struct (CI.Coq_sym s, []), aux t2)
       | None ->
         CI.Coq_unsupported "Unsupported representable (why are we in the None case?)")
    | Terms.Constructor (nm, id_args) ->
      let comp = Some (it, "datatype contents") in
      (* assuming here that the id's are in canonical order *)
      CI.Coq_constructor (CI.Coq_sym nm, List.map (fun x -> f comp (snd x)) id_args)
    | Terms.WrapI (ity, arg) ->
      let maxInt = Memory.max_integer_type ity in
      let minInt = Memory.min_integer_type ity in
      CI.Coq_wrapI (maxInt, minInt, aux arg)
    | Terms.Let ((nm, x), y) -> CI.Coq_let (CI.Coq_sym nm, aux x, aux y)
    | Terms.ArrayShift { base; ct; index } ->
      let size_of_ct = Z.of_int @@ Memory.size_of_ctype ct in
      (* do a + b * c*)
      CI.Coq_arrayshift (aux base, size_of_ct, aux index)
    | Terms.Tuple _ -> CI.Coq_unsupported "Unsupported tuple"
    | Terms.NthTuple (_, _) -> CI.Coq_unsupported "Unsupported nth tuple"
    | Terms.Struct (_, _) -> CI.Coq_unsupported "Unsupported struct"
    | Terms.MemberShift _ -> CI.Coq_unsupported "Unsupported member shift"
    | Terms.CopyAllocId _ -> CI.Coq_unsupported "Unsupported copy alloc id"
    | Terms.HasAllocId _ -> CI.Coq_unsupported "Unsupported has alloc id"
    | Terms.SizeOf _ -> CI.Coq_unsupported "Unsupported size of"
    | Terms.OffsetOf (_, _) -> CI.Coq_unsupported "Unsupported offset of"
    | Terms.Nil _ -> CI.Coq_unsupported "Unsupported nil"
    | Terms.Cons (_, _) -> CI.Coq_unsupported "Unsupported cons"
    | Terms.Head _ -> CI.Coq_unsupported "Unsupported head"
    | Terms.Tail _ -> CI.Coq_unsupported "Unsupported tail"
    | Terms.Representable (_, _) -> CI.Coq_unsupported "Unsupported representable"
    | Terms.Aligned _ -> CI.Coq_unsupported "Unsupported aligned"
    | Terms.MapConst (_, _) -> CI.Coq_unsupported "Unsupported map const"
    | Terms.MapDef (_, _) -> CI.Coq_unsupported "Unsupported map def"
    | Terms.CN_None _ | Terms.CN_Some _ | Terms.IsSome _ | Terms.GetOpt _ ->
      CI.Coq_unsupported "Unsupported map def" (* TODO(HK): added for plumbing *)
  in
  f b it


(* unpacking LogicalConstraints *)
let lc_to_coq_ir (gl : Global.t) (t : LC.t) =
  match t with
  | LC.T t -> CI.Coq_pure (it_to_coq_ir gl t None)
  | LC.Forall ((sym, bt), it) ->
    CI.Coq_forall
      (CI.Coq_sym sym, bt_to_coq_ir gl bt, CI.Coq_pure (it_to_coq_ir gl it None))


(* TODO(HK): added this auxiliary function for plumbing *)
let q_step_to_coq_ir (step : Sctypes.t) : CI.coq_term =
  CI.Coq_const (CI.Coq_Z (Z.of_int (Memory.size_of_ctype step)))


(* Unpacking LogicalReturnTypes *)
let rec lrt_to_coq_ir (gl : Global.t) (t : LRT.t) =
  match t with
  | LRT.Constraint (lc, _, t) ->
    let d = lrt_to_coq_ir gl t in
    let c = lc_to_coq_ir gl lc in
    CI.Coq_Constraint_LRT (c, d)
  | LRT.Define ((sym, it), _, t) ->
    let d = lrt_to_coq_ir gl t in
    let l = it_to_coq_ir gl it None in
    CI.Coq_let (CI.Coq_sym sym, l, d)
  | LRT.I -> CI.Coq_LRT_I
  | LRT.Resource ((nm, (req, bt)), _, t) ->
    (match req with
     | P p ->
       (match p.name with
        | Owned (_, init) ->
          (match init with
           | Init ->
             Coq_Owned_LRT
               ( CI.Coq_sym nm,
                 bt_to_coq_ir gl bt,
                 CI.Iris_term (lrt_to_coq_ir gl t),
                 it_to_coq_ir gl p.pointer None,
                 List.map (fun x -> it_to_coq_ir gl x None) p.iargs )
           | Uninit ->
             Coq_Block_LRT
               ( CI.Coq_sym nm,
                 bt_to_coq_ir gl bt,
                 CI.Iris_term (lrt_to_coq_ir gl t),
                 it_to_coq_ir gl p.pointer None ))
        | PName p_nm ->
          Coq_PName_LRT
            ( CI.Coq_sym nm,
              CI.Coq_sym p_nm,
              bt_to_coq_ir gl bt,
              lrt_to_coq_ir gl t,
              List.map (fun x -> it_to_coq_ir gl x None) p.iargs,
              it_to_coq_ir gl p.pointer None ))
     | Q q ->
       (match q.name with
        | Owned _ ->
          CI.Coq_Each_LRT
            ( CI.Coq_sym nm,
              CI.Coq_sym (fst q.q),
              (* q name *)
              bt_to_coq_ir gl (snd q.q),
              (* q value type *)
              it_to_coq_ir gl q.pointer None,
              (* pointer *)
              q_step_to_coq_ir q.step,
              (* step *)
              it_to_coq_ir gl q.permission None,
              (* permission *)
              lrt_to_coq_ir gl t )
        | PName _ -> CI.Coq_unsupported "unsupported Qpred PName in LRT"))


(* Unpacking LogicalArgumentTypes that wrap IndexTerms (i.e. in resource predicates) *)
let rec it_lat_to_coq_ir (gl : Global.t) (t : Terms.Normal.t LAT.t) =
  match t with
  | LAT.Define ((sym, it), _, t) ->
    let d = it_lat_to_coq_ir gl t in
    let l = it_to_coq_ir gl it None in
    CI.Coq_Define (CI.Coq_sym sym, l, d)
  | LAT.Constraint (lc, _, t) ->
    let c = lc_to_coq_ir gl lc in
    let d = it_lat_to_coq_ir gl t in
    CI.Coq_Constraint_LAT (c, d)
  | LAT.I t -> CI.Coq_LAT_I (it_to_coq_ir gl t None)
  | LAT.Resource ((nm, (req, bt)), _, t) ->
    (match req with
     | P p ->
       (match p.name with
        | Owned (_, init) ->
          (match init with
           | Init ->
             Coq_Owned_LAT
               ( CI.Coq_sym nm,
                 bt_to_coq_ir gl bt,
                 CI.Iris_term (it_lat_to_coq_ir gl t),
                 it_to_coq_ir gl p.pointer None,
                 List.map (fun x -> it_to_coq_ir gl x None) p.iargs )
           | Uninit ->
             Coq_Block_LAT
               ( CI.Coq_sym nm,
                 bt_to_coq_ir gl bt,
                 CI.Iris_term (it_lat_to_coq_ir gl t),
                 it_to_coq_ir gl p.pointer None ))
        | PName p_nm ->
          Coq_PName_LAT
            ( CI.Coq_sym nm,
              CI.Coq_sym p_nm,
              bt_to_coq_ir gl bt,
              it_lat_to_coq_ir gl t,
              List.map (fun x -> it_to_coq_ir gl x None) p.iargs,
              it_to_coq_ir gl p.pointer None ))
     (* Can iterated resources even appear here? *)
     | Q q ->
       (match q.name with
        | Owned _ -> CI.Coq_unsupported "unsupported Qpred Owned in LRT"
        | PName _ -> CI.Coq_unsupported "unsupported Qpred PName in LRT"))


(* Unpacking LogicalArgumentTypes that wrap LogicalReturnTypes *)
let rec lrtlat_to_coq_ir (gl : Global.t) t =
  match t with
  | LAT.Define ((sym, it), _, t) ->
    let d = lrtlat_to_coq_ir gl t in
    let l = it_to_coq_ir gl it None in
    CI.Coq_Define (CI.Coq_sym sym, l, d)
  | LAT.Constraint (lc, _, t) ->
    let c = lc_to_coq_ir gl lc in
    let d = lrtlat_to_coq_ir gl t in
    CI.Coq_Constraint_LAT (c, d)
  | LAT.I t -> lrt_to_coq_ir gl t
  | LAT.Resource ((nm, (req, bt)), _, t) ->
    (match req with
     | P p ->
       (match p.name with
        | Owned (_, init) ->
          (match init with
           | Init ->
             Coq_Owned_LAT
               ( CI.Coq_sym nm,
                 bt_to_coq_ir gl bt,
                 CI.Iris_term (lrtlat_to_coq_ir gl t),
                 it_to_coq_ir gl p.pointer None,
                 List.map (fun x -> it_to_coq_ir gl x None) p.iargs )
           | Uninit ->
             Coq_Block_LAT
               ( CI.Coq_sym nm,
                 bt_to_coq_ir gl bt,
                 CI.Iris_term (lrtlat_to_coq_ir gl t),
                 it_to_coq_ir gl p.pointer None ))
        | PName p_nm ->
          Coq_PName_LAT
            ( CI.Coq_sym nm,
              CI.Coq_sym p_nm,
              bt_to_coq_ir gl bt,
              lrtlat_to_coq_ir gl t,
              List.map (fun x -> it_to_coq_ir gl x None) p.iargs,
              it_to_coq_ir gl p.pointer None ))
     | Q q ->
       (match q.name with
        | Owned (_, init) ->
          (match init with
           | Init ->
             CI.Coq_Each_LAT
               ( CI.Coq_sym nm,
                 CI.Coq_sym (fst q.q),
                 (* q name *)
                 bt_to_coq_ir gl (snd q.q),
                 (* q value type *)
                 it_to_coq_ir gl q.pointer None,
                 (* pointer *)
                 q_step_to_coq_ir q.step,
                 (* step *)
                 it_to_coq_ir gl q.permission None,
                 (* permission *)
                 lrtlat_to_coq_ir gl t )
           | Uninit ->
             Coq_Block_LAT
               ( CI.Coq_sym nm,
                 bt_to_coq_ir gl bt,
                 CI.Iris_term (lrtlat_to_coq_ir gl t),
                 it_to_coq_ir gl q.pointer None ))
          (* todo: Each stuff*)
        | PName _ -> Coq_unsupported "unsupported Qpred PName in LRT"))


(* Main translation function for lemmas *)
let rec lemmat_to_coq_ir (gl : Global.t) (ftyp : AT.lemmat) =
  match ftyp with
  | AT.Computational ((sym, bt), _, t) | AT.Ghost ((sym, bt), _, t) ->
    let d = lemmat_to_coq_ir gl t in
    CI.Coq_forall (CI.Coq_sym sym, bt_to_coq_ir gl bt, d)
  | AT.L t -> lrtlat_to_coq_ir gl t


(* Translating all functions for the global typing context *)
let fun_to_coq_ir (gl : Global.t) nm =
  let open Definition.Function in
  let def = Sym.Map.find nm gl.logical_functions in
  let arg_tys = List.map (fun (nm, bt) -> (CI.Coq_sym nm, bt_to_coq_ir gl bt)) def.args in
  match def.body with
  | Uninterp ->
    if fun_prop_ret gl nm then
      CI.Coq_fun_uninterp
        (CI.Coq_sym nm, CI.Coq_uninterp_prop, arg_tys, bt_to_coq_ir gl def.return_bt)
    else
      CI.Coq_fun_uninterp
        (CI.Coq_sym nm, CI.Coq_uninterp, arg_tys, bt_to_coq_ir gl def.return_bt)
  | Def body ->
    CI.Coq_fun_def
      ( CI.Coq_sym nm,
        CI.Coq_def (it_to_coq_ir gl body (Some (body, "logical fun def"))),
        arg_tys,
        bt_to_coq_ir gl def.return_bt )
  | Rec_Def body ->
    CI.Coq_fun_def
      ( CI.Coq_sym nm,
        CI.Coq_recdef (it_to_coq_ir gl body (Some (body, "logical fun recdef"))),
        arg_tys,
        bt_to_coq_ir gl def.return_bt )


let logical_funs_to_coq_ir (gl : Global.t) (funs : Sym.t list list) =
  let logicalfun_clump_to_coq_ir (gl : Global.t) (nms : Sym.t list) =
    List.map (fun_to_coq_ir gl) nms
  in
  let is_uninterp a =
    match a with CI.Coq_fun_uninterp _ -> true | CI.Coq_fun_def _ -> false
  in
  let translated_funs = List.map (logicalfun_clump_to_coq_ir gl) funs in
  ( List.filter (fun x -> is_uninterp (List.hd x)) translated_funs,
    List.filter (fun x -> not (is_uninterp (List.hd x))) translated_funs )


(* Translating all resource predicates for the global typing context *)

let pred_to_coq_ir (gl : Global.t) (nm : Sym.t) =
  let open Definition.Predicate in
  let pred = Sym.Map.find nm gl.resource_predicates in
  let translate_one_clause (clause : Definition.Clause.t) =
    CI.Coq_clause
      ([ it_to_coq_ir gl clause.guard None ], it_lat_to_coq_ir gl clause.packing_ft)
  in
  let args = List.map (fun (nm, bt) -> (CI.Coq_sym nm, bt_to_coq_ir gl bt)) pred.iargs in
  (* distinguishing between defined and uninterpreted predicates *)
  match pred.clauses with
  | Some clauses ->
    CI.Coq_rpred
      ( CI.Coq_sym nm,
        CI.Coq_sym pred.pointer,
        args,
        bt_to_coq_ir gl (snd pred.oarg),
        List.map translate_one_clause clauses )
  | None ->
    CI.Coq_rpred_uninterp
      (CI.Coq_sym nm, CI.Coq_sym pred.pointer, args, bt_to_coq_ir gl (snd pred.oarg))


let resource_pred_to_coq_ir (gl : Global.t) (preds : Sym.t list list) =
  let rpred_clump_to_coq_ir (gl : Global.t) (nms : Sym.t list) =
    List.map (pred_to_coq_ir gl) nms
  in
  List.map (rpred_clump_to_coq_ir gl) preds


(* translate the whole global context to coq_ir *)

let cn_to_coq_ir (global : Global.t) (lemmata : (Sym.t * (Loc.t * AT.lemmat)) list) =
  (* 1. Translate the datatypes *)
  let translated_dtypes =
    if Option.is_some global.datatype_order then
      dtypes_to_coq_ir global (Option.get global.datatype_order)
    else
      []
  in
  (* 2. Translate the logical functions *)
  let translated_logical_funs =
    if Option.is_some global.logical_function_order then
      logical_funs_to_coq_ir global (Option.get global.logical_function_order)
    else
      ([], [])
  in
  (* 3. Translate the resource predicates (todo) *)
  let translated_preds =
    if Option.is_some global.resource_predicate_order then
      resource_pred_to_coq_ir global (Option.get global.resource_predicate_order)
    else
      []
  in
  (* 4. Translate the lemma statement *)
  let translate_lemmas ((sym : Sym.t), (_, lemmat)) =
    let d = lemmat_to_coq_ir global lemmat in
    CI.Coq_lemma (CI.Coq_sym sym, d)
  in
  (* gives a list of pairs: (lemma name, translated lemma)*)
  CI.Coq_gl
    ( translated_dtypes,
      translated_logical_funs,
      translated_preds,
      List.map translate_lemmas lemmata )
