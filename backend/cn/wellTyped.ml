module LS = LogicalSorts
module BT = BaseTypes
module SymSet = Set.Make(Sym)
module TE = TypeErrors
module RE = Resources
module RET = ResourceTypes
module AT = ArgumentTypes
module LAT = LogicalArgumentTypes

open Global
open TE
open Pp
open Locations


open Typing
open Effectful.Make(Typing)


let check_consistency = ref true


let ensure_logical_sort (loc : loc) ~(expect : LS.t) (has : LS.t) : (unit, type_error) m =
  if LS.equal has expect 
  then return () 
  else fail (fun _ -> {loc; msg = Mismatch {has = BT.pp has; expect = BT.pp expect}})

let ensure_base_type (loc : loc) ~(expect : BT.t) (has : BT.t) : (unit, type_error) m =
  ensure_logical_sort loc ~expect has


let check_bound_l loc s = 
  let@ is_bound = bound_l s in
  if is_bound then return ()
  else fail (fun _ -> {loc; msg = TE.Unknown_variable s})


let illtyped_index_term (loc: loc) context it has expected ctxt =
  {loc = loc; msg = TypeErrors.Illtyped_it {context; it; has; expected; ctxt}}


let ensure_integer_or_real_type (loc : loc) context it = 
  let open BT in
  match IT.bt it with
  | (Integer | Real) -> return ()
  | _ -> 
     let expect = "integer or real type" in
     fail (illtyped_index_term loc context it (IT.bt it) expect)

let ensure_set_type loc context it = 
  let open BT in
  match IT.bt it with
  | Set bt -> return bt
  | _ -> fail (illtyped_index_term loc context it (IT.bt it) "set type")

let ensure_list_type loc context it = 
  let open BT in
  match IT.bt it with
  | List bt -> return bt
  | _ -> fail (illtyped_index_term loc context it (IT.bt it) "list type")

let ensure_map_type loc context it = 
  let open BT in
  match IT.bt it with
  | Map (abt, rbt) -> return (abt, rbt)
  | _ -> fail (illtyped_index_term loc context it (IT.bt it) "map/array type")

let ensure_same_argument_number loc input_output has ~expect =
  if has = expect then return () else 
    match input_output with
    | `General -> fail (fun _ -> {loc; msg = Number_arguments {has; expect}})
    | `Input -> fail (fun _ -> {loc; msg = Number_input_arguments {has; expect}})
    | `Output -> fail (fun _ -> {loc; msg = Number_output_arguments {has; expect}})


module WBT = struct

  open BT
  let is_bt loc = 
    let rec aux = function
      | Unit -> return ()
      | Bool -> return ()
      | Integer -> return ()
      | Real -> return ()
      | Loc -> return ()
      | Struct tag -> let@ _ = get_struct_decl loc tag in return ()
      | Record members -> ListM.iterM (fun (_, bt) -> aux bt) members
      | Map (abt, rbt) -> ListM.iterM aux [abt; rbt]
      | List bt -> aux bt
      | Tuple bts -> ListM.iterM aux bts
      | Set bt -> aux bt
    in
    fun bt -> aux bt
[@@deriving eq, ord]

end


module WLS = struct

  let is_ls loc ls = WBT.is_bt loc ls

end


module WCT = struct

  open Sctypes

  let is_ct loc = 
    let rec aux = function
      | Void -> return ()
      | Integer _ -> return ()
      | Array (ct, _) -> aux ct
      | Pointer ct -> aux ct
      | Struct tag -> let@ _ = get_struct_decl loc tag in return ()
      | Function ((_, rct), args, _) -> ListM.iterM aux (rct :: List.map fst args)
    in
    fun ct -> aux ct

end


module WIT = struct


  open BaseTypes
  open LogicalSorts
  open IndexTerms

  type t = IndexTerms.t

  (* We expect a (sub) term to be constant unless it contains a free variable
     or it makes use of an uninterpreted logical predicate. *)
  let is_const loc t =
    let fs = IndexTerms.free_vars t in
    Pp.debug 2 (lazy (Pp.item "is_const: vars" (Pp.flow (Pp.string " ")
        (IT.pp t :: Pp.colon :: List.map Sym.pp (SymSet.elements fs)))));
    if not (SymSet.is_empty fs) then return false
    else
    let ps = IndexTerms.preds_of t in
    Pp.debug 2 (lazy (Pp.item "is_const: preds" (Pp.flow (Pp.string " ")
        (IT.pp t :: Pp.colon :: List.map Sym.pp (SymSet.elements ps)))));
    let@ defns = ListM.mapM (Typing.is_fully_defined_pred loc) (SymSet.elements ps) in
    return (List.for_all (fun x -> x) defns)


  let rec infer : 'bt. Loc.t -> context:(BT.t IT.term) -> 'bt IT.term -> (IT.t, type_error) m =
      fun loc ~context (IT (it, _)) ->
      match it with
      | Lit lit ->
         let@ (bt, lit) = match lit with
           | Sym s ->
              let@ () = check_bound_l loc s in
              let@ bt = get_l s in
              return (bt, Sym s)
           | Z z -> 
              return (Integer, Z z)
           | Q q -> 
              return (Real, Q q)
           | Pointer p -> 
              return (Loc, Pointer p)
           | Bool b -> 
              return (BT.Bool, Bool b)
           | Unit -> 
              return (BT.Unit, Unit)
           | Default bt -> 
              let@ () = WBT.is_bt loc bt in
              return (bt, Default bt)
           | Null ->
              return (BT.Loc, Null)
         in
         return (IT (Lit lit, bt))
      | Arith_op arith_op ->
         begin match arith_op with
         | Add (t,t') ->
            let@ t = infer loc ~context t in
            let@ () = ensure_integer_or_real_type loc context t in
            let@ t' = check loc ~context (IT.bt t) t' in
            return (IT (Arith_op (Add (t, t')), IT.bt t))
         | Sub (t,t') ->
            let@ t = infer loc ~context t in
            let@ () = ensure_integer_or_real_type loc context t in
            let@ t' = check loc ~context (IT.bt t) t' in
            return (IT (Arith_op (Sub (t, t')), IT.bt t))
         | Mul (t,t') ->
            let@ t = infer loc ~context t in
            let@ () = ensure_integer_or_real_type loc context t in
            let@ t' = check loc ~context (IT.bt t) t' in
            let@ c = is_const loc t in
            let@ c' = is_const loc t' in
            let@ () = 
              if c || c' then return () else
                let hint = "Only multiplication by constants is allowed" in
                fail (fun ctxt -> {loc; msg = NIA {context; it = IT.mul_ (t, t'); ctxt; hint}})
            in
            return (IT (Arith_op (Mul (t, t')), IT.bt t))
         | MulNoSMT (t,t') ->
            let@ t = infer loc ~context t in
            let@ () = ensure_integer_or_real_type loc context t in
            let@ t' = check loc ~context (IT.bt t) t' in
            return (IT (Arith_op (MulNoSMT (t, t')), IT.bt t))
         | Div (t,t') ->
            let@ t = infer loc ~context t in
            let@ () = ensure_integer_or_real_type loc context t in
            let@ t' = check loc ~context (IT.bt t) t' in
            let@ c' = is_const loc t' in
            let@ () = 
              if c' then return () else
                let hint = "Only division by constants is allowed" in
                fail (fun ctxt -> {loc; msg = NIA {context; it = div_ (t, t'); ctxt; hint}})
            in
            return (IT (Arith_op (Div (t, t')), IT.bt t))
         | DivNoSMT (t,t') ->
            let@ t = infer loc ~context t in
            let@ () = ensure_integer_or_real_type loc context t in
            let@ t' = check loc ~context (IT.bt t) t' in
            return (IT (Arith_op (DivNoSMT (t, t')), IT.bt t))
         | Exp (t,t') ->
            let@ t = check loc ~context Integer t in
            let@ t' = check loc ~context Integer t' in
            begin match is_z t, is_z t' with
            | Some z, Some z' -> 
               if Z.lt z' Z.zero then
                 fail (fun ctxt -> {loc; msg = NegativeExponent {context; it = exp_ (t, t'); ctxt}})
               else if Z.fits_int32 z' then
                 return (IT (Arith_op (Exp (t, t')), Integer))
               else 
                 fail (fun ctxt -> {loc; msg = TooBigExponent {context; it = exp_ (t, t'); ctxt}})
            | _ ->
               let hint = "Only exponentiation of two constants is allowed" in
               fail (fun ctxt -> {loc; msg = NIA {context; it = exp_ (t, t'); ctxt; hint}})
            end
           | ExpNoSMT (t,t') ->
              let@ t = check loc ~context Integer t in
              let@ t' = check loc ~context Integer t' in
              return (exp_no_smt_ (t, t'))
           | Rem (t,t') ->
              let@ t = check loc ~context Integer t in
              let@ t' = check loc ~context Integer t' in
	      let@ c' = is_const loc t' in
              let@ () = 
                if c' then return () else
                  let hint = "Only division by constants is allowed" in
                  fail (fun ctxt -> {loc; msg = NIA {context; it = rem_ (t, t'); ctxt; hint}})
              in
              return (IT (Arith_op (Rem (t, t')), Integer))
           | RemNoSMT (t,t') ->
              let@ t = check loc ~context Integer t in
              let@ t' = check loc ~context Integer t' in
              return (IT (Arith_op (RemNoSMT (t, t')), Integer))
           | Mod (t,t') ->
              let@ t = check loc ~context Integer t in
              let@ t' = check loc ~context Integer t' in
	      let@ c' = is_const loc t' in
              let@ () = 
                if c' then return () else
                  let hint = "Only division by constants is allowed" in
                  fail (fun ctxt -> {loc; msg = NIA {context; it = mod_ (t, t'); ctxt; hint}})
              in
              return (IT (Arith_op (Mod (t, t')), Integer))
           | ModNoSMT (t,t') ->
              let@ t = check loc ~context Integer t in
              let@ t' = check loc ~context Integer t' in
              return (IT (Arith_op (ModNoSMT (t, t')), Integer))
           | LT (t,t') ->
              let@ t = infer loc ~context t in
              let@ () = ensure_integer_or_real_type loc context t in
              let@ t' = check loc ~context (IT.bt t) t' in
              return (IT (Arith_op (LT (t, t')), BT.Bool))
           | LE (t,t') ->
              let@ t = infer loc ~context t in
              let@ () = ensure_integer_or_real_type loc context t in
              let@ t' = check loc ~context (IT.bt t) t' in
              return (IT (Arith_op (LE (t, t')), BT.Bool))
           | Min (t,t') ->
              let@ t = infer loc ~context t in
              let@ () = ensure_integer_or_real_type loc context t in
              let@ t' = check loc ~context (IT.bt t) t' in
              return (IT (Arith_op (Min (t, t')), IT.bt t))
           | Max (t,t') ->
              let@ t = infer loc ~context t in
              let@ () = ensure_integer_or_real_type loc context t in
              let@ t' = check loc ~context (IT.bt t) t' in
              return (IT (Arith_op (Max (t, t')), IT.bt t))
           | IntToReal t ->
              let@ t = check loc ~context Integer t in
              return (IT (Arith_op (IntToReal t), BT.Real))
           | RealToInt t ->
              let@ t = check loc ~context Real t in
              return (IT (Arith_op (IntToReal t), BT.Integer))
           | XORNoSMT (t, t') ->
              let@ t = check loc ~context Integer t in
              let@ t' = check loc ~context Integer t' in
              return (IT (Arith_op (XORNoSMT (t, t')), BT.Integer))
         end
      | Bool_op bool_op ->
         let@ (bt, bool_op) = match bool_op with
           | And ts ->
              let@ ts = ListM.mapM (check loc ~context Bool) ts in
              return (BT.Bool, And ts)
           | Or ts ->
              let@ ts = ListM.mapM (check loc ~context Bool) ts in
              return (BT.Bool, Or ts)
           | Impl (t,t') ->
              let@ t = check loc ~context Bool t in
              let@ t' = check loc ~context Bool t' in
              return (BT.Bool, Impl (t, t'))
           | Not t ->
              let@ t = check loc ~context Bool t in
              return (BT.Bool, Not t)
           | ITE (t,t',t'') ->
              let@ t = check loc ~context Bool t in
              let@ t' = infer loc ~context t' in
              let@ t'' = check loc ~context (IT.bt t') t'' in
              return (IT.bt t', ITE (t, t', t''))
           | EQ (t,t') ->
              let@ t = infer loc ~context t in
              let@ t' = check loc ~context (IT.bt t) t' in
              return (BT.Bool, EQ (t,t')) 
           | EachI ((i1, s, i2), t) ->
              pure begin 
                  let@ () = add_l s Integer in
                  let@ t = check loc ~context Bool t in
                  return (BT.Bool, EachI ((i1, s, i2), t))
                end
         in
         return (IT (Bool_op bool_op, bt))
      | Tuple_op tuple_op ->
         let@ (bt, tuple_op) = match tuple_op with
           | Tuple ts ->
              let@ ts = ListM.mapM (infer loc ~context) ts in
              let bts = List.map IT.bt ts in
              return (BT.Tuple bts, Tuple ts)
           | NthTuple (n, t') ->
              let@ t' = infer loc ~context t' in
              let@ item_bt = match IT.bt t' with
                | Tuple bts ->
                   begin match List.nth_opt bts n with
                   | Some t -> return t
                   | None -> 
                      let expected = "tuple with at least " ^ string_of_int n ^ "components" in
                      fail (illtyped_index_term loc context t' (Tuple bts) expected)
                   end
                | has -> 
                   fail (illtyped_index_term loc context t' has "tuple")
              in
              return (item_bt, NthTuple (n, t'))
         in
         return (IT (Tuple_op tuple_op, bt))
      | Struct_op struct_op ->
         let@ (bt, struct_op) = match struct_op with
           | Struct (tag, members) ->
              let@ layout = get_struct_decl loc tag in
              let decl_members = Memory.member_types layout in
              let@ () = 
                let has = List.length members in
                let expect = List.length decl_members in
                if has = expect then return ()
                else fail (fun _ -> {loc; msg = Number_members {has; expect}})
              in
              let@ members = 
                ListM.mapM (fun (member,t) ->
                    let@ bt = match List.assoc_opt Id.equal member decl_members with
                      | Some sct -> return (BT.of_sct sct)
                      | None -> fail (fun _ -> {loc; msg = Unknown_member (tag, member)})
                    in
                    let@ t = check loc ~context bt t in
                    return (member, t)
                  ) members
              in
              return (BT.Struct tag, Struct (tag, members))
           | StructMember (t, member) ->
              let@ t = infer loc ~context t in
              let@ tag = match IT.bt t with
                | Struct tag -> return tag
                | has -> fail (illtyped_index_term loc context t has "struct")
              in
              let@ field_ct = get_struct_member_type loc tag member in
              return (BT.of_sct field_ct, StructMember (t, member))
           | StructUpdate ((t, member), v) ->
              let@ t = infer loc ~context t in
              let@ tag = match IT.bt t with
                | Struct tag -> return tag
                | has -> fail (illtyped_index_term loc context t has "struct")
              in
              let@ field_ct = get_struct_member_type loc tag member in
              let@ v = check loc ~context (BT.of_sct field_ct) v in
              return (BT.Struct tag, StructUpdate ((t, member), v))
         in
         return (IT (Struct_op struct_op, bt))
      | Record_op record_op ->
         let@ (bt, record_op) = match record_op with
           | Record members ->
              let@ members = 
                ListM.mapM (fun (member,t) ->
                    let@ t = infer loc ~context t in
                    return (member, t)
                  ) members
              in
              let member_types = 
                List.map (fun (member, t) -> (member, IT.bt t)
                  ) members
              in
              return (BT.Record member_types, IT.Record members)
           | RecordMember (t, member) ->
              let@ t = infer loc ~context t in
              let@ members = match IT.bt t with
                | Record members -> return members
                | has -> fail (illtyped_index_term loc context t has "struct")
              in
              let@ bt = match List.assoc_opt Sym.equal member members with
                | Some bt -> return bt
                | None -> 
                   let expected = "struct with member " ^ Sym.pp_string member in
                   fail (illtyped_index_term loc context t (IT.bt t) expected)
              in
              return (bt, RecordMember (t, member))
           | RecordUpdate ((t, member), v) ->
              let@ t = infer loc ~context t in
              let@ members = match IT.bt t with
                | Record members -> return members
                | has -> fail (illtyped_index_term loc context t has "struct")
              in
              let@ bt = match List.assoc_opt Sym.equal member members with
                | Some bt -> return bt
                | None -> 
                   let expected = "struct with member " ^ Sym.pp_string member in
                   fail (illtyped_index_term loc context t (IT.bt t) expected)
              in
              let@ v = check loc ~context bt v in
              return (IT.bt t, RecordUpdate ((t, member), v))
         in
         return (IT (Record_op record_op, bt))
      | Pointer_op pointer_op ->
         let@ (bt, pointer_op) = match pointer_op with 
           | LTPointer (t, t') ->
              let@ t = check loc ~context Loc t in
              let@ t' = check loc ~context Loc t' in
              return (BT.Bool, LTPointer (t, t'))
           | LEPointer (t, t') ->
              let@ t = check loc ~context Loc t in
              let@ t' = check loc ~context Loc t' in
              return (BT.Bool, LEPointer (t, t'))
           | IntegerToPointerCast t ->
              let@ t = check loc ~context Integer t in
              return (Loc, IntegerToPointerCast t)
           | PointerToIntegerCast t ->
              let@ t = check loc ~context Loc t in
              return (Integer, PointerToIntegerCast t)
           | MemberOffset (tag, member) ->
              let@ layout = get_struct_decl loc tag in
              let decl_members = Memory.member_types layout in
              let@ () = match List.assoc_opt Id.equal member decl_members with
                | Some _ -> return ()
                | None -> fail (fun _ -> {loc; msg = Unknown_member (tag, member)})
              in
              return (Integer, MemberOffset (tag, member))
           | ArrayOffset (ct, t) ->
              let@ () = WCT.is_ct loc ct in
              let@ t = check loc ~context Integer t in
              return (Integer, ArrayOffset (ct, t))
         in
         return (IT (Pointer_op pointer_op, bt))
      | CT_pred ct_pred ->
         let@ (bt, ct_pred) = match ct_pred with
           | AlignedI t ->
              let@ t_t = check loc ~context Loc t.t in
              let@ t_align = check loc ~context Integer t.align in
              return (BT.Bool, AlignedI {t = t_t; align=t_align})
           | Aligned (t, ct) ->
              let@ () = WCT.is_ct loc ct in
              let@ t = check loc ~context Loc t in
              return (BT.Bool, Aligned (t, ct))
           | Representable (ct, t) ->
              let@ () = WCT.is_ct loc ct in
              let@ t = check loc ~context (BT.of_sct ct) t in
              return (BT.Bool, Representable (ct, t))
           | Good (ct, t) ->
              let@ () = WCT.is_ct loc ct in
              let@ t = check loc ~context (BT.of_sct ct) t in
              return (BT.Bool, Good (ct, t))
         in
         return (IT (CT_pred ct_pred, bt))
      | List_op list_op ->
         let@ (bt, list_op) = match list_op with
           | Nil -> 
              fail (fun _ -> {loc; msg = Polymorphic_it context})
           | Cons (t1,t2) ->
              let@ t1 = infer loc ~context t1 in
              let@ t2 = check loc ~context (List (IT.bt t1)) t2 in
              return (BT.List (IT.bt t1), Cons (t1, t2))
           | List [] ->
              fail (fun _ -> {loc; msg = Polymorphic_it context})
           | List (t :: ts) ->
              let@ t = infer loc ~context t in
              let@ ts = ListM.mapM (check loc ~context (IT.bt t)) ts in
              return (BT.List (IT.bt t), List (t :: ts))
           | Head t ->
              let@ t = infer loc ~context t in
              let@ bt = ensure_list_type loc context t in
              return (bt, Head t)
           | Tail t ->
              let@ t = infer loc ~context t in
              let@ bt = ensure_list_type loc context t in
              return (BT.List bt, Tail t)
           | NthList (i, t) ->
              let@ t = infer loc ~context t in
              let@ bt = ensure_list_type loc context t in
              return (bt, NthList (i, t))
         in
         return (IT (List_op list_op, bt))
      | Set_op set_op ->
         let@ (bt, set_op) = match set_op with
           | SetMember (t,t') ->
              let@ t = infer loc ~context t in
              let@ t' = check loc ~context (Set (IT.bt t)) t' in
              return (BT.Bool, SetMember (t, t'))
           | SetUnion its ->
              let@ (t, ts) = match its with
                | t :: ts -> return (t, ts)
                | _ -> fail (fun _ -> {loc; msg = Polymorphic_it context})
              in
              let@ t = infer loc ~context t in
              let@ itembt = ensure_set_type loc context t in
              let@ ts = ListM.mapM (check loc ~context (Set itembt)) ts in
              return (BT.Set itembt, SetUnion (t :: ts))
           | SetIntersection its ->
              let@ (t, ts) = match its with
                | t :: ts -> return (t, ts)
                | _ -> fail (fun _ -> {loc; msg = Polymorphic_it context})
              in
              let@ t = infer loc ~context t in
              let@ itembt = ensure_set_type loc context t in
              let@ ts = ListM.mapM (check loc ~context (Set itembt)) ts in
              return (BT.Set itembt, SetIntersection (t :: ts))
           | SetDifference (t, t') ->
              let@ t  = infer loc ~context t in
              let@ itembt = ensure_set_type loc context t in
              let@ t' = check loc ~context (Set itembt) t' in
              return (BT.Set itembt, SetDifference (t, t'))
           | Subset (t, t') ->
              let@ t = infer loc ~context t in
              let@ itembt = ensure_set_type loc context t in
              let@ t' = check loc ~context (Set itembt) t' in
              return (BT.Bool, Subset (t,t'))
         in
         return (IT (Set_op set_op, bt))
      | Map_op map_op -> 
         let@ (bt, map_op) = match map_op with
           | Const (index_bt, t) ->
              let@ () = WBT.is_bt loc index_bt in
              let@ t = infer loc ~context t in
              return (BT.Map (index_bt, IT.bt t), Const (index_bt, t))
           | Set (t1, t2, t3) ->
              let@ t1 = infer loc ~context t1 in
              let@ (abt, rbt) = ensure_map_type loc context t1 in
              let@ t2 = check loc ~context abt t2 in
              let@ t3 = check loc ~context rbt t3 in
              return (IT.bt t1, Set (t1, t2, t3))
           | Get (t, arg) -> 
              let@ t = infer loc ~context t in
              let@ (abt, bt) = ensure_map_type loc context t in
              let@ arg = check loc ~context abt arg in
              return (bt, Get (t, arg))
           | Def ((s, abt), body) ->
              let@ () = WBT.is_bt loc abt in
              pure begin
                  let@ () = add_l s abt in
                  let@ body = infer loc ~context body in
                  return (Map (abt, IT.bt body), Def ((s, abt), body))
                end
         in
         return (IT (Map_op map_op, bt))
      | Pred (name, args) ->
         let@ def = Typing.get_logical_predicate_def loc name in
         let has_args, expect_args = List.length args, List.length def.args in
         let@ () = ensure_same_argument_number loc `General has_args ~expect:expect_args in
         let@ args = 
           ListM.map2M (fun has_arg (_, def_arg_bt) ->
               check loc ~context def_arg_bt has_arg
             ) args def.args
         in
         return (IT (Pred (name, args), def.return_bt))
         

    and check : 'bt. Loc.t -> context:(BT.t IT.term) -> LS.t -> 'bt IT.term -> (IT.t, type_error) m =
      fun loc ~context ls it ->
      let@ () = WLS.is_ls loc ls in
      match it, ls with
      | IT (List_op Nil, _), List bt ->
         return (IT (List_op Nil, BT.List bt))
      | _, _ ->
         let@ it = infer loc ~context it in
         if LS.equal ls (IT.bt it) then
           return it
         else
           let expected = Pp.plain (LS.pp ls) in
           fail (illtyped_index_term loc context it (IT.bt it) expected)

  let infer loc it = infer loc ~context:it it
  let check loc ls it = check loc ~context:it ls it



end





let unconstrained_lvar loc infos lvar = 
  let (loc, odescr) = SymMap.find lvar infos in
  fail (fun _ -> {loc; msg = Unconstrained_logical_variable (lvar, odescr)})



module WRET = struct

  let welltyped loc r = 
    let@ iargs = match RET.predicate_name r with
      | Block _ ->
         return (Resources.block_iargs)
      | Owned ct ->
         return (Resources.owned_iargs ct)
      | PName name -> 
         let@ def = Typing.get_resource_predicate_def loc name in
         return (def.iargs)
    in
    match r with
    | P p -> 
       let@ _ = WIT.check loc BT.Loc p.pointer in
       let@ _ = WIT.check loc BT.Bool p.permission in
       let has_iargs, expect_iargs = List.length p.iargs, List.length iargs in
       (* +1 because of pointer argument *)
       let@ () = ensure_same_argument_number loc `Input (1 + has_iargs) ~expect:(1 + expect_iargs) in
       let@ _ = ListM.map2M (fun (_, expected) arg -> WIT.check loc expected arg) iargs p.iargs in
       return ()
    | Q p ->
       let@ _ = WIT.check loc BT.Loc p.pointer in
       let@ _ = 
         pure begin 
             let@ () = add_l p.q Integer in
             let@ _ = WIT.check loc BT.Bool p.permission in
             let has_iargs, expect_iargs = List.length p.iargs, List.length iargs in
             (* +1 because of pointer argument *)
             let@ () = ensure_same_argument_number loc `Input (1 + has_iargs) ~expect:(1 + expect_iargs) in
             ListM.map2M (fun (_, expected) arg -> WIT.check loc expected arg) iargs p.iargs
           end  
       in
       return ()
end



let oargs_spec loc = function
  | RET.P {name = Block _; _} -> 
     return (Resources.block_oargs)
  | RET.P {name = Owned ct; _} -> 
     return (Resources.owned_oargs ct)
  | RET.P {name = PName pn; _} ->
     let@ def = Typing.get_resource_predicate_def loc pn in
         return (BT.Record def.oargs)
  | RET.Q {name = Block _; _} -> 
     return (Resources.q_block_oargs)
  | RET.Q {name = Owned ct; _} -> 
     return (Resources.q_owned_oargs ct)
  | RET.Q {name = PName pn; _} ->
     let@ def = Typing.get_resource_predicate_def loc pn in
     return (BT.Record (List.map_snd (BT.make_map_bt Integer) def.oargs))



module WRE = struct

  let welltyped loc ((resource, O resource_oargs) : RE.t) = 
    let@ () = WRET.welltyped loc resource in
    let@ oargs_spec = oargs_spec loc resource in
    WIT.check loc oargs_spec resource_oargs

end


module WRS = struct

  let welltyped loc ((resource, bt)) = 
    let@ () = WRET.welltyped loc resource in
    let@ () = WBT.is_bt loc bt in
    let@ oargs_spec = oargs_spec loc resource in
    ensure_base_type loc ~expect:oargs_spec bt

end




module WLC = struct
  type t = LogicalConstraints.t

  let welltyped loc lc =
    match lc with
    | LC.T it -> 
       let@ _ = WIT.check loc BT.Bool it in
       return ()
    | LC.Forall ((s,bt), it) ->
       let@ () = WBT.is_bt loc bt in
       pure begin
           let@ () = add_l s bt in
           let@ _ = WIT.check loc BT.Bool it in
           return ()
       end

end

module WLRT = struct

  module LRT = LogicalReturnTypes
  open LRT
  type t = LogicalReturnTypes.t

  let welltyped loc lrt = 
    let rec aux = function
      | Define ((s, it), info, lrt) ->
         let@ it = WIT.infer loc it in
         let@ () = add_l s (IT.bt it) in
         let@ () = add_c (LC.t_ (IT.def_ s it)) in
         aux lrt
      | Resource ((s, (re, re_oa_spec)), info, lrt) -> 
         let@ () = WRS.welltyped (fst info) (re, re_oa_spec) in
         let@ () = add_l s re_oa_spec in
         let@ () = add_r None (re, O (IT.sym_ (s, re_oa_spec))) in
         aux lrt
      | Constraint (lc, info, lrt) ->
         let@ () = WLC.welltyped (fst info) lc in
         let@ () = add_c lc in
         aux lrt
      | I -> 
         return ()
    in
    pure (aux lrt)


end


module WRT = struct

  include ReturnTypes
  type t = ReturnTypes.t

  let welltyped loc rt = 
    pure begin match rt with 
      | Computational ((name,bt), info, lrt) ->
         let@ () = WBT.is_bt (fst info) bt in
         let@ () = add_l name bt in
         let@ () = add_a (Sym.fresh_same name) (bt, name) in
         WLRT.welltyped loc lrt
      end

end



module WFalse = struct
  include False
  type t = False.t
  let welltyped _ _ = return ()
end

module type WOutputSpec = sig val name_bts : (Sym.t * LS.t) list end
module WOutputDef (Spec : WOutputSpec) = struct
  include OutputDef
  type t = OutputDef.t
  let welltyped loc assignment =
    let rec aux name_bts assignment =
      match name_bts, assignment with
      | [], [] -> 
         return ()
      | (name, bt) :: name_bts, {loc; name = name'; value = it} :: assignment 
           when Sym.equal name name' ->
         let@ _ = WIT.check loc bt it in
         aux name_bts assignment
      | (name, bt) :: name_bts, {loc; name = name'; value = it} :: _ ->
         fail (fun _ -> {loc; msg = Generic (!^"expected output argument" ^^^ Sym.pp name ^^^ !^"but found" ^^^ Sym.pp name')})
      | (name, _) :: _, _ -> 
         fail (fun _ -> {loc; msg = Generic (!^"expected output argument" ^^^ Sym.pp name)})
      | _, {loc = loc'; name = name'; _} :: _ -> 
         fail (fun _ -> {loc; msg = Generic (!^"unexpected output argument" ^^^ Sym.pp name')})
    in
    aux Spec.name_bts assignment

end


module type WI_Sig = sig

  type t

  val subst : IndexTerms.t Subst.t -> t -> t

  val pp : t -> Pp.document

  val welltyped : 
    Loc.t -> 
    t -> 
    (unit, type_error) m
end


module WLAT (WI: WI_Sig) = struct

  type t = WI.t LAT.t

  let welltyped kind loc (at : t) : (unit, type_error) m = 
    let rec aux = function
      | LAT.Define ((s, it), info, at) ->
         let@ it = WIT.infer loc it in
         let@ () = add_l s (IT.bt it) in
         let@ () = add_c (LC.t_ (IT.def_ s it)) in
         aux at
      | LAT.Resource ((s, (re, re_oa_spec)), info, at) -> 
         let@ () = WRS.welltyped (fst info) (re, re_oa_spec) in
         let@ () = add_l s re_oa_spec in
         let@ () = add_r None (re, O (IT.sym_ (s, re_oa_spec))) in
         aux at
      | LAT.Constraint (lc, info, at) ->
         let@ () = WLC.welltyped (fst info) lc in
         let@ () = add_c lc in
         aux at
      | LAT.I i -> 
         let@ provable = provable loc in
         let@ () = 
           if !check_consistency then
             match provable (LC.t_ (IT.bool_ false)) with
             | `True -> fail (fun _ -> {loc; msg = Generic !^("this "^kind^" makes inconsistent assumptions")})
             | `False -> return ()
           else return ()
         in
         let@ () = WI.welltyped loc i in
         return ()
    in
    pure (aux at)



end



module WAT (WI: WI_Sig) = struct

  module WLAT = WLAT(WI)

  type t = WI.t AT.t

  let welltyped kind loc (at : t) : (unit, type_error) m = 
    let rec aux = function
      | AT.Computational ((name,bt), info, at) ->
         let@ () = WBT.is_bt (fst info) bt in
         let@ () = add_l name bt in
         let@ () = add_a (Sym.fresh_same name) (bt, name) in
         aux at
      | AT.L at ->
         WLAT.welltyped kind loc at
    in
    pure (aux at)



end


module WFT = WAT(WRT)
module WLT = WAT(WFalse)
module WPackingFT(Spec : WOutputSpec) = WLAT(WOutputDef(Spec))

module WRPD = struct

  let welltyped pd = 
    pure begin
        let open ResourcePredicates in
        let@ () = add_l pd.pointer BT.Loc in
        let@ () = 
          ListM.iterM (fun (s, ls) -> 
              let@ () = WLS.is_ls pd.loc ls in
              add_l s ls
            ) pd.iargs 
        in
        let@ () = ListM.iterM (WLS.is_ls pd.loc) (List.map snd pd.oargs) in
        let module WPackingFT = WPackingFT(struct let name_bts = pd.oargs end)  in
        match pd.clauses with
        | None -> return ()
        | Some clauses ->
           ListM.iterM (fun {loc; guard; packing_ft} ->
               let@ _ = WIT.check loc BT.Bool guard in
               WPackingFT.welltyped "clause" pd.loc packing_ft
             ) clauses
      end

end



module WLPD = struct

  let welltyped (pd : LogicalPredicates.definition) = 
    pure begin
        let@ () = ListM.iterM (WLS.is_ls pd.loc) (List.map snd pd.args) in
        let@ () = add_ls pd.args in
        let@ () = WBT.is_bt pd.loc pd.return_bt in
        match pd.definition with
        | Def body -> let@ _ = WIT.check pd.loc pd.return_bt body in return ()
        | Uninterp -> return ()
      end

end



