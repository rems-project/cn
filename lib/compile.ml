module CF = Cerb_frontend
module Cn = CF.Cn
module SBT = BaseTypes.Surface
module BT = BaseTypes
module Def = Definition
module IT = IndexTerms
module LAT = LogicalArgumentTypes
module LRT = LogicalReturnTypes
module LC = LogicalConstraints
module Req = Request
module Mu = Mucore
module RT = ReturnTypes
module STermMap = Map.Make (IndexTerms.Surface)
module StringMap = Map.Make (String)
open Pp.Infix

type[@warning "-69" (* unused-record-field *)] function_sig =
  { args : (Sym.t * BaseTypes.t) list;
      (* FIXME either delete this field, or explain why we don't need it *)
    return_bty : BaseTypes.t
  }

type predicate_sig =
  { pred_iargs : (Sym.t * BaseTypes.t) list;
    pred_output : BaseTypes.t
  }

type 'a cerb_frontend_result =
  ('a, Locations.t * Cerb_frontend.Errors.cause) Cerb_frontend.Exception.exceptM

type env =
  { computationals : (SBT.t * Sym.t option) Sym.Map.t;
    logicals : SBT.t Sym.Map.t;
    predicates : predicate_sig Sym.Map.t;
    functions : function_sig Sym.Map.t;
    datatypes : BaseTypes.dt_info Sym.Map.t;
    datatype_constrs : BaseTypes.constr_info Sym.Map.t;
    tagDefs : (Cerb_frontend.Symbol.sym, Mu.tag_definition) Pmap.map;
    fetch_enum_expr :
      Locations.t -> Sym.t -> unit CF.AilSyntax.expression cerb_frontend_result;
    fetch_typedef : Locations.t -> Sym.t -> CF.Ctype.ctype cerb_frontend_result
  }

let init tagDefs fetch_enum_expr fetch_typedef =
  let alloc_sig = { pred_iargs = []; pred_output = Definition.alloc.oarg_bt } in
  let builtins =
    List.fold_left
      (fun acc (_, sym, (def : Definition.Function.t)) ->
         let fsig = { args = def.args; return_bty = def.return_bt } in
         Sym.Map.add sym fsig acc)
      Sym.Map.empty
      Builtins.builtin_fun_defs
  in
  { computationals = Sym.Map.empty;
    logicals = Sym.Map.(empty |> add Alloc.History.sym Alloc.History.sbt);
    predicates = Sym.Map.(empty |> add Alloc.Predicate.sym alloc_sig);
    functions = builtins;
    datatypes = Sym.Map.empty;
    datatype_constrs = Sym.Map.empty;
    tagDefs;
    fetch_enum_expr;
    fetch_typedef
  }


let pointer_eq_warned = ref false

(* FIXME: this is an ugly hack used by Core_to_mucore.collect_instrumentation
 * which in turn is used by Executable_spec.main and TestGeneration.run *)
module SymTable = Hashtbl.Make (Sym)

let exec_spec_hack_syms = SymTable.create 10000

let add_computational sym bTy env =
  SymTable.add exec_spec_hack_syms sym bTy;
  { env with computationals = Sym.Map.add sym (bTy, None) env.computationals }


let add_renamed_computational sym sym2 bTy env =
  SymTable.add exec_spec_hack_syms sym bTy;
  { env with computationals = Sym.Map.add sym (bTy, Some sym2) env.computationals }


let add_logical sym bTy env =
  SymTable.add exec_spec_hack_syms sym bTy;
  { env with logicals = Sym.Map.add sym bTy env.logicals }


let add_predicate sym pred_sig env =
  { env with predicates = Sym.Map.add sym pred_sig env.predicates }


let lookup_computational_or_logical sym env =
  match Sym.Map.find_opt sym env.logicals with
  | Some bt -> Some (bt, None)
  | None -> Sym.Map.find_opt sym env.computationals


let lookup_predicate sym env = Sym.Map.find_opt sym env.predicates

let lookup_function sym env = Sym.Map.find_opt sym env.functions

let lookup_struct_opt sym env =
  match Pmap.lookup sym env.tagDefs with
  | Some (StructDef xs) -> Some xs
  | Some UnionDef | None -> None


let add_datatype sym info env =
  let datatypes = Sym.Map.add sym info env.datatypes in
  { env with datatypes }


let add_datatype_constr sym info env =
  let datatype_constrs = Sym.Map.add sym info env.datatype_constrs in
  { env with datatype_constrs }


let big_union = List.fold_left Sym.Set.union Sym.Set.empty

let rec bound_by_pattern (Cn.CNPat (_loc, pat_)) =
  match pat_ with
  | CNPat_sym s -> Sym.Set.singleton s
  | CNPat_wild -> Sym.Set.empty
  | CNPat_constructor (_, args) ->
    big_union (List.map (fun (_, p) -> bound_by_pattern p) args)


let rec free_in_expr (Cn.CNExpr (_loc, expr_)) =
  match expr_ with
  | CNExpr_const _ -> Sym.Set.empty
  | CNExpr_var v -> Sym.Set.singleton v
  | CNExpr_list es -> free_in_exprs es
  | CNExpr_memberof (e, _id) -> free_in_expr e
  | CNExpr_arrow (e, _id) -> free_in_expr e
  | CNExpr_record members -> free_in_exprs (List.map snd members)
  | CNExpr_struct (_tag, members) -> free_in_exprs (List.map snd members)
  | CNExpr_memberupdates (e, updates) -> free_in_exprs (e :: List.map snd updates)
  | CNExpr_arrayindexupdates (e, updates) ->
    free_in_exprs (e :: List.concat_map (fun (e1, e2) -> [ e1; e2 ]) updates)
  | CNExpr_binop (_binop, e1, e2) -> free_in_exprs [ e1; e2 ]
  | CNExpr_sizeof _ -> Sym.Set.empty
  | CNExpr_offsetof _ -> Sym.Set.empty
  | CNExpr_array_shift (e1, _ct, e2) -> free_in_exprs [ e1; e2 ]
  | CNExpr_membershift (e, _opt_tag, _id) -> free_in_expr e
  | CNExpr_addr _ -> Sym.Set.empty
  | CNExpr_cast (_bt, e) -> free_in_expr e
  | CNExpr_call (_id, es) -> free_in_exprs es
  | CNExpr_cons (_c, args) -> free_in_exprs (List.map snd args)
  | CNExpr_each (s, _bt, _range, e) -> Sym.Set.remove s (free_in_expr e)
  | CNExpr_match (x, ms) ->
    let free_per_case =
      List.map
        (fun (pat, body) -> Sym.Set.diff (free_in_expr body) (bound_by_pattern pat))
        ms
    in
    Sym.Set.union (free_in_expr x) (big_union free_per_case)
  | CNExpr_let (s, e, body) ->
    Sym.Set.union (free_in_expr e) (Sym.Set.remove s (free_in_expr body))
  | CNExpr_ite (e1, e2, e3) -> free_in_exprs [ e1; e2; e3 ]
  | CNExpr_good (_typ, e) -> free_in_expr e
  | CNExpr_deref e -> free_in_expr e
  | CNExpr_value_of_c_atom (s, _) -> Sym.Set.singleton s
  | CNExpr_unchanged e -> free_in_expr e
  | CNExpr_at_env (e, _evaluation_scope) -> free_in_expr e
  | CNExpr_not e -> free_in_expr e
  | CNExpr_bnot e -> free_in_expr e
  | CNExpr_negate e -> free_in_expr e
  | CNExpr_default _bt -> Sym.Set.empty


and free_in_exprs es = big_union (List.map free_in_expr es)

let rec base_type env (bTy : _ Cn.cn_base_type) =
  let self bTy = base_type env bTy in
  match bTy with
  | CN_unit -> BT.Unit
  | CN_bool -> Bool
  | CN_integer -> Integer
  | CN_bits (CN_unsigned, n) -> Bits (Unsigned, n)
  | CN_bits (CN_signed, n) -> Bits (Signed, n)
  | CN_real -> Real
  | CN_loc -> Loc None
  | CN_alloc_id -> Alloc_id
  | CN_struct tag_sym -> Struct tag_sym
  | CN_record members -> Record (List.map (fun (m, bt) -> (m, self bt)) members)
  | CN_datatype dt_sym -> Datatype dt_sym
  | CN_map (bTy1, bTy2) -> Map (self bTy1, self bTy2)
  | CN_list bTy' -> List (self bTy')
  | CN_tuple bTys -> Tuple (List.map self bTys)
  | CN_set bTy' -> Set (self bTy')
  | CN_user_type_name _ ->
    failwith "user type-abbreviation not removed by cabs->ail elaboration"
  | CN_c_typedef_name sym ->
    (* FIXME handle errors here with CN mechanisms *)
    let here = Locations.other __LOC__ in
    (match env.fetch_typedef here sym with
     | CF.Exception.Result r -> Memory.sbt_of_sct (Sctypes.of_ctype_unsafe here r)
     | CF.Exception.Exception (_loc, msg) -> failwith (CF.Pp_errors.short_message msg))


let add_predicates env defs =
  let aux env (def : _ Cn.cn_predicate) =
    let iargs =
      List.map
        (fun (id_or_sym, bTy) -> (id_or_sym, SBT.proj (base_type env bTy)))
        def.cn_pred_iargs
    in
    let output = SBT.proj (base_type env (snd def.cn_pred_output)) in
    add_predicate def.cn_pred_name { pred_iargs = iargs; pred_output = output } env
  in
  List.fold_left aux env defs


type message =
  | Builtins of Builtins.message
  | Global of Global.message
  | WellTyped of WellTyped.message
  | Cannot_convert_enum_const of Cerb_frontend.AilSyntax.constant
  | Cannot_convert_enum_expr of unit Cerb_frontend.AilSyntax.expression
  | Cerb_frontend of Locations.t * Cerb_frontend.Errors.cause
  | Illtyped_binary_it of
      { left : IndexTerms.Surface.t;
        right : IndexTerms.Surface.t;
        binop : Cerb_frontend.Cn.cn_binop
      }
  | First_iarg_missing
  | First_iarg_not_pointer of
      { pname : Request.name;
        found_bty : BaseTypes.t
      }
  | Datatype_repeated_member of Id.t
  | No_pointee_ctype of IndexTerms.Surface.t
  | Each_quantifier_not_numeric of Sym.t * BaseTypes.Surface.t
  | Generic of Pp.document [@deprecated "Temporary, for refactor, to be deleted."]

type err =
  { loc : Locations.t;
    msg : message
  }

module Or_Error = struct
  type 'a t = ('a, err) Result.t

  let bind = Result.bind

  let return = Result.ok

  let fail = Result.error
end

open Effectful.Make (Or_Error)

open Or_Error

(* TODO: handle more kinds of constant expression *)
let convert_enum_expr =
  let open CF.AilSyntax in
  let conv_const loc = function
    | ConstantInteger (IConstant (z, _, _)) as c ->
      let@ bt =
        match BT.pick_integer_encoding_type z with
        | Some bt -> return bt
        | None -> fail { loc; msg = Cannot_convert_enum_const c }
      in
      return (IT.Surface.inj (IT.num_lit_ z bt loc))
    | c -> fail { loc; msg = Cannot_convert_enum_const c }
  in
  let rec conv_expr_ e1 loc = function
    | AilEconst const -> conv_const loc const
    | AilEannot (_cty, expr) -> conv_expr expr
    | _ -> fail { loc; msg = Cannot_convert_enum_expr e1 }
  and conv_expr e =
    match e with AnnotatedExpression (_, _, loc, expr) -> conv_expr_ e loc expr
  in
  conv_expr


let do_decode_enum env loc sym =
  (* FIXME handle errors with CN mechanisms *)
  match env.fetch_enum_expr loc sym with
  | CF.Exception.Result expr -> convert_enum_expr expr
  | CF.Exception.Exception (_loc, msg) -> failwith (CF.Pp_errors.short_message msg)


let add_function _loc sym func_sig env =
  return { env with functions = Sym.Map.add sym func_sig env.functions }


let add_user_defined_functions env defs =
  let aux env (def : _ Cn.cn_function) =
    let args =
      List.map (fun (sym, bTy) -> (sym, SBT.proj (base_type env bTy))) def.cn_func_args
    in
    let return_bt = SBT.proj (base_type env def.cn_func_return_bty) in
    let fsig = { args; return_bty = return_bt } in
    add_function def.cn_func_loc def.cn_func_name fsig env
  in
  ListM.fold_leftM aux env defs


let add_datatype_info env (dt : _ Cn.cn_datatype) =
  Pp.debug 2 (lazy (Pp.item "translating datatype declaration" (Sym.pp dt.cn_dt_name)));
  (* SMT format constraints seem to require variables to be unique to the
     datatype, not just the constructor. *)
  let add_param m (nm, ty) =
    match StringMap.find_opt (Id.get_string nm) m with
    | None ->
      return (StringMap.add (Id.get_string nm) (nm, SBT.proj (base_type env ty)) m)
    | Some _ -> fail { loc = Id.get_loc nm; msg = Datatype_repeated_member nm }
  in
  let@ all_params =
    ListM.fold_leftM add_param StringMap.empty (List.concat_map snd dt.cn_dt_cases)
  in
  let add_constr env (cname, params) =
    let params = List.map (fun (nm, ty) -> (nm, SBT.proj (base_type env ty))) params in
    let info = BT.Datatype.{ params; datatype_tag = dt.cn_dt_name } in
    add_datatype_constr cname info env
  in
  let env = List.fold_left add_constr env dt.cn_dt_cases in
  let all_params = List.map snd (StringMap.bindings all_params) in
  let constrs = List.map fst dt.cn_dt_cases in
  return (add_datatype dt.cn_dt_name BT.Datatype.{ constrs; all_params } env)


let add_datatypes env dts = ListM.fold_leftM add_datatype_info env dts

module C_vars = struct
  (* the expression that encodes the current value of this c variable *)
  type state =
    | Value of Sym.t * SBT.t (* currently the variable is a pure value, this one *)
    | Points_to of IT.Surface.t
  (* currently the variable is a pointer to memory holding this value *)

  type scope =
    { var_state : state Sym.Map.t;
      pointee_values : IT.Surface.t STermMap.t
    }

  let empty_state = { var_state = Sym.Map.empty; pointee_values = STermMap.empty }

  type name = string

  let start = "start"

  type named_scopes = scope StringMap.t

  type env =
    { current : scope;
      old : named_scopes
    }

  let init = { current = empty_state; old = StringMap.empty }

  let get_old_scopes { current = _; old } = old

  let push_scope { current; old } old_name =
    { current = empty_state; old = StringMap.add old_name current old }


  let add c_sym cvs { current; old } =
    { current = { current with var_state = Sym.Map.add c_sym cvs current.var_state };
      old
    }


  let add_pointee_value p v { current; old } =
    { current = { current with pointee_values = STermMap.add p v current.pointee_values };
      old
    }


  let add cvss st = List.fold_left (fun st (sym, cvs) -> add sym cvs st) st cvss

  let add_pointee_values pvs st =
    List.fold_left (fun st (p, v) -> add_pointee_value p v st) st pvs


  type 'a t =
    | Done of 'a
    | Error of err
    | ScopeExists of Locations.t * name * (bool -> 'a t)
    | Value_of_c_variable of
        Locations.t * Sym.t * name option * (IT.Surface.t option -> 'a t)
    | Deref of Locations.t * IT.Surface.t * name option * (IT.Surface.t option -> 'a t)

  module Monad = struct
    type nonrec 'a t = 'a t

    let return x = Done x

    let rec bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
      match m with
      | Done x -> f x
      | Error err -> Error err
      | ScopeExists (loc, scope, k) -> ScopeExists (loc, scope, fun b -> bind (k b) f)
      | Value_of_c_variable (loc, s, scope, k) ->
        Value_of_c_variable (loc, s, scope, fun it_o -> bind (k it_o) f)
      | Deref (loc, it, scope, k) -> Deref (loc, it, scope, fun it_o -> bind (k it_o) f)


    let fail e = Error e
  end

  let scope_exists loc scope = ScopeExists (loc, scope, fun b -> Done b)

  let deref loc it scope = Deref (loc, it, scope, fun o_v_it -> Done o_v_it)

  let value_of_c_variable loc sym scope =
    Value_of_c_variable (loc, sym, scope, fun o_v_it -> Done o_v_it)


  let liftResult = function Result.Ok a -> Done a | Result.Error e -> Error e

  open Effectful.Make (Monad)

  open Monad

  let pp_in_scope = function
    | Some scope -> !^"in evaluation scope" ^^^ Pp.squotes !^scope
    | None -> !^"in current scope"


  let lookup_struct loc tag env =
    match lookup_struct_opt tag env with
    | Some def -> return def
    | None -> fail { loc; msg = Global (Unknown_struct tag) }


  let lookup_member loc (_tag, def) member =
    let member_types = Memory.member_types def in
    match List.assoc_opt Id.equal member member_types with
    | Some ty -> return ty
    | None ->
      fail { loc; msg = Global (Unexpected_member (List.map fst member_types, member)) }


  let lookup_constr loc sym env =
    match Sym.Map.find_opt sym env.datatype_constrs with
    | Some info -> return info
    | None -> fail { loc; msg = Global (Unknown_datatype_constr sym) }


  (* TODO: type checks and disambiguation at this stage seems ill-advised,
     ideally would be integrated into wellTyped.ml *)
  let mk_binop loc bop (e1, e2) =
    let open IndexTerms in
    match (bop, get_bt e1) with
    | Cn.CN_add, (BT.Integer | Real | Bits _) ->
      return (IT (Binop (Add, e1, e2), get_bt e1, loc))
    | CN_add, Loc oct ->
      (match oct with
       | Some ct ->
         let (IT (it_, _, _)) =
           Surface.inj
             (arrayShift_ ~base:(Surface.proj e1) ct ~index:(Surface.proj e2) loc)
         in
         return (IT (it_, Loc oct, loc))
       | None -> fail { loc; msg = No_pointee_ctype e1 })
    | CN_sub, (Integer | Real | Bits _) ->
      return (IT (Binop (Sub, e1, e2), get_bt e1, loc))
    | CN_sub, Loc oct ->
      (match oct with
       | Some ct ->
         let here = Locations.other __LOC__ in
         let (IT (it_, _, _)) =
           Surface.inj
             (arrayShift_
                ~base:(Surface.proj e1)
                ct
                ~index:(sub_ (int_ 0 here, Surface.proj e2) here)
                loc)
         in
         return (IT (it_, Loc oct, loc))
       | None -> fail { loc; msg = No_pointee_ctype e1 })
    | CN_mul, _ -> return (IT (Binop (Mul, e1, e2), get_bt e1, loc))
    | CN_div, _ -> return (IT (Binop (Div, e1, e2), get_bt e1, loc))
    | CN_mod, _ -> return (IT (Binop (Rem, e1, e2), get_bt e1, loc))
    | CN_equal, _ ->
      (match (get_bt e1, get_bt e2, !pointer_eq_warned) with
       | Loc _, Loc _, false ->
         pointer_eq_warned := true;
         Pp.warn
           loc
           !^"CN pointer equality is not the same as C's (will not warn again). Please \
              use `ptr_eq` or `is_null` (maybe `addr_eq`)."
       | _, _, _ -> ());
      return (IT (Binop (EQ, e1, e2), BT.Bool, loc))
    | CN_inequal, _ ->
      (match (get_bt e1, get_bt e2, !pointer_eq_warned) with
       | Loc _, Loc _, false ->
         pointer_eq_warned := true;
         Pp.warn
           loc
           !^"CN pointer equality is not the same as C's (will not warn again). Please \
              use `ptr_eq` or `is_null` (maybe `addr_eq`)."
       | _, _, _ -> ());
      return (not_ (IT (Binop (EQ, e1, e2), BT.Bool, loc)) loc)
    | CN_lt, (BT.Integer | BT.Real | BT.Bits _) ->
      return (IT (Binop (LT, e1, e2), BT.Bool, loc))
    | CN_lt, BT.Loc _ -> return (IT (Binop (LTPointer, e1, e2), BT.Bool, loc))
    | CN_le, (BT.Integer | BT.Real | BT.Bits _) ->
      return (IT (Binop (LE, e1, e2), BT.Bool, loc))
    | CN_le, BT.Loc _ -> return (IT (Binop (LEPointer, e1, e2), BT.Bool, loc))
    | CN_gt, (BT.Integer | BT.Real | BT.Bits _) ->
      return (IT (Binop (LT, e2, e1), BT.Bool, loc))
    | CN_gt, BT.Loc _ -> return (IT (Binop (LTPointer, e2, e1), BT.Bool, loc))
    | CN_ge, (BT.Integer | BT.Real | BT.Bits _) ->
      return (IT (Binop (LE, e2, e1), BT.Bool, loc))
    | CN_ge, BT.Loc _ -> return (IT (Binop (LEPointer, e2, e1), BT.Bool, loc))
    | CN_or, BT.Bool -> return (IT (Binop (Or, e1, e2), BT.Bool, loc))
    | CN_and, BT.Bool -> return (IT (Binop (And, e1, e2), BT.Bool, loc))
    | CN_implies, BT.Bool -> return (IT (Binop (Implies, e1, e2), BT.Bool, loc))
    | CN_map_get, _ ->
      let@ rbt =
        match get_bt e1 with
        | Map (_, rbt) -> return rbt
        | has ->
          let expected = "map/array" in
          let reason = "map/array index" in
          fail
            { loc;
              msg =
                WellTyped
                  (Illtyped_it { it = Terms.pp e1; has = SBT.pp has; expected; reason })
            }
      in
      return (IT (MapGet (e1, e2), rbt, loc))
    | CN_band, (BT.Bits _ as bt) -> return (IT (Binop (BW_And, e1, e2), bt, loc))
    | CN_bor, (BT.Bits _ as bt) -> return (IT (Binop (BW_Or, e1, e2), bt, loc))
    | CN_bxor, (BT.Bits _ as bt) -> return (IT (Binop (BW_Xor, e1, e2), bt, loc))
    | _ -> fail { loc; msg = Illtyped_binary_it { left = e1; right = e2; binop = bop } }


  (* just copy-pasting and adapting Kayvan's older version of this code *)
  let member_access loc env (t : IT.Surface.t) member =
    match IT.get_bt t with
    | BT.Record members ->
      let@ member_bt =
        match List.assoc_opt Id.equal member members with
        | Some member_bt -> return member_bt
        | None ->
          fail { loc; msg = Global (Unexpected_member (List.map fst members, member)) }
      in
      return (IT.recordMember_ ~member_bt (t, member) loc)
    | Struct tag ->
      let@ defs_ = lookup_struct loc tag env in
      let@ ty = lookup_member loc (tag, defs_) member in
      let member_bt = Memory.sbt_of_sct ty in
      return (IT.IT (StructMember (t, member), member_bt, loc))
    | has ->
      let expected = "struct" in
      let reason = "struct member access" in
      fail
        { loc;
          msg =
            WellTyped
              (Illtyped_it
                 { it = Terms.pp t; has = BaseTypes.Surface.pp has; expected; reason })
        }


  let rec cn_pat env locally_bound (Cn.CNPat (loc, pat_), bt) =
    match pat_ with
    | CNPat_wild -> return (env, locally_bound, IT.Pat (PWild, bt, loc))
    | CNPat_sym s ->
      let env' = add_logical s bt env in
      let locally_bound' = Sym.Set.add s locally_bound in
      return (env', locally_bound', IT.Pat (PSym s, bt, loc))
    | CNPat_constructor (cons, args) ->
      let@ cons_info = lookup_constr loc cons env in
      let@ env', locally_bound', args =
        ListM.fold_leftM
          (fun (env, locally_bound, acc) (m, pat') ->
             match List.assoc_opt Id.equal m cons_info.params with
             | None ->
               fail
                 { loc;
                   msg = Global (Unexpected_member (List.map fst cons_info.params, m))
                 }
             | Some mbt ->
               let@ env', locally_bound', pat' =
                 cn_pat env locally_bound (pat', SBT.inj mbt)
               in
               return (env', locally_bound', acc @ [ (m, pat') ]))
          (env, locally_bound, [])
          args
      in
      return (env', locally_bound', IT.Pat (PConstructor (cons, args), bt, loc))


  let check_quantified_base_type env loc sym bt =
    let bt = base_type env bt in
    if SBT.equal bt BT.Integer then
      return bt
    else if Option.is_some (SBT.is_bits_bt bt) then
      return bt
    else
      fail { loc; msg = Each_quantifier_not_numeric (sym, bt) }


  let warn_if_ct_mismatch loc context ~annot ~inferred ~ptr =
    match (inferred, annot) with
    | Sctypes.Void, _ (* this case will error later in the checks *)
    | _, Sctypes.(Integer IntegerTypes.Char)
    | _, Sctypes.(Integer (Signed Ichar))
    | _, Sctypes.(Integer (Unsigned Ichar)) ->
      ()
    | _ ->
      let pred_name =
        match context with `RW -> "RW" | `W -> "W" | `Array_shift -> "array_shift"
      in
      if not (Sctypes.equal annot inferred) then
        Pp.warn
          loc
          (!^"annotation on"
           ^^^ !^pred_name
           ^^^ !^"suggests"
           ^^^ IT.pp ptr
           ^^^ !^"has type"
           ^^^ Sctypes.pp (Sctypes.Pointer annot)
           ^^^ !^"but it has type"
           ^^^ Sctypes.pp (Sctypes.Pointer inferred)
           ^^ Pp.dot)


  let infer_scty ~pred_loc:loc ~ptr (context : [ `RW | `W | `Array_shift ]) oty =
    let context_str =
      match context with `RW -> "RW" | `W -> "W" | `Array_shift -> "array_shift"
    in
    match (oty, IT.get_bt ptr) with
    | Some annot, BT.Loc (Some inferred) ->
      let annot = Sctypes.of_ctype_unsafe loc annot in
      warn_if_ct_mismatch loc context ~annot ~inferred ~ptr;
      return annot
    | Some annot, BT.Loc None ->
      let annot = Sctypes.of_ctype_unsafe loc annot in
      return annot
    | None, BT.Loc (Some inferred) -> return inferred
    | None, Loc None ->
      fail
        { loc;
          msg =
            Generic
              (!^"Cannot tell C-type of pointer. Please use "
               ^^^ !^context_str
               ^^^ !^"<CTYPE>'.")
            [@alert "-deprecated"]
        }
    | _, has ->
      let expected = "pointer" in
      let reason = context_str ^ "<_>" in
      fail
        { loc;
          msg =
            WellTyped
              (Illtyped_it { it = Terms.pp ptr; has = SBT.pp has; expected; reason })
        }


  let cn_expr =
    let open IndexTerms in
    let module BT = BaseTypes in
    let rec trans
              (evaluation_scope : string option)
              locally_bound
              env
              (Cn.CNExpr (loc, expr_))
      : IndexTerms.Surface.t Monad.t
      =
      let self = trans evaluation_scope locally_bound env in
      match expr_ with
      | CNExpr_const CNConst_NULL -> return (IT (Const Null, BT.Loc None, loc))
      | CNExpr_const (CNConst_integer n) -> return (IT (Const (Z n), BT.Integer, loc))
      | CNExpr_const (CNConst_bits ((sign, n), v)) ->
        let sign =
          match sign with CN_unsigned -> BT.Unsigned | CN_signed -> BT.Signed
        in
        return (IT (Const (Bits ((sign, n), v)), BT.Bits (sign, n), loc))
      | CNExpr_const (CNConst_bool b) -> return (IT (Const (Bool b), BT.Bool, loc))
      | CNExpr_const CNConst_unit -> return (IT (Const Unit, BT.Unit, loc))
      | CNExpr_var sym ->
        let@ sym, bTy =
          match lookup_computational_or_logical sym env with
          | None ->
            Pp.debug
              2
              (lazy
                (Pp.item
                   ("failed lookup of CNExpr_var " ^ Sym.pp_string sym)
                   (Pp.list
                      (fun (nm, _) -> Sym.pp nm)
                      (Sym.Map.bindings env.computationals))));
            fail { loc; msg = WellTyped (Unknown_variable sym) }
          | Some (bt, None) -> return (sym, bt)
          | Some (bt, Some renamed_sym) -> return (renamed_sym, bt)
        in
        return (IT (Sym sym, bTy, loc))
      | CNExpr_list es ->
        let@ es = ListM.mapM self es in
        let item_bt = get_bt (List.hd es) in
        let _, nil_pos, _ =
          (* parser should ensure loc is a region *)
          Option.get @@ Locations.get_region loc
        in
        let cons hd tl =
          let hd_pos = Option.get @@ Locations.start_pos @@ IT.get_loc hd in
          let loc = Locations.(region (hd_pos, nil_pos) NoCursor) in
          IT (Cons (hd, tl), BT.List item_bt, loc)
        in
        let nil =
          let nil_loc = Cerb_location.point nil_pos in
          IT (Nil (SBT.proj item_bt), BT.List item_bt, nil_loc)
        in
        return (List.fold_right cons es nil)
      | CNExpr_memberof (e, xs) ->
        let@ e = self e in
        member_access loc env e xs
      | CNExpr_arrow (e, xs) ->
        (* Desugar a->b as ( *a).b *)
        let@ e = self (CNExpr (loc, CNExpr_deref e)) in
        member_access loc env e xs
      | CNExpr_record members ->
        let@ members = ListM.mapsndM self members in
        let bts = List.map_snd IT.get_bt members in
        return (IT (IT.Record members, BT.Record bts, loc))
      | CNExpr_struct (tag, members) ->
        let@ members = ListM.mapsndM self members in
        return (IT (IT.Struct (tag, members), BT.Struct tag, loc))
      | CNExpr_memberupdates (e, updates) ->
        let@ e = self e in
        let bt = IT.get_bt e in
        let end_pos = Option.get @@ Locations.end_pos loc in
        (match IT.get_bt e with
         | Struct _ ->
           let@ expr =
             ListM.fold_rightM
               (fun (id, v) expr ->
                  let@ v = self v in
                  let start_pos = Option.get @@ Locations.start_pos @@ Id.get_loc id in
                  let cursor = Cerb_location.PointCursor start_pos in
                  let loc = Locations.region (start_pos, end_pos) cursor in
                  return (IT (StructUpdate ((expr, id), v), bt, loc)))
               updates
               e
           in
           return expr
         | _ ->
           fail
             { loc = IT.get_loc e;
               msg =
                 WellTyped
                   (Illtyped_it
                      { it = Terms.pp e;
                        has = SBT.pp bt;
                        expected = "struct";
                        reason =
                          (let head, pos = Locations.head_pos_of_location loc in
                           head ^ "\n" ^ pos)
                      })
             })
      | CNExpr_arrayindexupdates (e, updates) ->
        let@ e = self e in
        let bt = IT.get_bt e in
        (* start_pos points to start_pos of e ignored cursor points to '[' end_pos points
           to ']' *)
        let start_pos, end_pos, _ =
          (* parser should ensure loc is a region *)
          Option.get @@ Locations.get_region loc
        in
        let@ (IT (e, bt, loc)) =
          ListM.fold_leftM
            (fun acc (i, v) ->
               let@ i = self i in
               let@ v = self v in
               let end_pos = Option.get @@ Locations.end_pos @@ IT.get_loc v in
               (* cursor for the first update doesn't point to '[' - oh well *)
               let cursor =
                 Cerb_location.PointCursor
                   (Option.get @@ Locations.start_pos @@ IT.get_loc i)
               in
               return
                 (IT
                    ( MapSet (acc, i, v),
                      bt,
                      Cerb_location.region (start_pos, end_pos) cursor )))
            e
            updates
        in
        (* cursor points to start of last index ignored start_pos is just start_pos as
           above ignored end_pos points to the end last value we want to restore it to
           point to ']' *)
        let _, _, cursor = Option.get @@ Locations.get_region loc in
        return (IT (e, bt, Locations.region (start_pos, end_pos) cursor))
      | CNExpr_binop (bop, e1_, e2_) ->
        let@ e1 = self e1_ in
        let@ e2 = self e2_ in
        mk_binop loc bop (e1, e2)
      | CNExpr_sizeof ct ->
        let scty = Sctypes.of_ctype_unsafe loc ct in
        return (IT (SizeOf scty, Memory.size_sbt, loc))
      | CNExpr_offsetof (tag, member) ->
        let@ _ = lookup_struct loc tag env in
        return (IT (OffsetOf (tag, member), Memory.sint_sbt, loc))
      | CNExpr_array_shift (base, ty_annot, index) ->
        let@ base = self base in
        let@ ct = infer_scty ~pred_loc:loc ~ptr:base `Array_shift ty_annot in
        (match IT.get_bt base with
         | Loc _ ->
           let@ index = self index in
           (match IT.get_bt index with
            | Integer | Bits _ ->
              return (IT (ArrayShift { base; ct; index }, Loc (Some ct), loc))
            | has ->
              let expected = "integer or bits" in
              let reason = "pointer arithmetic" in
              fail
                { loc;
                  msg =
                    WellTyped
                      (Illtyped_it
                         { it = Terms.pp index; has = SBT.pp has; expected; reason })
                })
         | has ->
           let expected = "pointer" in
           let reason = "pointer arithmetic" in
           fail
             { loc;
               msg =
                 WellTyped
                   (Illtyped_it { it = Terms.pp base; has = SBT.pp has; expected; reason })
             })
      | CNExpr_membershift (e, opt_tag, member) ->
        let@ e = self e in
        let with_tag tag =
          let@ struct_def = lookup_struct loc tag env in
          let@ member_ty = lookup_member loc (tag, struct_def) member in
          let (IT (it_, _, _)) =
            Surface.inj (memberShift_ (Surface.proj e, tag, member) loc)
          in
          (* Surface.inj will not have produced a C-type-annotated bt. So stick that on
             now. *)
          return (IT (it_, Loc (Some member_ty), loc))
        in
        (match (opt_tag, IT.get_bt e) with
         | Some (Ctype (_, Struct tag)), Loc (Some (Struct tag')) ->
           if Sym.equal tag tag' then
             with_tag tag
           else (
             let expected = Pp.plain @@ SBT.pp (Struct tag) in
             let reason = "struct member offset" in
             fail
               { loc;
                 msg =
                   WellTyped
                     (Illtyped_it
                        { it = Terms.pp e; has = SBT.pp (Struct tag'); expected; reason })
               })
         | Some (Ctype (_, Struct tag)), Loc None | None, Loc (Some (Struct tag)) ->
           with_tag tag
         | None, Loc None -> fail { loc; msg = No_pointee_ctype e }
         | _, has ->
           let expected = "struct pointer" in
           let reason = "struct member offset" in
           fail
             { loc;
               msg =
                 WellTyped
                   (Illtyped_it { it = Terms.pp e; has = SBT.pp has; expected; reason })
             })
      | CNExpr_addr nm -> return (sym_ (nm, BT.Loc None, loc))
      | CNExpr_cast (bt, expr) ->
        let@ expr = self expr in
        let bt = base_type env bt in
        return (IT (Cast (SBT.proj bt, expr), bt, loc))
      | CNExpr_call (fsym, exprs) ->
        let@ args = ListM.mapM self exprs in
        let@ b =
          liftResult
            (Result.map_error
               (fun Builtins.{ loc; msg } -> ({ loc; msg = Builtins msg } : err))
               (Builtins.apply_builtin_funs fsym args loc))
        in
        (match b with
         | Some t -> return t
         | None ->
           let@ bt =
             match lookup_function fsym env with
             | Some fsig -> return fsig.return_bty
             | None ->
               fail
                 { loc;
                   msg = Global (Unknown_logical_function { id = fsym; resource = false })
                 }
           in
           return (apply_ fsym args (BaseTypes.Surface.inj bt) loc))
      | CNExpr_cons (c_nm, exprs) ->
        let@ cons_info = lookup_constr loc c_nm env in
        let@ exprs =
          ListM.mapM
            (fun (nm, expr) ->
               let@ expr = self expr in
               return (nm, expr))
            exprs
        in
        return (IT (Constructor (c_nm, exprs), BT.Datatype cons_info.datatype_tag, loc))
      | CNExpr_each (sym, bt, r, e) ->
        let@ expr =
          trans
            evaluation_scope
            (Sym.Set.add sym locally_bound)
            (add_logical sym BT.Integer env)
            e
        in
        let@ bt = check_quantified_base_type env loc sym bt in
        return
          (IT
             ( EachI ((Z.to_int (fst r), (sym, SBT.proj bt), Z.to_int (snd r)), expr),
               BT.Bool,
               loc ))
      | CNExpr_match (x, ms) ->
        let@ x = self x in
        let@ ms =
          ListM.mapM
            (fun (pat, body) ->
               let@ env', locally_bound', pat =
                 cn_pat env locally_bound (pat, IT.get_bt x)
               in
               let@ body = trans evaluation_scope locally_bound' env' body in
               return (pat, body))
            ms
        in
        let rbt = IT.get_bt (snd (List.hd ms)) in
        return (IT (Match (x, ms), rbt, loc))
      | CNExpr_let (s, e, body) ->
        let@ e = self e in
        let@ body =
          trans
            evaluation_scope
            (Sym.Set.add s locally_bound)
            (add_logical s (IT.get_bt e) env)
            body
        in
        return (IT (Let ((s, e), body), IT.get_bt body, loc))
      | CNExpr_ite (e1, e2, e3) ->
        let@ e1 = self e1 in
        let@ e2 = self e2 in
        let@ e3 = self e3 in
        return (ite_ (e1, e2, e3) loc)
      | CNExpr_good (ty, e) ->
        let scty = Sctypes.of_ctype_unsafe loc ty in
        let@ e = self e in
        return (IT (Good (scty, e), BT.Bool, loc))
      | CNExpr_not e ->
        let@ e = self e in
        return (not_ e loc)
      | CNExpr_bnot e ->
        let@ e = self e in
        return (bw_compl_ e loc)
      | CNExpr_negate e ->
        let@ e = self e in
        (match e with
         | IT (Const (Z z), _bt, _) -> return (IT (Const (Z (Z.neg z)), BT.Integer, loc))
         | IT (Const (Bits ((sign, width), z)), _bt, _) ->
           (* this will be checked to fit in WellTyped.infer *)
           return (IT (Const (Bits ((sign, width), Z.neg z)), BT.Bits (sign, width), loc))
         | _ -> return (negate e loc))
      | CNExpr_default bt ->
        let bt = base_type env bt in
        return (IT (Const (Default (SBT.proj bt)), bt, loc))
      | CNExpr_unchanged e ->
        let@ cur_e = self e in
        let@ old_e = self (CNExpr (loc, CNExpr_at_env (e, start))) in
        (* want to bypass the warning for (Loc, Loc) equality *)
        (* mk_binop loc CN_equal (cur_e, old_e) *)
        return (IT (Binop (EQ, cur_e, old_e), BT.Bool, loc))
      | CNExpr_at_env (e, scope) ->
        let@ () =
          match evaluation_scope with
          | None -> return ()
          | Some _ ->
            fail
              { loc;
                msg = Generic !^"Cannot nest evaluation scopes." [@alert "-deprecated"]
              }
        in
        let@ scope_exists = scope_exists loc scope in
        (* let@ () = match StringMap.mem scope env.old_states with *)
        let@ () =
          match scope_exists with
          | true -> return ()
          | false ->
            fail
              { loc;
                msg =
                  Generic !^("Unknown evaluation scope '" ^ scope ^ "'.")
                  [@alert "-deprecated"]
              }
        in
        trans (Some scope) locally_bound env e
      | CNExpr_deref e ->
        let@ () =
          let locally_bound_in_e = Sym.Set.inter (free_in_expr e) locally_bound in
          match Sym.Set.elements locally_bound_in_e with
          | [] -> return ()
          | s :: _ ->
            let msg =
              Sym.pp s
              ^^^ !^"is locally bound in this expression."
              ^/^ !^"Cannot dereference a pointer expression containing a locally bound \
                     variable."
            in
            fail { loc; msg = Generic msg [@alert "-deprecated"] }
        in
        let@ expr = self e in
        let@ o_v = deref loc expr evaluation_scope in
        (* let@ o_v = match evaluation_scope with *)
        (*   | Some scope -> *)
        (*      let state = StringMap.find scope env.old_states in *)
        (*      return (STermMap.find_opt expr state.pointee_values) *)
        (*   | None -> *)
        (*      deref loc expr *)
        (* in *)
        (match o_v with
         | Some v -> return v
         | None ->
           let msg =
             !^"Cannot dereference"
             ^^^ Pp.squotes (Terms.pp expr)
             ^^^ pp_in_scope evaluation_scope
             ^^ Pp.dot
             ^^^ !^"Is the necessary ownership missing?"
           in
           fail { loc; msg = Generic msg [@alert "-deprecated"] })
      | CNExpr_value_of_c_atom (sym, C_kind_var) ->
        assert (not (Sym.Set.mem sym locally_bound));
        (* let@ o_v = match evaluation_scope with *)
        (*   | Some scope -> *)
        (*      let state = StringMap.find scope env.old_states in *)
        (*      let o_v =  *)
        (*        Option.map (function *)
        (*            | Value x -> x *)
        (*            | Points_to x -> x *)
        (*          ) (Sym.Map.find_opt sym state.var_state) *)
        (*      in *)
        (*      return o_v *)
        (*   | None -> *)
        (*      value_of_c_variable loc sym *)
        (* in *)
        let@ o_v = value_of_c_variable loc sym evaluation_scope in
        (match o_v with
         | None ->
           let msg =
             !^"Cannot resolve the value of"
             ^^^ Sym.pp sym
             ^^^ pp_in_scope evaluation_scope
             ^^ Pp.dot
             ^^^ !^"Is the ownership for accessing"
             ^^^ Sym.pp sym
             ^^^ !^"missing?"
           in
           fail { loc; msg = Generic msg [@alert "-deprecated"] }
         | Some v -> return v)
      | CNExpr_value_of_c_atom (sym, C_kind_enum) ->
        assert (not (Sym.Set.mem sym locally_bound));
        liftResult (do_decode_enum env loc sym)
    in
    trans None


  let cn_res_info ~pred_loc env res args =
    let open Req in
    let@ ptr_expr, iargs =
      match args with
      | [] -> fail { loc = pred_loc; msg = First_iarg_missing }
      | x :: xs -> return (x, xs)
    in
    let@ pname, oargs_ty =
      match res with
      | Cn.CN_owned oty ->
        let@ scty = infer_scty ~pred_loc ~ptr:ptr_expr `RW oty in
        (* we don't take Resource.owned_oargs here because we want to maintain the C-type
           information *)
        let oargs_ty = Memory.sbt_of_sct scty in
        return (Req.Owned (scty, Init), oargs_ty)
      | CN_block oty ->
        let@ scty = infer_scty ~pred_loc ~ptr:ptr_expr `W oty in
        let oargs_ty = Memory.sbt_of_sct scty in
        return (Req.Owned (scty, Uninit), oargs_ty)
      | CN_named pred ->
        let@ pred_sig =
          match lookup_predicate pred env with
          | None ->
            fail
              { loc = pred_loc;
                msg = Global (Unknown_resource_predicate { id = pred; logical = false })
              }
          | Some pred_sig -> return pred_sig
        in
        let output_bt = pred_sig.pred_output in
        return (Req.PName pred, SBT.inj output_bt)
    in
    return (pname, ptr_expr, iargs, oargs_ty)


  let split_pointer_linear_step loc sym_args (ptr_expr : IT.Surface.t) =
    let open Pp in
    let qs = IT.sym_ sym_args in
    let msg_s = "Iterated predicate pointer must be array_shift<ctype>(ptr, q_var):" in
    match IT.get_term ptr_expr with
    | ArrayShift { base = p; ct; index = x } when Terms.equal_annot SBT.equal x qs ->
      return (p, ct)
    | _ -> fail { loc; msg = Generic (!^msg_s ^^^ IT.pp ptr_expr) [@alert "-deprecated"] }


  let owned_good _sym (res_t, _oargs_ty) =
    let here = Locations.other __LOC__ in
    match res_t with
    | Req.P { pointer; name = Owned (scty, _); _ } ->
      [ ( LC.T (IT.good_ (Pointer scty, pointer) here),
          (here, Some "default pointer constraint") )
      ]
    | Req.Q { pointer; name = Owned (scty, _); _ } ->
      [ ( LC.T (IT.good_ (Pointer scty, pointer) here),
          (here, Some "default pointer constraint") )
      ]
    | _ -> []


  let cn_let_resource__pred env sym (pred_loc, res, args) =
    let@ args = ListM.mapM (cn_expr Sym.Set.empty env) args in
    let@ pname, ptr_expr, iargs, oargs_ty = cn_res_info ~pred_loc env res args in
    let pt =
      ( Req.P
          { name = pname;
            pointer = IT.Surface.proj ptr_expr;
            iargs = List.map IT.Surface.proj iargs
          },
        oargs_ty )
    in
    let pointee_value =
      match pname with
      | Owned (_, Init) ->
        let here = Locations.other __LOC__ in
        [ (ptr_expr, IT.sym_ (sym, oargs_ty, here)) ]
      | _ -> []
    in
    return (pt, pointee_value)


  let cn_let_resource__each env (q, bt, guard, pred_loc, res, args) =
    (* FIXME pred_loc is the wrong location, but the frontend is not tracking the correct one *)
    let@ bt' = check_quantified_base_type env pred_loc q bt in
    let env_with_q = add_logical q bt' env in
    let@ guard_expr = cn_expr (Sym.Set.singleton q) env_with_q guard in
    let@ args = ListM.mapM (cn_expr (Sym.Set.singleton q) env_with_q) args in
    let@ pname, ptr_expr, iargs, oargs_ty = cn_res_info ~pred_loc env_with_q res args in
    let here = Locations.other __LOC__ in
    let@ ptr_base, step = split_pointer_linear_step pred_loc (q, bt', here) ptr_expr in
    let m_oargs_ty = SBT.make_map_bt bt' oargs_ty in
    let pt =
      ( Req.Q
          { name = pname;
            q = (q, SBT.proj bt');
            q_loc = here;
            pointer = IT.Surface.proj ptr_base;
            step;
            permission = IT.Surface.proj guard_expr;
            iargs = List.map IT.Surface.proj iargs
          },
        m_oargs_ty )
    in
    return (pt, [])


  let cn_let_resource env (sym, the_res) =
    let@ pt, pointee_values =
      match the_res with
      | Cn.CN_pred (pred_loc, res, args) ->
        cn_let_resource__pred env sym (pred_loc, res, args)
      | CN_each (q, bt, guard, pred_loc, res, args) ->
        cn_let_resource__each env (q, bt, guard, pred_loc, res, args)
    in
    return (pt, owned_good sym pt, pointee_values)


  let cn_assrt env (loc, assrt) =
    match assrt with
    | Cn.CN_assert_exp e_ ->
      let@ e = cn_expr Sym.Set.empty env e_ in
      return (LC.T (IT.Surface.proj e))
    | CN_assert_qexp (sym, bTy, e1_, e2_) ->
      let bt = base_type env bTy in
      let env_with_q = add_logical sym bt env in
      let@ e1 = cn_expr (Sym.Set.singleton sym) env_with_q e1_ in
      let@ e2 = cn_expr (Sym.Set.singleton sym) env_with_q e2_ in
      return
        (LC.Forall
           ((sym, SBT.proj bt), IT.impl_ (IT.Surface.proj e1, IT.Surface.proj e2) loc))


  let cn_statement env (loc, (stmt_ : _ Cn.cn_statement_)) =
    let open Cnprog in
    match stmt_ with
    | CN_pack_unpack (pack_unpack, pred, args) ->
      let@ args = ListM.mapM (cn_expr Sym.Set.empty env) args in
      let@ name, pointer, iargs, _oargs_ty = cn_res_info ~pred_loc:loc env pred args in
      let stmt =
        Pack_unpack
          ( pack_unpack,
            { name;
              pointer = IT.Surface.proj pointer;
              iargs = List.map IT.Surface.proj iargs
            } )
      in
      return (Statement (loc, stmt))
    | CN_to_from_bytes (to_from, pred, args) ->
      let@ args = ListM.mapM (cn_expr Sym.Set.empty env) args in
      let@ name, pointer, iargs, _oargs_ty = cn_res_info ~pred_loc:loc env pred args in
      return
        (Statement
           ( loc,
             To_from_bytes
               ( to_from,
                 { name;
                   pointer = IT.Surface.proj pointer;
                   iargs = List.map IT.Surface.proj iargs
                 } ) ))
    | CN_have assrt ->
      let@ assrt = cn_assrt env (loc, assrt) in
      return (Statement (loc, Have assrt))
    | CN_instantiate (to_instantiate, expr) ->
      let@ expr = cn_expr Sym.Set.empty env expr in
      let expr = IT.Surface.proj expr in
      let to_instantiate =
        match to_instantiate with
        | Cn.I_Everything -> Cn.I_Everything
        | I_Function f -> I_Function f
        | I_Good ct -> I_Good (Sctypes.of_ctype_unsafe loc ct)
      in
      return (Statement (loc, Instantiate (to_instantiate, expr)))
    | CN_split_case e ->
      let@ e = cn_assrt env (loc, e) in
      return (Statement (loc, Split_case e))
    | CN_extract (attrs, to_extract, expr) ->
      let@ expr = cn_expr Sym.Set.empty env expr in
      let expr = IT.Surface.proj expr in
      let to_extract =
        match to_extract with
        | Cn.E_Everything -> Cn.E_Everything
        | E_Pred (CN_owned oty) ->
          E_Pred (CN_owned (Option.map (Sctypes.of_ctype_unsafe loc) oty))
        | E_Pred (CN_block oty) ->
          E_Pred (CN_block (Option.map (Sctypes.of_ctype_unsafe loc) oty))
        | E_Pred (CN_named pn) -> E_Pred (CN_named pn)
      in
      return (Statement (loc, Extract (attrs, to_extract, expr)))
    | CN_unfold (s, args) ->
      let@ args = ListM.mapM (cn_expr Sym.Set.empty env) args in
      let args = List.map IT.Surface.proj args in
      return (Statement (loc, Unfold (s, args)))
    | CN_assert_stmt e ->
      let@ e = cn_assrt env (loc, e) in
      return (Statement (loc, Assert e))
    | CN_apply (s, args) ->
      let@ args = ListM.mapM (cn_expr Sym.Set.empty env) args in
      let args = List.map IT.Surface.proj args in
      return (Statement (loc, Apply (s, args)))
    | CN_inline nms -> return (Statement (loc, Inline nms))
    | CN_print expr ->
      let@ expr = cn_expr Sym.Set.empty env expr in
      let expr = IT.Surface.proj expr in
      return (Statement (loc, Print expr))
end

module Handle = struct
  let with_state C_vars.{ current; old } : 'a C_vars.t -> 'a Or_Error.t =
    let state_for_scope = function None -> current | Some s -> StringMap.find s old in
    let rec aux : _ C_vars.t -> _ Or_Error.t = function
      | Done x -> return x
      | Error { loc; msg } -> fail { loc; msg }
      | Value_of_c_variable (loc, sym, scope, k) ->
        let variable_state = (state_for_scope scope).var_state in
        let o_v =
          Option.map
            (function
              | C_vars.Value (sym', sbt) -> IT.sym_ (sym', sbt, loc) | Points_to x -> x)
            (Sym.Map.find_opt sym variable_state)
        in
        aux (k o_v)
      | Deref (_loc, it, scope, k) ->
        let pointee_values = (state_for_scope scope).pointee_values in
        let o_v = STermMap.find_opt it pointee_values in
        aux (k o_v)
      | ScopeExists (_loc, scope, k) -> aux (k (StringMap.mem scope old))
    in
    aux


  let pure what = function
    | C_vars.Done x -> return x
    | Error { loc; msg } -> fail { loc; msg }
    | Value_of_c_variable (loc, _, _, _) ->
      let msg = !^what ^^^ !^"are not allowed to refer to (the state of) C variables." in
      fail { loc; msg = Generic msg [@alert "-deprecated"] }
    | Deref (loc, _, _, _) ->
      let msg = !^what ^^^ !^"are not allowed to dereference pointers." in
      fail { loc; msg = Generic msg [@alert "-deprecated"] }
    | ScopeExists (loc, _, _) ->
      let msg = !^what ^^^ !^"are not allowed to use evaluation scopes." in
      fail { loc; msg = Generic msg [@alert "-deprecated"] }


  let pointee_ct loc it =
    match IT.get_bt it with
    | BT.Loc (Some ct) -> return ct
    | BT.Loc None ->
      let msg = !^"Cannot tell pointee C-type of" ^^^ Pp.squotes (IT.pp it) ^^ Pp.dot in
      fail { loc; msg = Generic msg [@alert "-deprecated"] }
    | has ->
      let expected = "pointer" in
      let reason = "dereferencing" in
      let msg =
        WellTyped (Illtyped_it { it = IT.pp it; has = SBT.pp has; expected; reason })
      in
      fail { loc; msg }


  let with_loads allocations old_states : Cnprog.t C_vars.t -> Cnprog.t Or_Error.t =
    let rec aux = function
      | C_vars.Done x -> return x
      | Error { loc; msg } -> fail { loc; msg }
      | Value_of_c_variable (loc, sym, scope, k) ->
        (match scope with
         | Some scope ->
           let variable_state = C_vars.((StringMap.find scope old_states).var_state) in
           let o_v =
             Option.map
               (function
                 | C_vars.Value (sym', sbt) -> IT.sym_ (sym', sbt, loc)
                 | C_vars.Points_to x -> x)
               (Sym.Map.find_opt sym variable_state)
           in
           aux (k o_v)
         | None ->
           let ct = Sctypes.of_ctype_unsafe loc (allocations sym) in
           let pointer = IT.sym_ (sym, BT.Loc (Some ct), loc) in
           load loc "read" pointer k)
      | Deref (loc, pointer, scope, k) ->
        (match scope with
         | Some scope ->
           let pointee_values = (StringMap.find scope old_states).pointee_values in
           let o_v = STermMap.find_opt pointer pointee_values in
           aux (k o_v)
         | None -> load loc "deref" pointer k)
      | ScopeExists (_loc, scope, k) -> aux (k (StringMap.mem scope old_states))
    and load loc action_pp pointer k =
      let@ pointee_ct = pointee_ct loc pointer in
      let value_loc = loc in
      let value_s = Sym.fresh_make_uniq (action_pp ^ "_" ^ Pp.plain (IT.pp pointer)) in
      let value_bt = Memory.sbt_of_sct pointee_ct in
      let value = IT.sym_ (value_s, value_bt, value_loc) in
      let@ prog = aux (k (Some value)) in
      let load = Cnprog.{ ct = pointee_ct; pointer = IT.Surface.proj pointer } in
      return (Cnprog.Let (loc, (value_s, load), prog))
    in
    aux
end

let expr syms env st expr = Handle.with_state st (C_vars.cn_expr syms env expr)

let let_resource env st res = Handle.with_state st (C_vars.cn_let_resource env res)

let assrt env st assrt = Handle.with_state st (C_vars.cn_assrt env assrt)

let cn_func_body env body =
  let handle = Handle.pure "Function definitions" in
  let@ body = handle (C_vars.cn_expr Sym.Set.empty env body) in
  return (IT.Surface.proj body)


let known_attrs = [ "rec"; "coq_unfold" ]

let function_ env (def : _ Cn.cn_function) =
  Pp.debug 2 (lazy (Pp.item "translating function defn" (Sym.pp def.cn_func_name)));
  let args = List.map (fun (sym, bTy) -> (sym, base_type env bTy)) def.cn_func_args in
  let env' = List.fold_left (fun acc (sym, bt) -> add_logical sym bt acc) env args in
  let is_rec =
    List.exists (fun id -> String.equal (Id.get_string id) "rec") def.cn_func_attrs
  in
  let coq_unfold =
    List.exists (fun id -> String.equal (Id.get_string id) "coq_unfold") def.cn_func_attrs
  in
  let@ () =
    ListM.iterM
      (fun id ->
         if List.exists (String.equal (Id.get_string id)) known_attrs then
           return ()
         else
           fail
             { loc = def.cn_func_loc;
               msg =
                 Generic (Pp.item "Unknown attribute" (Id.pp id)) [@alert "-deprecated"]
             })
      def.cn_func_attrs
  in
  let@ body =
    match def.cn_func_body with
    | Some body ->
      let@ body = cn_func_body env' body in
      return (if is_rec then Def.Function.Rec_Def body else Def.Function.Def body)
    | None -> return Def.Function.Uninterp
  in
  let return_bt = base_type env def.cn_func_return_bty in
  let def2 =
    Def.Function.
      { loc = def.cn_func_loc;
        args = List.map_snd SBT.proj args;
        return_bt = SBT.proj return_bt;
        emit_coq = not coq_unfold;
        body
      }
  in
  return (def.cn_func_name, def2)


let ownership (loc, (addr_s, ct)) env =
  let name =
    match Sym.description addr_s with
    | SD_ObjectAddress obj_name -> Sym.fresh_make_uniq ("O_" ^ obj_name)
    | _ -> assert false
  in
  let resource =
    Cn.CN_pred (loc, CN_owned (Some ct), [ CNExpr (loc, CNExpr_var addr_s) ])
  in
  let@ (pt_ret, oa_bt), lcs, _ =
    Handle.pure "'Accesses'" (C_vars.cn_let_resource env (name, resource))
  in
  let value = IT.sym_ (name, oa_bt, loc) in
  return (name, ((pt_ret, oa_bt), lcs), value)


let allocation_token loc addr_s =
  let name =
    match Sym.description addr_s with
    | SD_ObjectAddress obj_name -> Sym.fresh_make_uniq ("A_" ^ obj_name)
    | _ -> assert false
  in
  let alloc_ret = Request.make_alloc (IT.sym_ (addr_s, BT.Loc (), loc)) in
  ((name, (Request.P alloc_ret, Alloc.History.value_bt)), (loc, None))


let cn_clause env clause =
  let rec cn_clause_aux env st acc clause =
    let module LAT = LogicalArgumentTypes in
    match clause with
    | Cn.CN_letResource (res_loc, sym, the_res, cl) ->
      let@ (pt_ret, oa_bt), lcs, pointee_vals =
        Handle.with_state st (C_vars.cn_let_resource env (sym, the_res))
      in
      let acc' z =
        acc
          (LAT.mResource
             ((sym, (pt_ret, SBT.proj oa_bt)), (res_loc, None))
             (LAT.mConstraints lcs z))
      in
      let env' = add_logical sym oa_bt env in
      let st' = C_vars.add_pointee_values pointee_vals st in
      cn_clause_aux env' st' acc' cl
    | CN_letExpr (loc, sym, e_, cl) ->
      let@ e = Handle.with_state st (C_vars.cn_expr Sym.Set.empty env e_) in
      let acc' z = acc (LAT.mDefine (sym, IT.Surface.proj e, (loc, None)) z) in
      cn_clause_aux (add_logical sym (IT.get_bt e) env) st acc' cl
    | CN_assert (loc, assrt, cl) ->
      let@ lc = Handle.with_state st (C_vars.cn_assrt env (loc, assrt)) in
      let acc' z = acc (LAT.mConstraint (lc, (loc, None)) z) in
      cn_clause_aux env st acc' cl
    | CN_return (_loc, e_) ->
      let@ e = Handle.with_state st (C_vars.cn_expr Sym.Set.empty env e_) in
      let e = IT.Surface.proj e in
      acc (LAT.I e)
  in
  cn_clause_aux env C_vars.init (fun z -> return z) clause


let cn_clauses env clauses =
  let rec self acc = function
    | Cn.CN_clause (loc, cl_) ->
      let@ cl = cn_clause env cl_ in
      let here = Locations.other __LOC__ in
      return (Def.Clause.{ loc; guard = IT.bool_ true here; packing_ft = cl } :: acc)
    | CN_if (loc, e_, cl_, clauses') ->
      let@ e = Handle.pure "Predicate guards" (C_vars.cn_expr Sym.Set.empty env e_) in
      let@ cl = cn_clause env cl_ in
      self ({ loc; guard = IT.Surface.proj e; packing_ft = cl } :: acc) clauses'
  in
  let@ xs = self [] clauses in
  return (List.rev xs)


let predicate env (def : _ Cn.cn_predicate) =
  Pp.debug 2 (lazy (Pp.item "translating predicate defn" (Sym.pp def.cn_pred_name)));
  let iargs, output_bt =
    match lookup_predicate def.cn_pred_name env with
    | None -> assert false
    | Some pred_sig -> (pred_sig.pred_iargs, pred_sig.pred_output)
  in
  let env' =
    List.fold_left (fun acc (sym, bTy) -> add_logical sym (SBT.inj bTy) acc) env iargs
  in
  let@ clauses =
    match def.cn_pred_clauses with
    | Some clauses ->
      let@ clauses = cn_clauses env' clauses in
      return (Some clauses)
    | None -> return None
  in
  match iargs with
  | (iarg0, BaseTypes.Loc ()) :: iargs' ->
    return
      ( def.cn_pred_name,
        Def.Predicate.
          { loc = def.cn_pred_loc;
            pointer = iarg0;
            iargs = iargs';
            oarg_bt = output_bt;
            clauses
          } )
  | (_, found_bty) :: _ ->
    fail
      { loc = def.cn_pred_loc;
        msg = First_iarg_not_pointer { pname = PName def.cn_pred_name; found_bty }
      }
  | [] -> fail { loc = def.cn_pred_loc; msg = First_iarg_missing }


let rec logical_ret_generic env st = function
  | Cn.CN_cletResource (loc, name, resource) :: ensures ->
    let@ (pt_ret, oa_bt), lcs, pointee_values =
      Handle.with_state st (C_vars.cn_let_resource env (name, resource))
    in
    let env = add_logical name oa_bt env in
    let st = C_vars.add_pointee_values pointee_values st in
    let@ lrt, env, st = logical_ret_generic env st ensures in
    return
      ( LRT.mResource
          ((name, (pt_ret, SBT.proj oa_bt)), (loc, None))
          (LRT.mConstraints lcs lrt),
        env,
        st )
  | CN_cletExpr (loc, name, expr) :: ensures ->
    let@ expr = Handle.with_state st (C_vars.cn_expr Sym.Set.empty env expr) in
    let@ lrt, env, st =
      logical_ret_generic (add_logical name (IT.get_bt expr) env) st ensures
    in
    return (LRT.mDefine (name, IT.Surface.proj expr, (loc, None)) lrt, env, st)
  | CN_cconstr (loc, constr) :: ensures ->
    let@ lc = Handle.with_state st (C_vars.cn_assrt env (loc, constr)) in
    let@ lrt, env, st = logical_ret_generic env st ensures in
    return (LRT.mConstraint (lc, (loc, None)) lrt, env, st)
  | [] -> return (LRT.I, env, st)


let logical_ret env st conds =
  let@ lrt, _env, _st = logical_ret_generic env st conds in
  return lrt


let logical_arg env st (requires, ensures) =
  let@ args_lrt, env, st = logical_ret_generic env st requires in
  let st = C_vars.(push_scope st start) in
  let@ ret_lrt, _env, _st = logical_ret_generic env st ensures in
  return (LAT.of_lrt args_lrt (LAT.I ret_lrt))


let rec logical_ret_accesses env st (accesses, ensures) =
  match accesses with
  | (loc, (addr_s, ct)) :: accesses ->
    let@ name, ((pt_ret, oa_bt), lcs), value = ownership (loc, (addr_s, ct)) env in
    let env = add_logical name oa_bt env in
    let st = C_vars.add [ (addr_s, Points_to value) ] st in
    let@ lrt = logical_ret_accesses env st (accesses, ensures) in
    return
      (LRT.mResource
         ((name, (pt_ret, SBT.proj oa_bt)), (loc, None))
         (LRT.mConstraints lcs lrt))
  | [] -> logical_ret env st ensures


let return_type loc (env : env) st (s, ct) (accesses, ensures) =
  let ct = Sctypes.of_ctype_unsafe loc ct in
  let sbt = Memory.sbt_of_sct ct in
  let bt = SBT.proj sbt in
  let@ lrt = logical_ret_accesses (add_computational s sbt env) st (accesses, ensures) in
  (* let info = (loc, Some "return value good") in *)
  (* let here = Locations.other __LOC__ in *)
  (* let lrt = LRT.mConstraint (LC.T (IT.good_ (ct, IT.sym_ (s, bt, here)) here), info)
     lrt in *)
  return (RT.mComputational ((s, bt), (loc, None)) lrt)


(* copied and adjusted from cn_function *)
let lemma env (def : _ Cn.cn_lemma) =
  Pp.debug 2 (lazy (Pp.item "translating lemma defn" (Sym.pp def.cn_lemma_name)));
  let rec aux env = function
    | (sym, bTy) :: args' ->
      let bTy = base_type env bTy in
      let env = add_computational sym bTy env in
      let@ at = aux env args' in
      let info = (def.cn_lemma_loc, None) in
      return (ArgumentTypes.Computational ((sym, SBT.proj bTy), info, at))
    | [] ->
      let@ lat =
        logical_arg env C_vars.init (def.cn_lemma_requires, def.cn_lemma_ensures)
      in
      return (ArgumentTypes.L lat)
  in
  let@ at = aux env def.cn_lemma_args in
  return (def.cn_lemma_name, (def.cn_lemma_loc, at))


let statement
      (allocations : Sym.t -> CF.Ctype.ctype)
      old_states
      env
      (Cn.CN_statement (loc, stmt_))
  =
  Handle.with_loads allocations old_states (C_vars.cn_statement env (loc, stmt_))
