module BT = BaseTypes
module Res = Resource
module Req = Request
module LC = LogicalConstraints
module Loc = Locations
module IT = IndexTerms

let unfold_multiclause_preds = ref false

type solver = Solver.solver

type s =
  { typing_context : Context.t;
    solver : solver option;
    sym_eqs : IT.t Sym.Map.t;
    movable_indices : (Req.name * IT.t) list;
    log : Explain.log
  }

let empty_s (c : Context.t) =
  { typing_context = c;
    solver = None;
    sym_eqs = Sym.Map.empty;
    movable_indices = [];
    log = []
  }


type 'a pause = ('a * s, TypeErrors.t) Result.t

type 'a t = s -> ('a * s, TypeErrors.t) Result.t

type 'a m = 'a t

type failure = Context.t * Explain.log -> TypeErrors.t

(* basic functions *)

let return (a : 'a) : 'a t = fun s -> Ok (a, s)

let fail (f : failure) : 'a t = fun s -> Error (f (s.typing_context, s.log))

let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
  fun s -> match m s with Error e -> Error e | Ok (x, s') -> (f x) s'


let ( let@ ) = bind

let get () : s t = fun s -> Ok (s, s)

(* due to solver interaction, this has to be used carefully *)
let set (s' : s) : unit t = fun _s -> Ok ((), s')

let run (c : Context.t) (m : 'a t) : 'a Or_TypeError.t =
  match m (empty_s c) with Ok (a, _) -> Ok a | Error e -> Error e


let run_to_pause (c : Context.t) (m : 'a t) : 'a pause =
  match m (empty_s c) with Ok (a, s) -> Ok (a, s) | Error e -> Error e


let run_from_pause (f : 'a -> 'b t) (pause : 'a pause) =
  match pause with Ok (a, s) -> Result.map fst @@ f a s | Error e -> Error e


let pause_to_result (pause : 'a pause) : 'a Or_TypeError.t = Result.map fst pause

let pure (m : 'a t) : 'a t =
  fun s ->
  Option.iter Solver.push s.solver;
  let outcome = match m s with Ok (a, _) -> Ok (a, s) | Error e -> Error e in
  Option.iter (fun s -> Solver.pop s 1) s.solver;
  outcome


let pure_persist_logical_variables (m : 'a t) : 'a t =
  fun s ->
  Option.iter Solver.push s.solver;
  let outcome =
    match m s with
    | Ok (a, s') ->
      let typing_context =
        let open Context in
        { s.typing_context with logical = s'.typing_context.logical }
      in
      Ok (a, { s with typing_context })
    | Error e -> Error e
  in
  Option.iter (fun s -> Solver.pop s 1) s.solver;
  outcome


let sandbox (m : 'a t) : 'a Or_TypeError.t t =
  fun s ->
  let n = Solver.num_scopes (Option.get s.solver) in
  Solver.push (Option.get s.solver);
  let outcome =
    match m s with
    | Ok (a, _s') ->
      assert (Solver.num_scopes (Option.get s.solver) = n + 1);
      Solver.pop (Option.get s.solver) 1;
      Ok a
    | Error e ->
      let n' = Solver.num_scopes (Option.get s.solver) in
      assert (n' > n);
      Solver.pop (Option.get s.solver) (n' - n);
      Error e
  in
  Ok (outcome, s)


let lift (m : 'a Or_TypeError.t) : 'a m =
  fun s -> match m with Ok r -> Ok (r, s) | Error e -> Error e


(* end basic functions *)

module Eff = Effectful.Make (struct
    type nonrec 'a t = 'a t

    let bind = bind

    let return = return
  end)

let iterM = Eff.ListM.iterM

(* functions to make values derived from the monad state *)

let make_simp_ctxt s =
  Simplify.
    { global = s.typing_context.global; values = s.sym_eqs; simp_hook = (fun _ -> None) }


let simp_ctxt () =
  let@ s = get () in
  return (make_simp_ctxt s)


let make_provable loc ({ typing_context = s; solver; _ } as c) =
  let simp_ctxt = make_simp_ctxt c in
  let f ?(purpose = "") lc =
    Solver.provable
      ~loc
      ~solver:(Option.get solver)
      ~assumptions:s.constraints
      ~simp_ctxt
      ~purpose
      lc
  in
  f


let provable_internal loc =
  let@ s = get () in
  return (make_provable loc s)


(* boring functions for getting or setting, adding, or removing things in the context *)

let inspect (f : s -> 'a) : 'a t =
  let@ s = get () in
  return (f s)


let modify (f : s -> s) : unit t =
  let@ s = get () in
  set (f s)


let get_typing_context () : Context.t t = inspect (fun s -> s.typing_context)

let set_typing_context (c : Context.t) : unit t =
  modify (fun s -> { s with typing_context = c })


let inspect_typing_context (f : Context.t -> 'a) : 'a t =
  inspect (fun s -> f s.typing_context)


let modify_typing_context (f : Context.t -> Context.t) : unit t =
  let@ c = get_typing_context () in
  set_typing_context (f c)


let print_with_ctxt printer =
  let@ s = get_typing_context () in
  let () = printer s in
  return ()


let get_global () : Global.t t = inspect_typing_context (fun c -> c.global)

(** TODO delete this, have Global.t be constructed by itself *)
let set_global (g : Global.t) : unit t =
  modify_typing_context (fun s -> { s with global = g })


let record_action ((a : Explain.action), (loc : Loc.t)) : unit t =
  modify (fun s -> { s with log = Action (a, loc) :: s.log })


let modify_where (f : Where.t -> Where.t) : unit t =
  modify (fun s ->
    let log = Explain.State s.typing_context :: s.log in
    let typing_context = Context.modify_where f s.typing_context in
    { s with log; typing_context })


module ErrorReader = struct
  type nonrec 'a t = 'a t

  let return = return

  let bind = bind

  let get_global () =
    let@ s = get () in
    return s.typing_context.global


  let lift = function
    | Ok x -> return x
    | Error WellTyped.{ loc; msg } -> fail (fun _ -> { loc; msg = WellTyped msg })


  let fail loc msg = fail (fun _ -> { loc; msg = Global msg })

  let get_context () =
    let@ s = get () in
    return s.typing_context
end

module Global = struct
  include Global.Lift (ErrorReader)

  let empty = Global.empty

  let is_fun_decl global id = Option.is_some @@ Global.get_fun_decl global id

  let get_struct_member_type loc tag member =
    let@ decl = get_struct_decl loc tag in
    let@ ty = get_member_type loc member decl in
    return ty


  let get_fun_decls () =
    let@ global = get_global () in
    return (Sym.Map.bindings global.fun_decls)


  let add_struct_decl tag layout : unit m =
    let@ global = get_global () in
    set_global { global with struct_decls = Sym.Map.add tag layout global.struct_decls }


  let add_fun_decl fname entry =
    let@ global = get_global () in
    set_global { global with fun_decls = Sym.Map.add fname entry global.fun_decls }


  let add_lemma lemma_s (loc, lemma_typ) =
    let@ global = get_global () in
    set_global
      { global with lemmata = Sym.Map.add lemma_s (loc, lemma_typ) global.lemmata }


  let add_resource_predicate name entry =
    let@ global = get_global () in
    set_global
      { global with
        resource_predicates = Sym.Map.add name entry global.resource_predicates
      }


  let add_logical_function name entry =
    let@ global = get_global () in
    set_global
      { global with logical_functions = Sym.Map.add name entry global.logical_functions }


  let add_datatype name entry =
    let@ global = get_global () in
    set_global { global with datatypes = Sym.Map.add name entry global.datatypes }


  let add_datatype_constr name entry =
    let@ global = get_global () in
    set_global
      { global with datatype_constrs = Sym.Map.add name entry global.datatype_constrs }


  let set_datatype_order datatype_order =
    let@ g = get_global () in
    set_global { g with datatype_order }


  let get_datatype_order () =
    let@ g = get_global () in
    return g.datatype_order


  let set_resource_predicate_order resource_predicate_order =
    let@ g = get_global () in
    set_global { g with resource_predicate_order }


  let get_resource_predicate_order () =
    let@ g = get_global () in
    return g.resource_predicate_order


  let set_logical_function_order logical_function_order =
    let@ g = get_global () in
    set_global { g with logical_function_order }


  let get_logical_function_order () =
    let@ g = get_global () in
    return g.logical_function_order
end

(* end: convenient functions for global typing context *)

module WellTyped = WellTyped.Lift (ErrorReader)

let add_sym_eqs sym_eqs =
  modify (fun s ->
    let sym_eqs =
      List.fold_left (fun acc (s, v) -> Sym.Map.add s v acc) s.sym_eqs sym_eqs
    in
    { s with sym_eqs })


let bound_a sym = inspect_typing_context (fun s -> Context.bound_a sym s)

let bound_l sym = inspect_typing_context (fun s -> Context.bound_l sym s)

let bound sym = inspect_typing_context (fun s -> Context.bound sym s)

let get_a sym = inspect_typing_context (fun s -> Context.get_a sym s)

let get_l sym = inspect_typing_context (fun s -> Context.get_l sym s)

(* If the solver exists, declare the variable. If not, these variables will be
   declared when the solver is created (in [init_solver]). *)
let maybe_declare_variable_in_solver sym bt =
  let@ s = get () in
  match s.solver with
  | None -> return ()
  | Some solver -> return (Solver.declare_variable solver sym bt)


let add_a sym bt info =
  let@ () = modify_typing_context (fun s -> Context.add_a sym bt info s) in
  maybe_declare_variable_in_solver sym bt


(* Don't need to be declared in solver. *)
let add_a_value sym value info =
  modify_typing_context (fun s -> Context.add_a_value sym value info s)


let add_l sym bt info =
  let@ () = modify_typing_context (fun s -> Context.add_l sym bt info s) in
  maybe_declare_variable_in_solver sym bt


(* Don't need to be declared in solver. *)
let add_l_value sym value info =
  let@ () = modify_typing_context (fun s -> Context.add_l_value sym value info s) in
  add_sym_eqs [ (sym, value) ]


let rec add_ls = function
  | [] -> return ()
  | (s, ls, info) :: lvars ->
    let@ () = add_l s ls info in
    add_ls lvars


let get_cs () = inspect_typing_context (fun c -> c.constraints)

let remove_a sym =
  let@ s = get_typing_context () in
  set_typing_context (Context.remove_a sym s)


let remove_as = iterM remove_a

(* let add_label_to_trace label =  *)
(*   modify_typing_context (fun c -> Context.add_label_to_trace label c) *)

(* let add_trace_item_to_trace i =  *)
(*   modify_typing_context (fun c -> Context.add_trace_item_to_trace i c) *)

(* similar but less boring functions, where components interact *)

let get_solver () : solver t = inspect (fun s -> Option.get s.solver)

let init_solver () =
  let@ () =
    modify (fun s ->
      let c = s.typing_context in
      let solver = Solver.make c.global in
      LC.Set.iter (Solver.add_assumption solver c.global) c.constraints;
      { s with solver = Some solver })
  in
  let maybe_declare (sym, (binding, _info)) =
    match binding with
    | Context.Value _ -> return () (* no need to declare *)
    | Context.BaseType bt -> maybe_declare_variable_in_solver sym bt
  in
  let@ ctxt = get_typing_context () in
  let@ () = iterM maybe_declare (Sym.Map.to_list ctxt.computational) in
  let@ () = iterM maybe_declare (Sym.Map.to_list ctxt.logical) in
  return ()


let get_movable_indices () = inspect (fun s -> s.movable_indices)

let set_movable_indices ixs : unit m = modify (fun s -> { s with movable_indices = ixs })

let add_c_internal lc =
  let@ s = get_typing_context () in
  let@ solver = get_solver () in
  let@ simp_ctxt = simp_ctxt () in
  let lc = Simplify.LogicalConstraints.simp simp_ctxt lc in
  let s = Context.add_c lc s in
  let () = Solver.add_assumption solver s.global lc in
  let@ _ = add_sym_eqs (List.filter_map LC.is_sym_lhs_equality [ lc ]) in
  let@ () = set_typing_context s in
  return ()


let add_r_internal ?(derive_constraints = true) loc (r, Res.O oargs) =
  let@ s = get_typing_context () in
  let@ simp_ctxt = simp_ctxt () in
  let r = Simplify.Request.simp simp_ctxt r in
  let oargs = Simplify.IndexTerms.simp simp_ctxt oargs in
  let pointer_facts =
    if derive_constraints then
      Res.pointer_facts ~new_resource:(r, Res.O oargs) ~old_resources:(Context.get_rs s)
    else
      []
  in
  let@ () = set_typing_context (Context.add_r loc (r, O oargs) s) in
  iterM (fun x -> add_c_internal (LC.T x)) pointer_facts


(* functions to do with satisfying models *)

let model () =
  let m = Solver.model () in
  return m


let _model_has_prop () =
  let is_some_true t = Option.is_some t && IT.is_true (Option.get t) in
  return (fun prop m -> is_some_true (Solver.eval (fst m) prop))


(* let prove_or_model_with_past_model loc m = *)
(*   let@ has_prop = model_has_prop () in *)
(*   let@ p_f = provable_internal loc in *)
(*   let loc = Locations.other __LOC__ in *)
(*   let res lc = *)
(*     match lc with *)
(*     | LC.T t when has_prop (IT.not_ t loc) m -> `Counterex (lazy m) *)
(*     | _ -> *)
(*       (match p_f lc with `True -> `True | `False -> `Counterex (lazy (Solver.model ()))) *)
(*   in *)
(*   let res2 lc = match res lc with `Counterex _m -> `False | `True -> `True in *)
(*   return (res, res2) *)

(* let model_with_internal loc prop = *)
(*   let@ ms = get_just_models () in *)
(*   let@ has_prop = model_has_prop () in *)
(*   match List.find_opt (has_prop prop) ms with *)
(*   | Some m -> return (Some m) *)
(*   | None -> *)
(*     let@ prover = provable_internal loc in *)
(*     let here = Locations.other __LOC__ in *)
(*     (match prover (LC.T (IT.not_ prop here)) with *)
(*      | `True -> return None *)
(*      | `False -> *)
(*        let@ m = model () in *)
(*        let@ () = cond_check_model loc m prop in *)
(*        return (Some m)) *)

(* functions for binding return types and associated auxiliary functions *)

let make_return_record loc (record_name : string) record_members =
  let record_s = Sym.fresh_make_uniq record_name in
  let record_bt = BT.Record record_members in
  let@ () = add_l record_s record_bt (loc, lazy (Sym.pp record_s)) in
  let record_it = IT.sym_ (record_s, record_bt, loc) in
  let member_its =
    List.map
      (fun (s, member_bt) -> IT.recordMember_ ~member_bt (record_it, s) loc)
      record_members
  in
  return (record_it, member_its)


(* This essentially pattern-matches a logical return type against a record pattern.
   `record_it` is the index term for the record, `members` the pattern for its members. *)
let bind_logical_return_internal loc =
  let rec aux members lrt =
    match (members, lrt) with
    | member :: members, LogicalReturnTypes.Define ((s, it), _, lrt) ->
      let@ () =
        WellTyped.ensure_base_type loc ~expect:(IT.get_bt it) (IT.get_bt member)
      in
      let@ () = add_c_internal (LC.T (IT.eq__ member it loc)) in
      aux members (LogicalReturnTypes.subst (IT.make_subst [ (s, member) ]) lrt)
    | member :: members, Resource ((s, (re, bt)), _, lrt) ->
      let@ () = WellTyped.ensure_base_type loc ~expect:bt (IT.get_bt member) in
      let@ () = add_r_internal loc (re, Res.O member) in
      aux members (LogicalReturnTypes.subst (IT.make_subst [ (s, member) ]) lrt)
    | members, Constraint (lc, _, lrt) ->
      let@ () = add_c_internal lc in
      aux members lrt
    | [], I -> return ()
    | _ -> assert false
  in
  fun members lrt -> aux members lrt


(* functions for resource inference *)

type changed =
  | Deleted
  | Unchanged
  | Changed of Res.t

let map_and_fold_resources_internal loc (f : Res.t -> 'acc -> changed * 'acc) (acc : 'acc)
  =
  let@ s = get_typing_context () in
  let@ provable_f = provable_internal loc in
  let resources = s.resources in
  let resources, acc =
    List.fold_right
      (fun re (resources, acc) ->
         let changed, acc = f re acc in
         match changed with
         | Deleted -> (resources, acc)
         | Unchanged -> (re :: resources, acc)
         | Changed re ->
           (match re with
            | Q { q; permission; _ }, _ ->
              let here = Locations.other __LOC__ in
              (match
                 provable_f
                   ~purpose:"map_and_fold_resources"
                   (LC.forall_ q (IT.not_ permission here))
               with
               | `True -> (resources, acc)
               | `False -> (re :: resources, acc))
            | _ -> (re :: resources, acc)))
      resources
      ([], acc)
  in
  let@ () = set_typing_context { s with resources } in
  return acc


(* let get_movable_indices () = *)
(*   inspect (fun s -> List.map (fun (pred, nm, _verb) -> (pred, nm)) s.movable_indices) *)

let consistency_check_threshold = 10

(* the main inference loop *)
let do_unfold_resources loc =
  let open Prooflog in
  let here = Locations.other __LOC__ in
  let start_time = Pp.time_start () in
  let rec aux count changed =
    let@ s = get_typing_context () in
    let@ movable_indices = get_movable_indices () in
    let@ provable_f = provable_internal here in
    let provable_f = provable_f ~purpose:"do_unfold_resources" in
    let resources = s.resources in
    Pp.debug 8 (lazy (Pp.string "-- checking resource unfolds now --"));
    let consistent_context () =
      match provable_f (LC.T (IT.bool_ false here)) with `True -> false | `False -> true
    in
    match count < consistency_check_threshold || consistent_context () with
    | false -> return changed (* contradictory state *)
    | true ->
      let count = if count < consistency_check_threshold then count else 0 in
      let keep, unpack, extract =
        List.fold_right
          (fun re (keep, unpack, extract) ->
             match
               Pack.unpack ~full:!unfold_multiclause_preds loc s.global provable_f re
             with
             | Some unpackable -> (keep, (re, unpackable) :: unpack, extract)
             | None ->
               let re_reduced, extracted =
                 Pack.extractable_multiple provable_f movable_indices re
               in
               let keep' =
                 match extracted with
                 | [] -> re_reduced :: keep
                 | _ ->
                   (match Pack.resource_empty provable_f re_reduced with
                    | `Empty -> keep
                    | `NonEmpty _ -> re_reduced :: keep)
               in
               (keep', unpack, extracted @ extract))
          resources
          ([], [], [])
      in
      let@ () = set_typing_context { s with resources = keep } in
      let do_unpack = function
        | re, `LRT lrt ->
          let pname = Req.get_name (fst re) in
          let@ _, members =
            make_return_record
              loc
              (* This string ends up as a solver variable (via Typing.make_return_record)
                 hence no "{<num>}" prefix should ever be printed. *)
              ("unpack_" ^ Pp.plain (Req.pp_name ~no_nums:true pname))
              (LogicalReturnTypes.binders lrt)
          in
          bind_logical_return_internal loc members lrt
        | re, `RES res ->
          let pname = Req.get_name (fst re) in
          let is_owned = match pname with Owned _ -> true | _ -> false in
          iterM (add_r_internal ~derive_constraints:(not is_owned) loc) res
      in
      let@ () = iterM do_unpack unpack in
      let@ () = iterM (add_r_internal loc) extract in
      let@ simp_ctxt = simp_ctxt () in
      (match (unpack, extract) with
       | [], [] -> return changed
       | _ ->
         let changed' =
           if Prooflog.is_enabled () then (
             let converted_unpack =
               List.map
                 (fun (re, unpackable) ->
                    match unpackable with
                    | `LRT lrt -> (re, UnpackLRT lrt)
                    | `RES res ->
                      let res_simp =
                        List.map
                          (fun (r, Res.O oargs) ->
                             let r = Simplify.Request.simp simp_ctxt r in
                             let oargs = Simplify.IndexTerms.simp simp_ctxt oargs in
                             (r, Res.O oargs))
                          res
                      in
                      (re, UnpackRES res_simp))
                 unpack
             in
             (converted_unpack, extract) :: changed)
           else
             []
         in
         let number_recursive_unfolds =
           List.fold_left
             (fun acc (re, _) ->
                match Req.get_name (fst re) with
                | PName pn when (Sym.Map.find pn s.global.resource_predicates).recursive
                  ->
                  acc + 1
                | _ -> acc)
             0
             unpack
         in
         let number_extracted = List.length extract in
         let count = count + number_recursive_unfolds + number_extracted in
         aux count changed')
  in
  let@ c = get_typing_context () in
  let@ changed = aux 0 [] in
  Pp.time_end "resource unfolding" start_time;
  match changed with
  | [] -> return ()
  | _ ->
    let@ c' = get_typing_context () in
    Prooflog.record_resource_inference_step
      (Prooflog.UnfoldResources (c, loc, List.rev changed, c'));
    return ()


let provable loc = provable_internal loc

let add_movable_index loc (pred, ix) =
  let@ ixs = get_movable_indices () in
  let@ () = set_movable_indices ((pred, ix) :: ixs) in
  do_unfold_resources loc


let bind_logical_return loc members lrt =
  let@ () = bind_logical_return_internal loc members lrt in
  do_unfold_resources loc


(* Same for return types *)
let bind_return loc members (rt : ReturnTypes.t) =
  match (members, rt) with
  | member :: members, Computational ((s, bt), _, lrt) ->
    let@ () = WellTyped.ensure_base_type loc ~expect:bt (IT.get_bt member) in
    let@ () =
      bind_logical_return
        loc
        members
        (LogicalReturnTypes.subst (IT.make_subst [ (s, member) ]) lrt)
    in
    return member
  | _ -> assert false


let add_r loc re =
  let@ () = add_r_internal loc re in
  do_unfold_resources loc


let add_rs loc rs =
  let@ () = iterM (add_r_internal loc) rs in
  do_unfold_resources loc


let add_c loc c =
  let@ () = add_c_internal c in
  do_unfold_resources loc


let add_cs loc cs =
  let@ () = iterM add_c_internal cs in
  do_unfold_resources loc


let all_resources _loc =
  let@ s = get_typing_context () in
  return (Context.get_rs s)


let map_and_fold_resources loc f acc = map_and_fold_resources_internal loc f acc

(* let prev_models_with _loc prop = *)
(*   let@ ms = get_just_models () in *)
(*   let@ has_prop = model_has_prop () in *)
(*   return (List.filter (has_prop prop) ms) *)

(* let _model_with loc prop = model_with_internal loc prop *)

(* auxiliary functions for diagnostics *)
