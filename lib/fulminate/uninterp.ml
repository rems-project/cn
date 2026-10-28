module LAT = LogicalArgumentTypes
module LRT = LogicalReturnTypes
module RT = ReturnTypes
module AT = ArgumentTypes
open Terms
module G = Graph.Persistent.Digraph.Concrete (Sym)
module Dfs = Graph.Traverse.Dfs (G)

(* b -> a : a's definition mentions b *)
(* Construct graph in reverse *)
let add_uses graph ~user ~used = Sym.Set.fold (fun b g -> G.add_edge g b user) used graph

let fn_calls_of_term it = Terms.preds_of it

let fn_calls_of_terms its =
  List.fold_left (fun acc it -> Sym.Set.union acc (fn_calls_of_term it)) Sym.Set.empty its


let fn_calls_of_constraint = function
  | LogicalConstraints.T it | Forall (_, it) -> fn_calls_of_term it


let fn_and_pred_calls_of_request = function
  | Request.P { name; pointer; iargs } ->
    let fns = fn_calls_of_terms (pointer :: iargs) in
    (match name with PName p -> Sym.Set.add p fns | Owned _ -> fns)
  | Q { name; pointer; permission; iargs; _ } ->
    let fns = fn_calls_of_terms (pointer :: permission :: iargs) in
    (match name with PName p -> Sym.Set.add p fns | Owned _ -> fns)


let uses_of_clause (clause : Definition.Clause.t) =
  let rec aux acc = function
    | LogicalArgumentTypes.Define ((_, it), _, lat) ->
      aux (Sym.Set.union acc (fn_calls_of_term it)) lat
    | Resource ((_, (req, _)), _, lat) ->
      aux (Sym.Set.union acc (fn_and_pred_calls_of_request req)) lat
    | Constraint (lc, _, lat) -> aux (Sym.Set.union acc (fn_calls_of_constraint lc)) lat
    | I it -> Sym.Set.union acc (fn_calls_of_term it)
  in
  aux (fn_calls_of_term clause.guard) clause.packing_ft


let fn_calls_of_function (def : Definition.Function.t) =
  match def.body with
  | Def body | Rec_Def body -> fn_calls_of_term body
  | Uninterp -> Sym.Set.empty


let fn_and_pred_calls_of_predicate (def : Definition.Predicate.t) =
  match def.clauses with
  | Some clauses ->
    List.fold_left
      (fun acc clause -> Sym.Set.union acc (uses_of_clause clause))
      Sym.Set.empty
      clauses
  | None -> Sym.Set.empty


(* The uninterpreted functions and predicates, closed under use: every function or predicate
   whose definition reaches one of them. Returned as the two sets separately. *)
let uninterp_closure
      (functions : (Sym.t * Definition.Function.t) list)
      (predicates : (Sym.t * Definition.Predicate.t) list)
  =
  let graph = G.empty in
  let graph = List.fold_left (fun graph (f, _) -> G.add_vertex graph f) graph functions in
  let graph =
    List.fold_left (fun graph (p, _) -> G.add_vertex graph p) graph predicates
  in
  let graph =
    List.fold_left
      (fun graph (f, def) -> add_uses graph ~user:f ~used:(fn_calls_of_function def))
      graph
      functions
  in
  let graph =
    List.fold_left
      (fun graph (p, def) ->
         add_uses graph ~user:p ~used:(fn_and_pred_calls_of_predicate def))
      graph
      predicates
  in
  let roots =
    Sym.Set.union
      (Sym.Set.of_list
         (List.filter_map
            (fun (f, def) -> if Definition.Function.is_uninterp def then Some f else None)
            functions))
      (Sym.Set.of_list
         (List.filter_map
            (fun (p, def) ->
               if Definition.Predicate.is_uninterp def then Some p else None)
            predicates))
  in
  let reached =
    Sym.Set.fold
      (fun root acc -> Dfs.fold_component Sym.Set.add acc graph root)
      roots
      Sym.Set.empty
  in
  let is_function = Sym.Set.of_list (List.map fst functions) in
  (Sym.Set.inter reached is_function, Sym.Set.diff reached is_function)


(* ------- *)

let is_uninterp_var (u_vars : Sym.Set.t) (IT (it_, _bt, _loc)) =
  match it_ with Sym s when Sym.Set.mem s u_vars -> true | _ -> false


(* Based on `Terms.is_call` *)
let is_uninterp_fn_call (u_fns : Sym.Set.t) (IT (it_, _bt, _loc)) =
  match it_ with Apply (f', _) when Sym.Set.mem f' u_fns -> true | _ -> false


(* Based on `Terms.mentions_call` *)
let contains_uninterp_var_or_fn_call u_vars u_fns =
  Terms.fold_subterms
    (fun _binders acc it ->
       acc || is_uninterp_var u_vars it || is_uninterp_fn_call u_fns it)
    false


let is_uninterp_pred_call (u_preds : Sym.Set.t) = function
  | Request.P { name; _ } | Q { name; _ } ->
    (match name with PName p_sym -> Sym.Set.mem p_sym u_preds | Owned _ -> false)


let pred_contains_uninterp_it u_vars u_fns =
  let uninterp_f = contains_uninterp_var_or_fn_call u_vars u_fns in
  let uninterp_f_list its = its |> List.map uninterp_f |> List.fold_left ( || ) false in
  function
  | Request.P { pointer; iargs; _ } -> uninterp_f pointer || uninterp_f_list iargs
  | Q { pointer; permission; iargs; _ } ->
    uninterp_f pointer || uninterp_f permission || uninterp_f_list iargs


let pred_is_or_contains_uninterp u_vars (u_fns, u_preds) req =
  is_uninterp_pred_call u_preds req || pred_contains_uninterp_it u_vars u_fns req


(* for now, duplicate *)
let rec accept_uninterp_lrt u_vars (u_fns, u_preds) = function
  | LRT.Define ((name, it), info, lrt) ->
    if contains_uninterp_var_or_fn_call u_vars u_fns it then
      accept_uninterp_lrt (Sym.Set.add name u_vars) (u_fns, u_preds) lrt
    else
      LRT.Define ((name, it), info, accept_uninterp_lrt u_vars (u_fns, u_preds) lrt)
  | Resource ((name, (ret, bt)), (loc, _str_opt), lrt) ->
    if pred_is_or_contains_uninterp u_vars (u_fns, u_preds) ret then
      accept_uninterp_lrt (Sym.Set.add name u_vars) (u_fns, u_preds) lrt
    else
      Resource
        ( (name, (ret, bt)),
          (loc, _str_opt),
          accept_uninterp_lrt u_vars (u_fns, u_preds) lrt )
  | Constraint (lc, (loc, _str_opt), lrt) ->
    (match lc with
     | T it | Forall (_, it) ->
       if contains_uninterp_var_or_fn_call u_vars u_fns it then
         accept_uninterp_lrt u_vars (u_fns, u_preds) lrt
       else
         Constraint (lc, (loc, _str_opt), accept_uninterp_lrt u_vars (u_fns, u_preds) lrt))
  | I -> I


let accept_uninterp_rt u_vars (u_fns, u_preds) = function
  | RT.Computational ((sym, bt), info, lrt) ->
    RT.Computational ((sym, bt), info, accept_uninterp_lrt u_vars (u_fns, u_preds) lrt)


let rec accept_uninterp_lat u_vars (u_fns, u_preds) fn_a = function
  | LAT.Define ((name, it), info, lat) ->
    if contains_uninterp_var_or_fn_call u_vars u_fns it then
      accept_uninterp_lat (Sym.Set.add name u_vars) (u_fns, u_preds) fn_a lat
    else
      LAT.Define ((name, it), info, accept_uninterp_lat u_vars (u_fns, u_preds) fn_a lat)
  | Resource ((name, (ret, bt)), (loc, _str_opt), lat) ->
    if pred_is_or_contains_uninterp u_vars (u_fns, u_preds) ret then
      accept_uninterp_lat (Sym.Set.add name u_vars) (u_fns, u_preds) fn_a lat
    else
      Resource
        ( (name, (ret, bt)),
          (loc, _str_opt),
          accept_uninterp_lat u_vars (u_fns, u_preds) fn_a lat )
  | Constraint (lc, (loc, _str_opt), lat) ->
    (match lc with
     | T it | Forall (_, it) ->
       if contains_uninterp_var_or_fn_call u_vars u_fns it then
         accept_uninterp_lat u_vars (u_fns, u_preds) fn_a lat
       else
         Constraint
           (lc, (loc, _str_opt), accept_uninterp_lat u_vars (u_fns, u_preds) fn_a lat))
  | I a -> I (fn_a u_vars (u_fns, u_preds) a)


let rec accept_uninterp_at u_vars (u_fns, u_preds) = function
  | AT.Computational (sym_and_bt, info, at) ->
    AT.Computational (sym_and_bt, info, accept_uninterp_at u_vars (u_fns, u_preds) at)
  | Ghost (sym_and_bt, info, at) ->
    Ghost (sym_and_bt, info, accept_uninterp_at u_vars (u_fns, u_preds) at)
  | L lat ->
    let fn_a u_vars' (u_fns', u_preds') (post, (stats, loops)) =
      let new_post = accept_uninterp_rt u_vars' (u_fns', u_preds') post in
      let _fn_b =
        Fun.id
        (* TODO *)
      in
      let new_loops =
        loops
        (* TODO *)
      in
      (new_post, (stats, new_loops))
    in
    L (accept_uninterp_lat u_vars (u_fns, u_preds) fn_a lat)


let accept_uninterp_toplevel (u_fns, u_preds) (instrumentation : Extract.instrumentation) =
  let u_vars = Sym.Set.empty in
  let new_internal =
    match instrumentation.internal with
    | Some internal -> Some (accept_uninterp_at u_vars (u_fns, u_preds) internal)
    | None -> None
  in
  { instrumentation with internal = new_internal }


let accept_uninterp_instr
      (instrumentation_list : Extract.instrumentation list)
      (prog5 : unit Mucore.file)
  =
  let u_fn_syms, u_pred_syms =
    uninterp_closure prog5.logical_predicates prog5.resource_predicates
  in
  List.map (accept_uninterp_toplevel (u_fn_syms, u_pred_syms)) instrumentation_list
