module LAT = LogicalArgumentTypes
module LRT = LogicalReturnTypes
module RT = ReturnTypes
module AT = ArgumentTypes
open Terms
module G = Graph.Persistent.Digraph.Concrete (Sym)
module Dfs = Graph.Traverse.Dfs (G)

let u_fn_syms = ref Sym.Set.empty

let u_pred_syms = ref Sym.Set.empty

(* b -> a : a's definition mentions b *)
(* Construct graph in reverse *)
let add_edges graph ~caller ~callee =
  Sym.Set.fold (fun b g -> G.add_edge g b caller) callee graph


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


let fn_and_pred_calls_of_clause (clause : Definition.Clause.t) =
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
      (fun acc clause -> Sym.Set.union acc (fn_and_pred_calls_of_clause clause))
      Sym.Set.empty
      clauses
  | None -> Sym.Set.empty


(* Construct graph to determine which functions are transitively dependent on uninterpreted functions,
and the same for predicates that are transitively dependent on uninterpreted functions and/or predicates.
Construct reverse graph by adding edges from the uninterpreted fn/pred calls to their callers, to make 
constructing reachable set via DFS easy.
*)
let set_uninterp_closure
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
      (fun graph (f, def) -> add_edges graph ~caller:f ~callee:(fn_calls_of_function def))
      graph
      functions
  in
  let graph =
    List.fold_left
      (fun graph (p, def) ->
         add_edges graph ~caller:p ~callee:(fn_and_pred_calls_of_predicate def))
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
  let uninterp_fns = Sym.Set.inter reached is_function in
  let uninterp_preds = Sym.Set.diff reached is_function in
  u_fn_syms := uninterp_fns;
  u_pred_syms := uninterp_preds


(* ------- *)

let is_uninterp_var (u_vars : Sym.Set.t) (IT (it_, _bt, _loc)) =
  match it_ with Sym s when Sym.Set.mem s u_vars -> true | _ -> false


(* Based on `Terms.is_call` *)
let is_uninterp_fn_call (IT (it_, _bt, _loc)) =
  match it_ with Apply (f', _) when Sym.Set.mem f' !u_fn_syms -> true | _ -> false


(* Based on `Terms.mentions_call` *)
(* Check if index term has uninterpreted variable or function call *)
let it_has_uninterp u_vars =
  Terms.fold_subterms
    (fun _binders acc it -> acc || is_uninterp_var u_vars it || is_uninterp_fn_call it)
    false


let is_uninterp_pred_call = function
  | Request.P { name; _ } | Q { name; _ } ->
    (match name with PName p_sym -> Sym.Set.mem p_sym !u_pred_syms | Owned _ -> false)


(* Check if predicate is uninterpreted, or has uninterpreted variables or function calls *)
let pred_is_or_has_uninterp u_vars req =
  let pred_has_uninterp u_vars =
    let uninterp_f = it_has_uninterp u_vars in
    let uninterp_f_list its = its |> List.map uninterp_f |> List.fold_left ( || ) false in
    function
    | Request.P { pointer; iargs; _ } -> uninterp_f pointer || uninterp_f_list iargs
    | Q { pointer; permission; iargs; _ } ->
      uninterp_f pointer || uninterp_f permission || uninterp_f_list iargs
  in
  is_uninterp_pred_call req || pred_has_uninterp u_vars req


(* for now, duplicate *)
let rec accept_uninterp_lrt u_vars = function
  | LRT.Define ((name, it), info, lrt) ->
    if it_has_uninterp !u_vars it then (
      u_vars := Sym.Set.add name !u_vars;
      accept_uninterp_lrt u_vars lrt)
    else
      LRT.Define ((name, it), info, accept_uninterp_lrt u_vars lrt)
  | Resource ((name, (ret, bt)), (loc, _str_opt), lrt) ->
    if pred_is_or_has_uninterp !u_vars ret then (
      u_vars := Sym.Set.add name !u_vars;
      accept_uninterp_lrt u_vars lrt)
    else
      Resource ((name, (ret, bt)), (loc, _str_opt), accept_uninterp_lrt u_vars lrt)
  | Constraint (lc, (loc, _str_opt), lrt) ->
    (match lc with
     | T it | Forall (_, it) ->
       let r = accept_uninterp_lrt u_vars lrt in
       if it_has_uninterp !u_vars it then
         r
       else
         Constraint (lc, (loc, _str_opt), r))
  | I -> I


let accept_uninterp_rt u_vars = function
  | RT.Computational ((sym, bt), info, lrt) ->
    RT.Computational ((sym, bt), info, accept_uninterp_lrt u_vars lrt)


let rec accept_uninterp_lat u_vars f = function
  | LAT.Define ((name, it), info, lat) ->
    if it_has_uninterp !u_vars it then (
      u_vars := Sym.Set.add name !u_vars;
      accept_uninterp_lat u_vars f lat)
    else
      LAT.Define ((name, it), info, accept_uninterp_lat u_vars f lat)
  | Resource ((name, (ret, bt)), (loc, _str_opt), lat) ->
    if pred_is_or_has_uninterp !u_vars ret then (
      u_vars := Sym.Set.add name !u_vars;
      accept_uninterp_lat u_vars f lat)
    else
      Resource ((name, (ret, bt)), (loc, _str_opt), accept_uninterp_lat u_vars f lat)
  | Constraint (lc, (loc, _str_opt), lat) ->
    (match lc with
     | T it | Forall (_, it) ->
       let r = accept_uninterp_lat u_vars f lat in
       if it_has_uninterp !u_vars it then
         r
       else
         Constraint (lc, (loc, _str_opt), r))
  | I a -> I (f u_vars a)


let rec accept_uninterp_at u_vars f = function
  | AT.Computational (sym_and_bt, info, at) ->
    AT.Computational (sym_and_bt, info, accept_uninterp_at u_vars f at)
  | Ghost (sym_and_bt, info, at) ->
    Ghost (sym_and_bt, info, accept_uninterp_at u_vars f at)
  | L lat -> L (accept_uninterp_lat u_vars f lat)


let cn_statement_has_uninterp u_vars = function
  | Cnstatement.Apply (_fsym, args) ->
    (* fsym is a lemma symbol: TODO look up if problematic *)
    List.fold_left ( || ) false (List.map (it_has_uninterp u_vars) args)
  | Assert lc ->
    (match lc with LogicalConstraints.T it | Forall (_, it) -> it_has_uninterp u_vars it)
  | Pack_unpack _ | Derive_constraints _ | To_from_bytes _ | Have _ | Instantiate _
  | Split_case _ | Extract _ | Unfold _ | Inline _ | Print _ ->
    false (* all no-ops in Fulminate *)


let accept_uninterp_cnprog u_vars (loc, cnprogs) =
  let rec accept_uninterp_cnprog_aux u_vars = function
    | Cnprog.Let (loc, (name, { ct; pointer }), prog) ->
      if it_has_uninterp !u_vars pointer then (
        u_vars := Sym.Set.add name !u_vars;
        accept_uninterp_cnprog_aux u_vars prog)
      else (
        match accept_uninterp_cnprog_aux u_vars prog with
        | Some cnprog -> Some (Cnprog.Let (loc, (name, { ct; pointer }), cnprog))
        | None -> None)
    | Pure (loc, cn_stat) ->
      if cn_statement_has_uninterp !u_vars cn_stat then
        None
      else
        Some (Pure (loc, cn_stat))
  in
  (loc, List.filter_map (accept_uninterp_cnprog_aux u_vars) cnprogs)


let accept_uninterp_statements u_vars = List.map (accept_uninterp_cnprog u_vars)

let accept_uninterp_rest_of_spec u_vars (post, (stats, loops)) =
  let new_post = accept_uninterp_rt u_vars post in
  let new_stats = accept_uninterp_statements u_vars stats in
  let new_loops =
    List.map
      (fun (contains_user_spec, cond_loc, loop_loc, at) ->
         ( contains_user_spec,
           cond_loc,
           loop_loc,
           accept_uninterp_at u_vars accept_uninterp_statements at ))
      loops
  in
  (new_post, (new_stats, new_loops))


let accept_uninterp (instrumentation : Extract.instrumentation) =
  (* New set of uninterpreted variables per function spec *)
  let u_vars = ref Sym.Set.empty in
  let new_internal =
    match instrumentation.internal with
    | Some internal ->
      Some (accept_uninterp_at u_vars accept_uninterp_rest_of_spec internal)
    | None -> None
  in
  { instrumentation with internal = new_internal }


let accept_uninterp_instr (instrumentation_list : Extract.instrumentation list) =
  List.map accept_uninterp instrumentation_list
