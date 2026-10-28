module LAT = LogicalArgumentTypes
module LRT = LogicalReturnTypes
module RT = ReturnTypes
module AT = ArgumentTypes
open Terms

(* Scan for uninterpreted functions and predicates, and the variables that get defined thereof *)

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
  let u_fn_syms =
    Sym.Set.of_list
      (List.filter_map
         (fun (fn_sym, (def : Definition.Function.t)) ->
            if Definition.Function.is_uninterp def then Some fn_sym else None)
         prog5.logical_predicates)
  in
  let u_pred_syms =
    Sym.Set.of_list
      (List.filter_map
         (fun (pred_sym, (def : Definition.Predicate.t)) ->
            if Definition.Predicate.is_uninterp def then Some pred_sym else None)
         prog5.resource_predicates)
  in
  List.map (accept_uninterp_toplevel (u_fn_syms, u_pred_syms)) instrumentation_list
