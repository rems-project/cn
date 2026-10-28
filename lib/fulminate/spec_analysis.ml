module LAT = LogicalArgumentTypes
module AT = ArgumentTypes
open Terms

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


let rec accept_uninterp_lat u_vars (u_fns, u_preds) = function
  | LAT.Define ((name, it), info, lat) ->
    if contains_uninterp_var_or_fn_call u_vars u_fns it then
      accept_uninterp_lat (Sym.Set.add name u_vars) (u_fns, u_preds) lat
    else
      LAT.Define ((name, it), info, accept_uninterp_lat u_vars (u_fns, u_preds) lat)
  | Resource ((name, (ret, bt)), (loc, _str_opt), lat) ->
    if pred_is_or_contains_uninterp u_vars (u_fns, u_preds) ret then
      accept_uninterp_lat (Sym.Set.add name u_vars) (u_fns, u_preds) lat
    else
      Resource
        ( (name, (ret, bt)),
          (loc, _str_opt),
          accept_uninterp_lat u_vars (u_fns, u_preds) lat )
  | Constraint (lc, (loc, _str_opt), lat) ->
    (match lc with
     | T it | Forall (_, it) ->
       if contains_uninterp_var_or_fn_call u_vars u_fns it then
         accept_uninterp_lat u_vars (u_fns, u_preds) lat
       else
         Constraint (lc, (loc, _str_opt), accept_uninterp_lat u_vars (u_fns, u_preds) lat))
  | I (_post, (_stats, _loops)) -> failwith "TODO"


let rec accept_uninterp_at (u_fns, u_preds) = function
  | AT.Computational (sym_and_bt, info, at) ->
    AT.Computational (sym_and_bt, info, accept_uninterp_at (u_fns, u_preds) at)
  | Ghost (sym_and_bt, info, at) ->
    Ghost (sym_and_bt, info, accept_uninterp_at (u_fns, u_preds) at)
  | L lat ->
    let u_vars = Sym.Set.empty in
    L (accept_uninterp_lat u_vars (u_fns, u_preds) lat)


let accept_uninterp_toplevel (u_fns, u_preds) (instrumentation : Extract.instrumentation) =
  let new_internal =
    match instrumentation.internal with
    | Some internal -> Some (accept_uninterp_at (u_fns, u_preds) internal)
    | None -> None
  in
  { instrumentation with internal = new_internal }
