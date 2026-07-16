module T = Terms.Normal
module ITMap = Map.Make (T)
module ITSet = Set.Make (T)

(* operations on a table of (possibly guarded) equalities *)

type eq_info =
  { guard : T.t option;
    rhs : T.t
  }

type table = eq_info list ITMap.t

let empty = (ITMap.empty : table)

(* maintaining a lookup table *)

let fetch_eqs (tab : table) lhs =
  match ITMap.find_opt lhs tab with None -> [] | Some xs -> xs


let guard_implies guard1 guard2 =
  match (guard1, guard2) with
  | _, None -> true
  | Some x, Some y -> T.equal x y
  | _ -> false


let eq_is_known tab (guard, lhs, rhs) =
  List.exists
    (fun info -> T.equal rhs info.rhs && guard_implies guard info.guard)
    (fetch_eqs tab lhs)


let add_eq (guard, lhs, rhs) (tab : table) =
  if eq_is_known tab (guard, lhs, rhs) then
    tab
  else
    ITMap.add lhs ({ guard; rhs } :: fetch_eqs tab lhs) tab


let add_eq_sym (guard, lhs, rhs) tab =
  add_eq (guard, lhs, rhs) (add_eq (guard, rhs, lhs) tab)


let add_one_eq (tab : table) (it : T.t) =
  match T.get_term it with
  | Terms.Binop (Terms.EQ, x, y) -> add_eq_sym (None, x, y) tab
  | Binop (Implies, guard, x) ->
    (match Terms.is_eq x with Some (y, z) -> add_eq_sym (Some guard, y, z) tab | _ -> tab)
  | _ -> tab


let add_eqs tab (it : T.t) =
  match Terms.is_and it with
  | Some (it1, it2) -> List.fold_left add_one_eq tab [ it1; it2 ]
  | _ -> add_one_eq tab it


let add_lc_eqs tab (lc : LogicalConstraints.t) =
  match lc with LogicalConstraints.T it -> add_eqs tab it | _ -> tab


let fetch_implied_eqs tab guard lhs =
  fetch_eqs tab lhs
  |> List.filter_map (fun info ->
    if guard_implies guard info.guard then
      Some info.rhs
    else
      None)


(* computing the closure of the above *)
let get_eq_vals (tab : table) (guard : T.t option) (x : T.t) =
  let rec seek known = function
    | [] -> known
    | x :: xs ->
      if ITSet.mem x known then
        seek known xs
      else
        seek (ITSet.add x known) (fetch_implied_eqs tab guard x @ xs)
  in
  seek ITSet.empty [ x ]
