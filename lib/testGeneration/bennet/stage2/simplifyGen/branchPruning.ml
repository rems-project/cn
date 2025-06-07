module IT = IndexTerms

module Unused = struct
  let transform_gt (gt : Term.t) : Term.t =
    let aux (gt : Term.t) : Term.t =
      match gt with
      | GT (Pick [ (_, gt') ], _, _) -> gt'
      | GT (ITE (it_cond, gt_then, gt_else), _, _) ->
        if IT.is_true it_cond then
          gt_then
        else if IT.is_false it_cond then
          gt_else
        else
          gt
      | _ -> gt
    in
    Term.map_gen_pre aux gt
end

module Inconsistent = struct
  let rec contains_false_assertion (gt : Term.t) : bool =
    let (GT (gt_, _, _)) = gt in
    match gt_ with
    | Uniform | Alloc | Call _ | Return _ -> false
    | Pick wgts ->
      List.is_empty wgts
      || List.for_all (fun (_, gt') -> contains_false_assertion gt') wgts
    | Asgn ((it_addr, _), _, gt') ->
      (match it_addr with
       | IT (Const Null, _, _) -> true
       | _ -> contains_false_assertion gt')
    | LetStar ((_, gt_inner), gt') ->
      contains_false_assertion gt_inner || contains_false_assertion gt'
    | Assert (lc, gt') ->
      (match lc with
       | (T it | Forall (_, it)) when IT.is_false it -> true
       | Forall (_, IT (Binop (Implies, it_perm, it_body), _, _))
         when IT.is_true it_perm && IT.is_false it_body ->
         true
       | _ -> contains_false_assertion gt')
    | ITE (_, gt_then, gt_else) ->
      contains_false_assertion gt_then && contains_false_assertion gt_else
    | Map (_, gt') -> contains_false_assertion gt'


  let transform_gt (gt : Term.t) : Term.t =
    let aux (gt : Term.t) : Term.t =
      match gt with
      | GT (Pick wgts, bt, loc_pick) ->
        Term.pick_
          (List.filter (fun (_, gt') -> not (contains_false_assertion gt')) wgts)
          bt
          loc_pick
      | GT (ITE (it_if, gt_then, gt_else), _, loc_ite) ->
        if contains_false_assertion gt_else then
          Term.assert_ (T it_if, gt_then) loc_ite
        else if contains_false_assertion gt_then then
          Term.assert_ (T (IT.not_ it_if (IT.get_loc it_if)), gt_else) loc_ite
        else
          gt
      | _ -> gt
    in
    Term.map_gen_post aux gt
end

let transform_gt (gt : Term.t) : Term.t =
  Cerb_debug.print_debug 2 [] (fun () -> "branch_pruning");
  gt |> Inconsistent.transform_gt |> Unused.transform_gt
