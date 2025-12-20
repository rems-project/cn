module IT = IndexTerms
module LC = LogicalConstraints

module Make (AD : Domain.T) = struct
  module Term = Term.Make (AD)

  let transform_it (it : IT.t) : IT.t =
    let aux (it : IT.t) : IT.t =
      match it with
      | IT (Unop (op, IT (ITE (it_if, it_then, it_else), _, loc_ite)), bt_unop, loc_unop)
        ->
        let f it' = IT.IT (Unop (op, it'), bt_unop, loc_unop) in
        IT.ite_ (it_if, f it_then, f it_else) loc_ite
      | IT
          ( Binop (op, IT (ITE (it_if, it_then, it_else), _, loc_ite), it2),
            bt_binop,
            loc_binop ) ->
        let f it' = IT.IT (Binop (op, it', it2), bt_binop, loc_binop) in
        IT.ite_ (it_if, f it_then, f it_else) loc_ite
      | IT
          ( Binop (op, it1, IT (ITE (it_if, it_then, it_else), _, loc_ite)),
            bt_binop,
            loc_binop ) ->
        let f it' = IT.IT (Binop (op, it1, it'), bt_binop, loc_binop) in
        IT.ite_ (it_if, f it_then, f it_else) loc_ite
      | IT
          ( EachI ((min, (i, i_bt), max), IT (ITE (it_if, it_then, it_else), _, loc_ite)),
            _,
            loc_each )
        when not (Sym.Set.mem i (IT.free_vars it_if)) ->
        let f it' = IT.eachI_ (min, (i, i_bt), max) it' loc_each in
        IT.ite_ (it_if, f it_then, f it_else) loc_ite
      | IT (Cast (cast_bt, IT (ITE (it_if, it_then, it_else), _, loc_ite)), _, loc_cast)
        ->
        let f it' = IT.cast_ cast_bt it' loc_cast in
        IT.ite_ (it_if, f it_then, f it_else) loc_ite
      (* TODO: Complete cases *)
      | _ -> it
    in
    IT.map_term_post aux it


  let transform_lc (lc : LC.t) : LC.t =
    match lc with
    | T it -> T (transform_it it)
    | Forall ((i, i_bt), IT (Binop (Implies, it_perm, it_body), _, loc_implies)) ->
      let it_perm = transform_it it_perm in
      let it_body = transform_it it_body in
      LC.Forall ((i, i_bt), IT.impl_ (it_perm, it_body) loc_implies)
    | _ -> failwith __LOC__


  let transform_gt (gt : Term.t) : Term.t =
    let aux (gt : Term.t) : Term.t =
      let (Annot (gt_, (), bt, loc)) = gt in
      match gt_ with
      | `Call (fsym, iargs) -> Term.call_ (fsym, List.map transform_it iargs) () bt loc
      | `Asgn ((it_addr, sct), it_val, gt') ->
        Term.asgn_ ((transform_it it_addr, sct), transform_it it_val, gt') () loc
      | `Return it -> Term.return_ (transform_it it) () loc
      | `Assert (lc, gt') ->
        (match transform_lc lc with
         | T (IT (ITE (it_if, it_then, it_else), _, loc_ite)) ->
           Term.ite_
             ( it_if,
               Term.assert_ (T it_then, gt') () loc,
               Term.assert_ (T it_else, gt') () loc )
             ()
             loc_ite
         | lc' -> Term.assert_ (lc', gt') () loc)
      | `ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (transform_it it_if, gt_then, gt_else) () loc
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        (match transform_it it_perm with
         | IT (ITE (it_if, it_then, it_else), _, loc_ite)
           when not (Sym.Set.mem i (IT.free_vars it_if)) ->
           Term.ite_
             ( it_if,
               Term.map_ ((i, i_bt, it_then), gt_inner) () loc,
               Term.map_ ((i, i_bt, it_else), gt_inner) () loc )
             ()
             loc_ite
         | it_perm' -> Term.map_ ((i, i_bt, it_perm'), gt_inner) () loc)
      | _ -> gt
    in
    Term.map_gen_post aux gt
end
