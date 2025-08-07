module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

module Make (AD : Domain.T) = struct
  module Term = Term.Make (AD)

  let rec cnf_ (e : BT.t IT.term) : BT.t IT.term =
    match e with
    | Unop (Not, e') ->
      (match cnf e' with
       (* Double negation elimination *)
       | IT (Unop (Not, IT (e, _, _)), _, _) -> e
       (* Flip inequalities *)
       | IT (Binop (LT, e1, e2), _, _) -> Binop (LE, e2, e1)
       | IT (Binop (LE, e1, e2), _, _) -> Binop (LT, e2, e1)
       (* De Morgan's Law *)
       | IT (Binop (And, e1, e2), info, loc) ->
         Binop
           ( Or,
             cnf (IT.IT (Unop (Not, e1), info, loc)),
             cnf (IT (Unop (Not, e2), info, loc)) )
       | IT (Binop (Or, e1, e2), info, loc) ->
         Binop
           ( And,
             cnf (IT (Unop (Not, e1), info, loc)),
             cnf (IT (Unop (Not, e2), info, loc)) )
       (* Otherwise *)
       | e'' -> Unop (Not, e''))
    | Binop (Or, e1, e2) ->
      (match (cnf e1, cnf e2) with
       (* Distribute disjunction *)
       | e', IT (Binop (And, e_x, e_y), info, loc)
       | IT (Binop (And, e_x, e_y), info, loc), e' ->
         Binop
           ( And,
             cnf (IT (Binop (Or, e', e_x), info, loc)),
             cnf (IT (Binop (Or, e', e_y), info, loc)) )
       | e1, e2 -> Binop (Or, e1, e2))
    | _ -> e


  and cnf (e : IT.t) : IT.t =
    let (IT (e, info, loc)) = e in
    IT (cnf_ e, info, loc)


  let listify_constraints (it : IT.t) : IT.t list =
    let rec loop (c : IT.t) : IT.t list =
      match c with IT (Binop (And, e1, e2), _, _) -> loop e1 @ loop e2 | _ -> [ c ]
    in
    loop it


  let transform_gt (gt : Term.t) : Term.t =
    let aux (gt : Term.t) : Term.t =
      let (Annot (gt_, (), _bt, loc)) = gt in
      match gt_ with
      | `Assert (T it, gt') ->
        it
        |> cnf
        |> listify_constraints
        |> List.fold_left (fun gt_rest it' -> Term.assert_ (LC.T it', gt_rest) () loc) gt'
      | `Assert (Forall ((i_sym, i_bt), it), gt') ->
        let its_in, its_out =
          it
          |> cnf
          |> listify_constraints
          |> List.partition (fun it' -> Sym.Set.mem i_sym (IT.free_vars it'))
        in
        let gt_forall =
          Term.assert_ (LC.Forall ((i_sym, i_bt), IT.and_ its_in loc), gt') () loc
        in
        List.fold_left
          (fun gt_rest it' -> Term.assert_ (LC.T it', gt_rest) () loc)
          gt_forall
          its_out
      | _ -> gt
    in
    Term.map_gen_pre aux gt
end
