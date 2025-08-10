module BT = BaseTypes
module IT = IndexTerms

module Make (AD : Domain.T) = struct
  module Term = Term.Make (AD)

  let rec is_external (gt : Term.t) : bool =
    let (Annot (gt_, (), _, _)) = gt in
    match gt_ with
    | `Arbitrary | `Return _ -> false
    | `Call _ -> true
    | `Pick gts -> gts |> List.exists is_external
    | `Asgn (_, _, gt_rest) -> is_external gt_rest
    | `LetStar ((_, gt_inner), gt_rest) -> is_external gt_inner || is_external gt_rest
    | `Assert (_, gt_rest) -> is_external gt_rest
    | `ITE (_, gt_then, gt_else) -> is_external gt_then || is_external gt_else
    | `Map (_, gt_inner) -> is_external gt_inner


  let rec dnf_ (e : BT.t IT.term) : BT.t IT.term =
    match e with
    | Unop (Not, e') ->
      (match dnf e' with
       (* Double negation elimination *)
       | IT (Unop (Not, IT (e, _, _)), _, _) -> e
       (* Flip inequalities *)
       | IT (Binop (LT, e1, e2), _, _) -> Binop (LE, e2, e1)
       | IT (Binop (LE, e1, e2), _, _) -> Binop (LT, e2, e1)
       (* De Morgan's Law *)
       | IT (Binop (And, e1, e2), info, loc) ->
         Binop
           ( Or,
             dnf (IT.IT (Unop (Not, e1), info, loc)),
             dnf (IT (Unop (Not, e2), info, loc)) )
       | IT (Binop (Or, e1, e2), info, loc) ->
         Binop
           ( And,
             dnf (IT (Unop (Not, e1), info, loc)),
             dnf (IT (Unop (Not, e2), info, loc)) )
       (* Otherwise *)
       | e'' -> Unop (Not, e''))
    | Binop (And, e1, e2) ->
      (match (dnf e1, dnf e2) with
       (* Distribute conjunctions *)
       | e', IT (Binop (Or, e_x, e_y), info, loc)
       | IT (Binop (Or, e_x, e_y), info, loc), e' ->
         Binop
           ( Or,
             dnf (IT (Binop (And, e', e_x), info, loc)),
             dnf (IT (Binop (And, e', e_y), info, loc)) )
       | e1, e2 -> Binop (And, e1, e2))
    | _ -> e


  and dnf (e : IT.t) : IT.t =
    let (IT (e, info, loc)) = e in
    IT (dnf_ e, info, loc)


  let listify_constraints (it : IT.t) : IT.t list =
    let rec loop (c : IT.t) : IT.t list =
      match c with IT (Binop (Or, e1, e2), _, _) -> loop e1 @ loop e2 | _ -> [ c ]
    in
    loop it


  let transform_gt (gt : Term.t) : Term.t =
    let rec aux (ext : Sym.Set.t) (gt : Term.t) : Term.t =
      let (Annot (gt_, (), bt, loc)) = gt in
      match gt_ with
      | `Arbitrary | `Call _ | `Return _ -> gt
      | `Pick gts -> Term.pick_ (List.map (aux ext) gts) () bt loc
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        Term.asgn_ ((it_addr, sct), it_val, aux ext gt_rest) () loc
      | `LetStar ((x, gt_inner), gt_rest) ->
        let gt_inner = aux ext gt_inner in
        let ext = if is_external gt_inner then Sym.Set.add x ext else ext in
        Term.let_star_ ((x, gt_inner), aux ext gt_rest) () loc
      | `Assert (T it, gt') ->
        let it = dnf it in
        let gt' = aux ext gt' in
        (match it with
         | IT (Binop (Or, _, _), _, _) ->
           let its_split, its_left =
             it
             |> listify_constraints
             |> List.partition (fun it' ->
               match it with
               | IT (Binop (EQ, IT (Sym x, _, _), _), _, _) when not (Sym.Set.mem x ext)
                 ->
                 true
               | IT (Binop (EQ, _, IT (Sym x, _, _)), _, _) when not (Sym.Set.mem x ext)
                 ->
                 true
               | _ -> Sym.Set.disjoint ext (IT.free_vars it'))
           in
           if List.is_empty its_split then
             gt
           else (
             let gt' =
               if List.is_empty its_left then
                 gt'
               else (
                 let it' =
                   List.fold_left
                     (fun it1 it2 -> IT.or2_ (it1, it2) loc)
                     (List.hd its_left)
                     (List.tl its_left)
                 in
                 Term.assert_ (T it', gt') () loc)
             in
             let cases =
               its_split |> List.map (fun it' -> Term.assert_ (T it', gt') () loc)
             in
             Term.pick_ cases () bt loc)
         | _ -> Term.assert_ (T it, gt') () loc)
      | `Assert ((Forall _ as lc), gt_rest) -> Term.assert_ (lc, aux ext gt_rest) () loc
      | `ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (it_if, aux ext gt_then, aux ext gt_else) () loc
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        Term.map_ ((i, i_bt, it_perm), aux ext gt_inner) () loc
    in
    aux Sym.Set.empty gt
end
