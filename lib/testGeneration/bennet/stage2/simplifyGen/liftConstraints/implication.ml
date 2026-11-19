module Make (AD : Domain.T) = struct
  module Term = Term.Make (AD)

  let transform_gt (gt : Term.t) : Term.t =
    let aux (gt : Term.t) : Term.t =
      let (Annot (gt_, (), _bt, loc)) = gt in
      match gt_ with
      | `Assert (T (IT (Binop (Implies, it_if, it_then), _, loc_implies)), gt') ->
        Term.ite_ (it_if, Term.assert_ (T it_then, gt') () loc, gt') () loc_implies
      | _ -> gt
    in
    Term.map_gen_pre aux gt
end
