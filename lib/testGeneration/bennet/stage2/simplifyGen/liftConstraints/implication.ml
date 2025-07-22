module GT = Term

let transform_gt (gt : GT.t) : GT.t =
  let aux (gt : GT.t) : GT.t =
    let (GT (gt_, _bt, loc)) = gt in
    match gt_ with
    | `Assert (T (IT (Binop (Implies, it_if, it_then), _, loc_implies)), gt') ->
      GT.ite_ (it_if, GT.assert_ (T it_then, gt') loc, gt') loc_implies
    | _ -> gt
  in
  GT.map_gen_pre aux gt
