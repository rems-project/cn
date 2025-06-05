let transform_gt (gt : Term.t) : Term.t =
  let aux (gt : Term.t) : Term.t =
    let (GT (gt_, _bt, loc)) = gt in
    match gt_ with
    | Assert (T (IT (ITE (it_if, it_then, it_else), _, loc_ite)), gt') ->
      Term.ite_
        (it_if, Term.assert_ (T it_then, gt') loc, Term.assert_ (T it_else, gt') loc)
        loc_ite
    | _ -> gt
  in
  Term.map_gen_pre aux gt


let transform_gd (gd : Def.t) : Def.t = Def.{ gd with body = transform_gt gd.body }

let transform (ctx : Ctx.t) : Ctx.t = List.map_snd transform_gd ctx
