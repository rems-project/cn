module IT = IndexTerms
module LC = LogicalConstraints

let transform_gt (gt : Term.t) : Term.t =
  let aux (gt : Term.t) : Term.t =
    let (GT (gt_, _bt, loc)) = gt in
    match gt_ with
    | Assert (T (IT (Let ((x, it_inner), it_rest), _, loc_let)), gt') ->
      Term.let_star_
        ( (x, Term.return_ it_inner (IT.get_loc it_inner)),
          Term.assert_ (LC.T it_rest, gt') loc )
        loc_let
    | Assert (Forall ((_i_sym, _i_bt), IT (Let _, _, _)), _) ->
      (* TODO: Pull out lets that don't refer to `i_sym` *)
      gt
    | _ -> gt
  in
  Term.map_gen_pre aux gt


let transform_gd (gd : Def.t) : Def.t = Def.{ gd with body = transform_gt gd.body }

let transform (ctx : Ctx.t) : Ctx.t = List.map_snd transform_gd ctx
