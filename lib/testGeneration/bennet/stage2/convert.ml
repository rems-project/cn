module BT = BaseTypes

let rec transform_gt (gt : Stage1.Term.t) : Term.t =
  let (GT (gt_, bt, loc)) = gt in
  let gt_ =
    match gt_ with
    | Arbitrary ->
      (match bt with
       | Loc () -> Term.Alloc
       | Bits _ -> Term.Uniform
       | _ -> failwith Pp.(plain (BT.pp bt ^^^ at ^^^ !^__LOC__)))
    | Call (fsym, args) -> Call (fsym, args)
    | Asgn ((it_addr, sct), it_val, gt_rest) ->
      Asgn ((it_addr, sct), it_val, transform_gt gt_rest)
    | LetStar ((x, gt_inner), gt_rest) ->
      LetStar ((x, transform_gt gt_inner), transform_gt gt_rest)
    | Return it -> Return it
    | Assert (lc, gt_rest) -> Assert (lc, transform_gt gt_rest)
    | ITE (it_if, gt_then, gt_else) ->
      ITE (it_if, transform_gt gt_then, transform_gt gt_else)
    | Map ((i_sym, i_bt, it_perm), gt_inner) ->
      Map ((i_sym, i_bt, it_perm), transform_gt gt_inner)
  in
  GT (gt_, bt, loc)


let transform_gd ({ filename; recursive; spec; name; iargs; oargs; body } : Stage1.Def.t)
  : Def.t
  =
  Def.{ filename; recursive; spec; name; iargs; oargs; body = transform_gt body }


let transform (ctx : Stage1.Ctx.t) : Ctx.t = List.map_snd transform_gd ctx
