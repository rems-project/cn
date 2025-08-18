module IT = IndexTerms

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let unfold (max_unfolds : int) (ctx : Ctx.t) : Ctx.t =
    if max_unfolds <= 0 then
      ctx
    else (
      let transform_gt (ctx : Ctx.t) (gt : Term.t) : Term.t =
        let rec aux (depth : int) (gt : Term.t) : Term.t =
          let (Annot (gt_, (), _bt, loc)) = gt in
          match gt_ with
          | `Arbitrary | `Symbolic | `Return _ -> gt
          | `Call (fsym, iargs) when depth < max_unfolds ->
            (try
               let gd = Ctx.find fsym ctx in
               aux
                 (depth + 1)
                 (Term.subst
                    (IT.make_subst (List.combine (List.map fst gd.iargs) iargs))
                    gd.body)
             with
             | Not_found -> gt (* Keep call if definition not found *))
          | `Call _ -> gt (* At max depth or definition not found, keep call *)
          | `Asgn ((it_addr, sct), it_val, gt_rest) ->
            Term.asgn_ ((it_addr, sct), it_val, aux depth gt_rest) () loc
          | `LetStar ((x, gt_inner), gt_rest) ->
            Term.let_star_ ((x, aux depth gt_inner), aux depth gt_rest) () loc
          | `Assert (lc, gt_rest) -> Term.assert_ (lc, aux depth gt_rest) () loc
          | `ITE (it_if, gt_then, gt_else) ->
            Term.ite_ (it_if, aux depth gt_then, aux depth gt_else) () loc
          | `Map ((i, i_bt, it_perm), gt_inner) ->
            Term.map_ ((i, i_bt, it_perm), aux depth gt_inner) () loc
        in
        aux 0 gt
      in
      let transform_gd (gd : Def.t) : Def.t =
        { gd with body = transform_gt ctx gd.body }
      in
      List.map_snd transform_gd ctx)
end
