module IT = IndexTerms

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let rec contains_bottom_domain (gt : Term.t) : bool =
    let (Annot (gt_, (), _, _)) = gt in
    match gt_ with
    | `Arbitrary | `Symbolic | `ArbitrarySpecialized _ | `ArbitraryDomain _ | `Call _
    | `Return _ ->
      false
    | `AssertDomain (ad, gt') -> AD.equal ad AD.bottom || contains_bottom_domain gt'
    | `Pick gts ->
      List.is_empty gts || List.for_all (fun gt' -> contains_bottom_domain gt') gts
    | `ITE (_, gt_then, gt_else) ->
      contains_bottom_domain gt_then && contains_bottom_domain gt_else
    | `LetStar ((_, gt_inner), gt') ->
      contains_bottom_domain gt_inner || contains_bottom_domain gt'
    | `Assert (_, gt') -> contains_bottom_domain gt'
    | `Asgn (_, _, gt') -> contains_bottom_domain gt'
    | `Map (_, gt') -> contains_bottom_domain gt'


  let transform_gt (gt : Term.t) : Term.t =
    let aux (gt : Term.t) : Term.t =
      match gt with
      | Annot (`Pick gts, (), bt, loc) ->
        Term.pick_
          (List.filter (fun gt' -> not (contains_bottom_domain gt')) gts)
          ()
          bt
          loc
      | Annot (`ITE (it_if, gt_then, gt_else), (), _, loc) ->
        if contains_bottom_domain gt_else then
          Term.assert_ (T it_if, gt_then) () loc
        else if contains_bottom_domain gt_then then
          Term.assert_ (T (IT.not_ it_if (IT.get_loc it_if)), gt_else) () loc
        else
          gt
      | _ -> gt
    in
    Term.map_gen_post aux gt


  let transform_gd (gd : Def.t) : Def.t = { gd with body = transform_gt gd.body }

  let transform (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "ad_pruning");
    List.map_snd transform_gd ctx
end
