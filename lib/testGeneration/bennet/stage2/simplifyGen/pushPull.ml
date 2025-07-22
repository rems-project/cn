module IT = IndexTerms

let pull_out_inner_generators (gt : Term.t) : Term.t =
  let aux (gt : Term.t) : Term.t =
    match gt with
    | GT (`LetStar ((x, gt1), gt2), _, loc_let) ->
      (match gt1 with
       | GT (`Asgn ((it_addr, sct), it_val, gt3), _, loc_asgn) ->
         Term.asgn_
           ((it_addr, sct), it_val, Term.let_star_ ((x, gt3), gt2) loc_let)
           loc_asgn
       | GT (`LetStar ((y, gt3), gt4), _, loc_let') ->
         let z = Sym.fresh_anon () in
         let gt4 =
           Term.subst
             (IT.make_subst [ (y, IT.sym_ (z, Term.bt gt3, Locations.other __LOC__)) ])
             gt4
         in
         Term.let_star_ ((z, gt3), Term.let_star_ ((x, gt4), gt2) loc_let) loc_let'
       | GT (`Assert (lc, gt3), _, loc_assert) ->
         Term.assert_ (lc, Term.let_star_ ((x, gt3), gt2) loc_let) loc_assert
       | GT (`ITE (it_if, gt_then, gt_else), _, loc_ite) ->
         Term.ite_
           ( it_if,
             Term.let_star_ ((x, gt_then), gt2) loc_let,
             Term.let_star_ ((x, gt_else), gt2) loc_let )
           loc_ite
       | GT (`Pick wgts, bt, loc_pick) ->
         Term.pick_
           (List.map_snd (fun gt' -> Term.let_star_ ((x, gt'), gt2) loc_let) wgts)
           bt
           loc_pick
       | _ -> gt)
    | _ -> gt
  in
  Term.map_gen_post aux gt


let push_in_outer_generators (gt : Term.t) : Term.t =
  let aux (gt : Term.t) : Term.t =
    match gt with
    | GT (`Asgn ((it_addr, sct), it_val, GT (`Pick wgts, _, loc_pick)), bt, loc_asgn) ->
      Term.pick_
        (List.map_snd (fun gt' -> Term.asgn_ ((it_addr, sct), it_val, gt') loc_asgn) wgts)
        bt
        loc_pick
    | GT (`LetStar ((x, gt_inner), GT (`Pick wgts, _, loc_pick)), bt, loc_let) ->
      Term.pick_
        (List.map_snd (fun gt' -> Term.let_star_ ((x, gt_inner), gt') loc_let) wgts)
        bt
        loc_pick
    | GT (`LetStar ((x, GT (`Pick wgts, _, loc_pick)), gt_rest), bt, loc_let) ->
      Term.pick_
        (List.map_snd (fun gt' -> Term.let_star_ ((x, gt'), gt_rest) loc_let) wgts)
        bt
        loc_pick
    | GT (`LetStar ((x, GT (`ITE (it_if, gt_then, gt_else), _, loc_ite)), gt2), _, loc_let)
      ->
      Term.ite_
        ( it_if,
          Term.let_star_ ((x, gt_then), gt2) loc_let,
          Term.let_star_ ((x, gt_else), gt2) loc_let )
        loc_ite
    | GT (`Assert (lc, GT (`Pick wgts, _, loc_pick)), bt, loc_assert) ->
      Term.pick_
        (List.map_snd (fun gt' -> Term.assert_ (lc, gt') loc_assert) wgts)
        bt
        loc_pick
    | _ -> gt
  in
  Term.map_gen_pre aux gt


let transform_gt (gt : Term.t) : Term.t =
  Cerb_debug.print_debug 2 [] (fun () -> "push_pull");
  let rec loop (gt : Term.t) : Term.t =
    let old_gt = gt in
    let new_gt = gt |> pull_out_inner_generators |> push_in_outer_generators in
    if Term.equal old_gt new_gt then new_gt else loop new_gt
  in
  loop gt
