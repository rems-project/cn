let transform_gt (tm : Term.t) : Term.t =
  let aux (tm : Term.t) : Term.t =
    match tm with
    | GT (`Pick wgts, bt, loc) ->
      let wgts =
        let gcd =
          List.fold_left
            (fun x y -> Z.gcd x y)
            (fst (List.hd wgts))
            (List.map fst (List.tl wgts))
        in
        List.map_fst (fun x -> Z.div x gcd) wgts
      in
      let w_sum = List.fold_left Z.add Z.zero (List.map fst wgts) in
      let max_int = Z.of_int Int.max_int in
      let f =
        if Z.leq w_sum max_int then
          fun w -> w
        else
          fun w -> Z.max Z.one (Z.div w (Z.div (Z.add w_sum (Z.pred max_int)) max_int))
      in
      Term.pick_ (List.map (fun (w, gt) -> (f w, gt)) wgts) bt loc
    | gt -> gt
  in
  Term.map_gen_pre aux tm


let transform_gd (gd : Def.t) : Def.t = { gd with body = transform_gt gd.body }

let transform (ctx : Ctx.t) : Ctx.t =
  Cerb_debug.print_debug 2 [] (fun () -> "minimize_pick_weights");
  List.map_snd transform_gd ctx
