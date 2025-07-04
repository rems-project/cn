module IT = IndexTerms
module LC = LogicalConstraints

let transform_gt (paused : _ Typing.pause) (tm : Term.t) : Term.t =
  let open Typing in
  let rec aux (tm : Term.t) : Term.t Typing.t =
    let here = Locations.other __LOC__ in
    let (GT (tm_, bt, loc)) = tm in
    match tm_ with
    | Uniform | Alloc | Return _ | Call _ -> return tm
    | Pick wgts ->
      let rec loop wgts =
        match wgts with
        | (w, tm) :: wgts' ->
          let@ otm =
            pure
              (let@ tm' = aux tm in
               let@ provable = provable loc in
               return
                 (match provable (LC.T (IT.bool_ false here)) with
                  | `True -> None
                  | `False -> Some (w, tm')))
          in
          let@ wgts' = loop wgts' in
          return (otm :: wgts')
        | [] -> return []
      in
      let@ wgts = loop wgts in
      return (Term.pick_ (List.filter_map (fun x -> x) wgts) bt loc)
    | Asgn ((it_addr, sct), it_val, gt_rest) ->
      (* TODO: Reason about separation? *)
      let@ gt_rest = aux gt_rest in
      return (Term.asgn_ ((it_addr, sct), it_val, gt_rest) loc)
    | LetStar ((x, GT (Return it, _, loc_ret)), gt_rest) ->
      let@ () = add_l_value x it (loc, lazy (Sym.pp x)) in
      let@ gt_rest = aux gt_rest in
      return (Term.let_star_ ((x, Term.return_ it loc_ret), gt_rest) loc)
    | LetStar ((x, gt_inner), gt_rest) ->
      let@ gt_inner = aux gt_inner in
      let@ () = add_l x bt (loc, lazy (Sym.pp x)) in
      let@ gt_rest = aux gt_rest in
      return (Term.let_star_ ((x, gt_inner), gt_rest) loc)
    | Assert (lc, gt_rest) ->
      let@ () = add_c loc lc in
      let@ gt_rest = aux gt_rest in
      return (Term.assert_ (lc, gt_rest) loc)
    | ITE (it_if, gt_then, gt_else) ->
      let@ ogt_then =
        pure
          (let@ () = add_c loc (LC.T it_if) in
           let@ gt_then = aux gt_then in
           let@ check = provable loc in
           return
             (match check (LC.T (IT.bool_ false here)) with
              | `True -> None
              | `False -> Some gt_then))
      in
      let@ ogt_else =
        pure
          (let@ () = add_c loc (LC.T (IT.not_ it_if here)) in
           let@ gt_else = aux gt_else in
           let@ provable = provable loc in
           return
             (match provable (LC.T (IT.bool_ false here)) with
              | `True -> None
              | `False -> Some gt_else))
      in
      return
        (match (ogt_then, ogt_else) with
         | Some gt_then, Some gt_else -> Term.ite_ (it_if, gt_then, gt_else) loc
         | Some gt, None | None, Some gt -> gt
         | None, None -> Term.pick_ [] bt loc)
    | Map ((i, i_bt, it_perm), gt_inner) ->
      let@ () = add_l i i_bt (loc, lazy (Sym.pp i)) in
      let@ () = add_c loc (LC.T it_perm) in
      let@ gt_inner = aux gt_inner in
      return (Term.map_ ((i, i_bt, it_perm), gt_inner) loc)
  in
  Result.get_ok
    (Typing.run_from_pause
       (fun _ ->
          let@ () = init_solver () in
          aux tm)
       paused)


let transform_gd (paused : _ Typing.pause) (def : Def.t) : Def.t =
  { def with body = transform_gt paused def.body }


let transform (paused : _ Typing.pause) (ctx : Ctx.t) : Ctx.t =
  List.map_snd (transform_gd paused) ctx
