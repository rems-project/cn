module IT = IndexTerms
module LC = LogicalConstraints

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let transform_gt (fast : bool) (tm : Term.t) : Term.t Typing.t =
    let open Typing in
    let rec aux (tm : Term.t) : Term.t option Typing.t =
      let here = Locations.other __LOC__ in
      let (Annot (tm_, (), bt, loc)) = tm in
      match tm_ with
      | `Arbitrary | `ArbitraryDomain _ | `Symbolic | `Return _ | `Call _ ->
        let@ check = provable loc in
        return
          (match check (LC.T (IT.bool_ false here)) with
           | `True -> None
           | `False -> Some tm)
      | `Pick gts ->
        let@ check = provable loc in
        (match check (LC.T (IT.bool_ false here)) with
         | `True -> return None
         | `False ->
           let rec loop gts =
             match gts with
             | tm :: wgts' ->
               let@ otm = pure (aux tm) in
               let@ wgts' = loop wgts' in
               return (otm :: wgts')
             | [] -> return []
           in
           let@ gts = loop gts in
           let gts = List.filter_map (fun x -> x) gts in
           return
             (if List.is_empty gts then
                None
              else
                Some (Term.pick_ gts () bt loc)))
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        let@ () =
          if fast then
            add_c loc (LC.T (IT.ne_ (it_addr, IT.null_ loc) loc))
          else (
            let sym = Sym.fresh_anon () in
            let it_sym = IT.sym_ (sym, IT.get_bt it_val, loc) in
            let@ () =
              add_l_value sym it_val (loc, lazy Pp.(Sym.pp sym ^^^ !^"=" ^^^ IT.pp it_val))
            in
            add_r
              loc
              (P { name = Owned (sct, Init); pointer = it_addr; iargs = [] }, O it_sym))
        in
        let@ gt_rest = aux gt_rest in
        return
          (let open Option in
           let@ gt_rest in
           return (Term.asgn_ ((it_addr, sct), it_val, gt_rest) () loc))
      | `LetStar ((x, Annot (`Return it, (), _, loc_ret)), gt_rest) ->
        let@ () = add_l_value x it (loc, lazy (Sym.pp x)) in
        let@ gt_rest = aux gt_rest in
        return
          (let open Option in
           let@ gt_rest in
           return (Term.let_star_ ((x, Term.return_ it () loc_ret), gt_rest) () loc))
      | `LetStar ((x, gt_inner), gt_rest) ->
        let@ gt_inner = pure (aux gt_inner) in
        (match gt_inner with
         | Some gt_inner ->
           let@ () = add_l x (Term.basetype gt_inner) (loc, lazy (Sym.pp x)) in
           let@ gt_rest = aux gt_rest in
           return
             (let open Option in
              let@ gt_rest in
              return (Term.let_star_ ((x, gt_inner), gt_rest) () loc))
         | None -> return None)
      | `Assert (lc, gt_rest) ->
        let@ check = provable loc in
        let@ redundant =
          match check lc with
          | `True -> return true
          | `False ->
            let@ () = add_c loc lc in
            return false
        in
        let@ gt_rest = aux gt_rest in
        return
          (let open Option in
           let@ gt_rest in
           return (if redundant then gt_rest else Term.assert_ (lc, gt_rest) () loc))
      | `AssertDomain (domain, gt_rest) ->
        let@ gt_rest = aux gt_rest in
        return
          (let open Option in
           let@ gt_rest in
           return (Term.assert_domain_ (domain, gt_rest) () loc))
      | `ITE (it_if, gt_then, gt_else) ->
        let@ check = provable loc in
        let@ ogt_then =
          match check (LC.T (IT.not_ it_if here)) with
          | `False ->
            pure
              (let@ () = add_c loc (LC.T it_if) in
               aux gt_then)
          | `True -> return None
        in
        let@ ogt_else =
          match check (LC.T it_if) with
          | `False ->
            pure
              (let@ () = add_c loc (LC.T (IT.not_ it_if here)) in
               aux gt_else)
          | `True -> return None
        in
        return
          (match (ogt_then, ogt_else) with
           | Some gt_then, Some gt_else ->
             Some (Term.ite_ (it_if, gt_then, gt_else) () loc)
           | Some gt, None | None, Some gt -> Some gt
           | None, None -> None)
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        let@ gt_inner =
          pure
            (let@ () = add_l i i_bt (loc, lazy (Sym.pp i)) in
             let@ () = add_c loc (LC.T it_perm) in
             aux gt_inner)
        in
        return
          (let open Option in
           let@ gt_inner in
           return (Term.map_ ((i, i_bt, it_perm), gt_inner) () loc))
    in
    let@ res = aux tm in
    return (Option.get res)


  let transform_gd (paused : _ Typing.pause) (fast : bool) (def : Def.t) : Def.t =
    let f () =
      Typing.run_from_pause
        (fun _ ->
           let open Typing in
           let@ () = init_solver () in
           let@ () =
             List.fold_left
               (fun acc (x, x_bt) ->
                  let@ () = add_a x x_bt (Locations.other __LOC__, lazy (Sym.pp x)) in
                  acc)
               (return ())
               def.iargs
           in
           transform_gt fast def.body)
        paused
    in
    match f () with
    | Ok body -> { def with body }
    | Error err ->
      TypeErrors.report_pretty err;
      exit 1


  let transform (paused : _ Typing.pause) (fast : bool) (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "smt_pruning");
    List.map_snd (transform_gd paused fast) ctx
end
