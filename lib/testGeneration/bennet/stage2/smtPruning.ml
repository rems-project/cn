module IT = IndexTerms
module LC = LogicalConstraints

module Make (AD : GenTerms.Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let transform_gt (fast : bool) (tm : Term.t) : Term.t Typing.t =
    let open Typing in
    let rec aux (tm : Term.t) : Term.t Typing.t =
      let here = Locations.other __LOC__ in
      let (Annot (tm_, (), bt, loc)) = tm in
      match tm_ with
      | `Arbitrary _ | `Return _ | `Call _ -> return tm
      | `Pick gts ->
        let rec loop gts =
          match gts with
          | tm :: wgts' ->
            let@ otm =
              pure
                (let@ tm' = aux tm in
                 let@ provable = provable loc in
                 return
                   (match provable (LC.T (IT.bool_ false here)) with
                    | `True -> None
                    | `False -> Some tm'))
            in
            let@ wgts' = loop wgts' in
            return (otm :: wgts')
          | [] -> return []
        in
        let@ gts = loop gts in
        return (Term.pick_ (List.filter_map (fun x -> x) gts) () bt loc)
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
        return (Term.asgn_ ((it_addr, sct), it_val, gt_rest) () loc)
      | `LetStar ((x, Annot (`Return it, (), _, loc_ret)), gt_rest) ->
        let@ () = add_l_value x it (loc, lazy (Sym.pp x)) in
        let@ gt_rest = aux gt_rest in
        return (Term.let_star_ ((x, Term.return_ it () loc_ret), gt_rest) () loc)
      | `LetStar ((x, gt_inner), gt_rest) ->
        let@ gt_inner = aux gt_inner in
        let@ () = add_l x (Term.basetype gt_inner) (loc, lazy (Sym.pp x)) in
        let@ gt_rest = aux gt_rest in
        return (Term.let_star_ ((x, gt_inner), gt_rest) () loc)
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
        return (if redundant then gt_rest else Term.assert_ (lc, gt_rest) () loc)
      | `ITE (it_if, gt_then, gt_else) ->
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
           | Some gt_then, Some gt_else -> Term.ite_ (it_if, gt_then, gt_else) () loc
           | Some gt, None | None, Some gt -> gt
           | None, None -> Term.pick_ [] () bt loc)
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        let@ gt_inner =
          pure
            (let@ () = add_l i i_bt (loc, lazy (Sym.pp i)) in
             let@ () = add_c loc (LC.T it_perm) in
             aux gt_inner)
        in
        return (Term.map_ ((i, i_bt, it_perm), gt_inner) () loc)
    in
    aux tm


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
    | Ok _body -> def
    | Error err ->
      TypeErrors.report_pretty err;
      exit 1


  let transform (paused : _ Typing.pause) (fast : bool) (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "smt_pruning");
    List.map_snd (transform_gd paused fast) ctx
end
