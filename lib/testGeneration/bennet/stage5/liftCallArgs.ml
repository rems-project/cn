module T = Terms.Normal
module MT = MakeTerm

(** Lift compound call arguments into pure bindings:

    [call f(root, min, key - 1)]
    becomes
    [let* t = return (key - 1); call f(root, min, t)]

    The runtime blames failures by pointer identity on cn value objects. A
    compound argument is materialized as an anonymous temporary in the caller,
    so when a failure escapes the callee blaming one of its parameters, no
    enclosing [BENNET_LET] matches and the failure discards at the top level.
    Binding the argument via [let* t = return _] places it in the backtrack
    chain: [BENNET_LET_RETURN]'s unwinding translates blame on [t] to the
    argument's free variables, which enclosing binders can catch. Arguments
    that are already plain variables need no lifting, since the callee
    parameter aliases the caller variable's pointer. *)
module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let lift_iargs (iargs : T.t list) (loc : Locations.t) : (Sym.t * T.t) list * T.t list =
    let lets, iargs' =
      iargs
      |> List.map (fun it ->
        match Terms.is_sym it with
        | Some _ -> (None, it)
        | None ->
          let t = Sym.fresh_anon () in
          (Some (t, it), MT.sym_ (t, T.get_bt it, loc)))
      |> List.split
    in
    (List.filter_map (fun x -> x) lets, iargs')


  let wrap_lets (lets : (Sym.t * T.t) list) (gt : Term.t) (loc : Locations.t) : Term.t =
    List.fold_right
      (fun (t, it) gt' -> Term.let_star_ ((t, Term.return_ it () loc), gt') () loc)
      lets
      gt


  let transform_gd (gd : Def.t) : Def.t =
    let rec aux (gt : Term.t) : Term.t =
      let (Annot (gt_, (), bt, loc)) = gt in
      match gt_ with
      (* Hoist the lifted bindings above a let-bound call, rather than nesting
         them in its right-hand side: backtrack labels inside a GNU statement
         expression cannot be jumped into from outside it. *)
      | `LetStar ((x, Annot (`Call (fsym, iargs), (), x_bt, x_loc)), gt_rest) ->
        let lets, iargs' = lift_iargs iargs x_loc in
        let call' = Term.call_ (fsym, iargs') () x_bt x_loc in
        wrap_lets lets (Term.let_star_ ((x, call'), aux gt_rest) () loc) loc
      | `LetStar ((x, Annot (`CallSized (fsym, iargs, sz), (), x_bt, x_loc)), gt_rest) ->
        let lets, iargs' = lift_iargs iargs x_loc in
        let call' = Term.call_sized_ (fsym, iargs', sz) () x_bt x_loc in
        wrap_lets lets (Term.let_star_ ((x, call'), aux gt_rest) () loc) loc
      | `Call (fsym, iargs) ->
        let lets, iargs' = lift_iargs iargs loc in
        wrap_lets lets (Term.call_ (fsym, iargs') () bt loc) loc
      | `CallSized (fsym, iargs, sz) ->
        let lets, iargs' = lift_iargs iargs loc in
        wrap_lets lets (Term.call_sized_ (fsym, iargs', sz) () bt loc) loc
      | `Arbitrary | `Symbolic | `ArbitrarySpecialized _ | `ArbitraryDomain _ | `Return _
        ->
        gt
      | `PickSized wgts -> Term.pick_sized_ (List.map_snd aux wgts) () bt loc
      | `Asgn ((it_addr, sct), it_val, gt') ->
        Term.asgn_ ((it_addr, sct), it_val, aux gt') () loc
      | `LetStar ((x, gt_inner), gt_rest) ->
        Term.let_star_ ((x, aux gt_inner), aux gt_rest) () loc
      | `Assert (lc, gt') -> Term.assert_ (lc, aux gt') () loc
      | `AssertDomain (ad, gt') -> Term.assert_domain_ (ad, aux gt') () loc
      | `ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (it_if, aux gt_then, aux gt_else) () loc
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        Term.map_ ((i, i_bt, it_perm), aux gt_inner) () loc
      | `SplitSize (syms, gt') -> Term.split_size_ (syms, aux gt') () loc
    in
    { gd with body = aux gd.body }


  let transform (ctx : Ctx.t) : Ctx.t = List.map_snd transform_gd ctx
end
