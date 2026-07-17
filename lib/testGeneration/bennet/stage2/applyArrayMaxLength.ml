module IT = MakeTerm
module LC = LogicalConstraints

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  (* Bound the length of every `each`/`Map` generator by `--max-array-length`
     (default 50). The assertion is placed *before* the map is generated, so an
     oversized index range is rejected in O(1) instead of building an unbounded
     array (which otherwise grows to ~buffer/element under the cheap allocator).

     We constrain the derived length `(it_max - it_min) + 1 <= max_array_length`,
     mirroring the symbolic engine (`bennet/symbolic/gather.ml`), rather than
     asserting on the raw upper bound `it_max`: this is correct when `it_min <> 0`
     and folds to a harmless constant for equality-set permissions.

     Guard: only emit the constraint when a genuine upper bound exists. For
     non-range permissions (e.g. `i == 1 || i > n`) `get_upper_bound_opt` returns
     `None` and `get_bounds` would fall back to the full type range, making the
     constraint unsatisfiable and breaking generation -- so those are skipped. *)
  let bound_length (i, i_bt, it_perm) (gt : Term.t) (loc : Locations.t) : Term.t =
    match TermBounds.get_upper_bound_opt (i, i_bt) it_perm with
    | None -> gt
    | Some it_max ->
      let it_min = TermBounds.get_lower_bound (i, i_bt) it_perm in
      let here = Locations.other __LOC__ in
      let array_len =
        IT.add_ (IT.sub_ (it_max, it_min) here, IT.num_lit_ (Z.of_int 1) i_bt here) here
      in
      let max_len =
        IT.num_lit_ (Z.of_int (TestGenConfig.get_max_array_length ())) i_bt here
      in
      Term.assert_ (LC.T (IT.le_ (array_len, max_len) here), gt) () loc


  let transform_gt (gt : Term.t) : Term.t =
    let rec aux (gt : Term.t) : Term.t =
      let (Annot (gt_, (), bt, loc)) = gt in
      match gt_ with
      | `Arbitrary | `Symbolic | `Call _ | `Return _ -> gt
      | `Pick gts -> Term.pick_ (List.map aux gts) () bt loc
      | `Asgn ((it_addr, sct), it_val, gt') ->
        Term.asgn_ ((it_addr, sct), it_val, aux gt') () loc
      | `LetStar
          ((x, Annot (`Map ((i, i_bt, it_perm), gt_inner), (), _, loc_map)), gt_rest) ->
        (* The common `each`: keep the `Map` as the direct RHS of the `let*` (so
           later stages still see `let*(Map)`), wrapping the whole binding. *)
        let mapped = Term.map_ ((i, i_bt, it_perm), aux gt_inner) () loc_map in
        let let_star = Term.let_star_ ((x, mapped), aux gt_rest) () loc in
        bound_length (i, i_bt, it_perm) let_star loc
      | `LetStar ((x, gt_inner), gt_rest) ->
        Term.let_star_ ((x, aux gt_inner), aux gt_rest) () loc
      | `Assert (lc, gt') -> Term.assert_ (lc, aux gt') () loc
      | `ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (it_if, aux gt_then, aux gt_else) () loc
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        (* A `Map` not directly bound by a `let*` (rare): wrap the map itself. *)
        let mapped = Term.map_ ((i, i_bt, it_perm), aux gt_inner) () loc in
        bound_length (i, i_bt, it_perm) mapped loc
    in
    aux gt


  let transform_gd (gd : Def.t) : Def.t = { gd with body = transform_gt gd.body }

  let transform (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "apply_array_max_length");
    List.map_snd transform_gd ctx
end
