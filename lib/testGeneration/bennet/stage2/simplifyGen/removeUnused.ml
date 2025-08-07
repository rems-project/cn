module IT = IndexTerms

module Make (AD : Domain.T) = struct
  module Term = Term.Make (AD)

  let transform_gt (gt : Term.t) : Term.t =
    Cerb_debug.print_debug 2 [] (fun () -> "remove_unused");
    let aux (gt : Term.t) : Term.t =
      let (Annot (gt_, (), _, _)) = gt in
      match gt_ with
      | `LetStar ((x, gt_inner), gt_rest)
        when (not (Term.contains_constraint gt_inner))
             && not (Sym.Set.mem x (Term.free_vars gt_rest)) ->
        gt_rest
      | `Assert (T it, gt_rest) when IT.is_true it -> gt_rest
      | _ -> gt
    in
    Term.map_gen_post aux gt
end
