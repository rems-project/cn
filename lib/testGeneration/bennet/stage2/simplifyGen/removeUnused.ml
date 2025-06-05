module IT = IndexTerms

let rec is_pure (gt : Term.t) : bool =
  let (GT (gt_, _, _)) = gt in
  match gt_ with
  | Uniform -> true
  | Pick wgts -> wgts |> List.map snd |> List.for_all is_pure
  | Alloc -> false
  | Call _ -> false (* Could be less conservative... *)
  | Asgn _ -> false
  | LetStar ((_, gt1), gt2) -> is_pure gt1 && is_pure gt2
  | Return _ -> true
  | Assert _ -> false
  | ITE (_, gt_then, gt_else) -> is_pure gt_then && is_pure gt_else
  | Map _ -> false


let transform_gt (gt : Term.t) : Term.t =
  let aux (gt : Term.t) : Term.t =
    let (GT (gt_, _, _)) = gt in
    match gt_ with
    | LetStar ((x, gt_inner), gt_rest)
      when is_pure gt_inner && not (Sym.Set.mem x (Term.free_vars gt_rest)) ->
      gt_rest
    | Assert (T it, gt_rest) when IT.is_true it -> gt_rest
    | _ -> gt
  in
  Term.map_gen_post aux gt
