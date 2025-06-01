module BT = BaseTypes
module IT = IndexTerms

let find_constraint (vars : Sym.Set.t) (x : Sym.t) (gt : Term.t) : (Term.t * IT.t) option =
  let rec aux (gt : Term.t) : (Term.t * IT.t) option =
    let open Option in
    let (GT (gt_, _, loc)) = gt in
    match gt_ with
    | Uniform | Alloc | Pick _ | Call _ | Return _ | ITE _ | Map _ -> None
    | Asgn ((it_addr, sct), it_val, gt_rest) ->
      let@ gt_rest, it = aux gt_rest in
      return (Term.asgn_ ((it_addr, sct), it_val, gt_rest) loc, it)
    | LetStar _ ->
      (* We assume reordering has been applied,
         so `assert`s appear before the next `let*` *)
      None
    | Assert (T (IT (Binop (EQ, IT (Sym x', _, _), it), _, _)), gt_rest)
      when Sym.equal x x' && Sym.Set.subset (IT.free_vars it) vars ->
      return (gt_rest, it)
    | Assert (T (IT (Binop (EQ, it, IT (Sym x', _, _)), _, _)), gt_rest)
      when Sym.equal x x' && Sym.Set.subset (IT.free_vars it) vars ->
      return (gt_rest, it)
    | _ -> None
  in
  aux gt


let transform_gt (vars : Sym.Set.t) (gt : Term.t) : Term.t =
  let aux (vars : Sym.Set.t) (gt : Term.t) : Term.t =
    let (GT (gt_, _, _)) = gt in
    match gt_ with
    | LetStar ((x, (GT (Uniform, _, loc) as gt)), gt_rest)
    | LetStar ((x, (GT (Alloc, _, loc) as gt)), gt_rest) ->
      let gt_rest, gt_res =
        match find_constraint vars x gt_rest with
        | Some (gt_rest, it) -> (gt_rest, Term.return_ it loc)
        | None -> (gt_rest, gt)
      in
      Term.let_star_ ((x, gt_res), gt_rest) loc
    | _ -> gt
  in
  Term.map_gen_pre (aux vars) gt


let transform_gd
      ({ filename : string;
         recursive : bool;
         spec;
         name : Sym.Set.elt;
         iargs : (Sym.Set.elt * BT.t) list;
         oargs : (Sym.Set.elt * BT.t) list;
         body : Term.t
       } :
        Def.t)
  : Def.t
  =
  let vars = iargs |> List.map fst |> Sym.Set.of_list in
  { filename; recursive; spec; name; iargs; oargs; body = transform_gt vars body }


let transform (ctx : Ctx.t) : Ctx.t = List.map_snd transform_gd ctx
