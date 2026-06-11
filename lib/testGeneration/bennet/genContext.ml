module Make (GT : GenTerms.T) = struct
  module GD = GenDefinitions.Make (GT)

  type t = (Sym.t * GD.t) list [@@deriving eq, ord]

  let empty = []

  let find (sym : Sym.t) (ctx : t) = List.assoc Sym.equal sym ctx

  let find_opt (sym : Sym.t) (ctx : t) = List.assoc_opt Sym.equal sym ctx

  let fold f acc ctx = List.fold_left (fun acc (sym, gd) -> f acc sym gd) acc ctx

  let pp (ctx : t) : Pp.document =
    Sym.executable_spec_enabled := false;
    Sym.print_nums := true;
    let ret =
      let open Pp in
      ctx |> List.map snd |> separate_map (semi ^^ twice hardline) GD.pp
    in
    Sym.executable_spec_enabled := true;
    Sym.print_nums := false;
    ret


  open struct
    let get_calls (gd : GD.t) : Sym.Set.t =
      let rec aux (gt : GT.t) : Sym.Set.t =
        let (Annot (gt_, _, _, _)) = gt in
        match gt_ with
        | `Arbitrary | `Symbolic | `ArbitrarySpecialized _ | `ArbitraryDomain _
        | `Return _ ->
          Sym.Set.empty
        | `Pick gts -> gts |> List.map aux |> List.fold_left Sym.Set.union Sym.Set.empty
        | `PickSized wgts | `PickSizedElab (_, wgts) ->
          wgts
          |> List.map snd
          |> List.map aux
          |> List.fold_left Sym.Set.union Sym.Set.empty
        | `Call (fsym, _) | `CallSized (fsym, _, _) -> Sym.Set.singleton fsym
        | `Asgn (_, _, gt')
        | `AsgnElab (_, _, _, gt')
        | `Assert (_, gt')
        | `AssertDomain (_, gt')
        | `Map (_, gt')
        | `MapElab (_, gt')
        | `SplitSize (_, gt')
        | `SplitSizeElab (_, _, gt') ->
          aux gt'
        | `LetStar ((_, gt1), gt2) | `ITE (_, gt1, gt2) ->
          Sym.Set.union (aux gt1) (aux gt2)
      in
      aux gd.body
  end

  let get_call_graph (ctx : t) : Sym.Digraph.t =
    ctx
    |> List.map_snd get_calls
    |> (ctx
        |> List.map fst
        |> List.fold_left Sym.Digraph.add_vertex Sym.Digraph.empty
        |> List.fold_left (fun cg (fsym, calls) ->
          Sym.Set.fold (fun fsym' cg' -> Sym.Digraph.add_edge cg' fsym fsym') calls cg))
end

module MakeOptional (GT : GenTerms.T) = struct
  module GD = GenDefinitions.MakeOptional (GT)

  type t = (Sym.t * GD.t) list [@@deriving eq, ord]

  let empty = []

  let find (sym : Sym.t) (ctx : t) : GD.t = List.assoc Sym.equal sym ctx

  let find_opt (sym : Sym.t) (ctx : t) : GD.t option = List.assoc_opt Sym.equal sym ctx

  let fold f acc ctx = List.fold_left (fun acc (sym, gd) -> f acc sym gd) acc ctx

  let pp (ctx : t) : Pp.document =
    let open Pp in
    ctx |> List.map snd |> separate_map (semi ^^ twice hardline) GD.pp


  let add (gd : GD.t) (ctx : t) : t =
    match (List.assoc_opt Sym.equal gd.name ctx, gd.body) with
    | Some { body = Some _; _ }, Some _ ->
      failwith ("Tried to add generator twice (`" ^ Sym.pp_string gd.name ^ "`)")
    | Some { body = None; recursive; _ }, Some _ ->
      (gd.name, { gd with recursive = recursive || gd.recursive })
      :: List.filter (fun (name, _) -> not (Sym.equal name gd.name)) ctx
    | Some ({ body = _; recursive; _ } as gd'), None ->
      (gd.name, { gd' with recursive = recursive || gd.recursive })
      :: List.filter (fun (name, _) -> not (Sym.equal name gd.name)) ctx
    | None, _ -> (gd.name, gd) :: ctx


  let drop_nones (ctx : t) : Make(GT).t =
    List.filter_map
      (fun (_, GD.{ filename; recursive; spec; name; iargs; oarg; c_types; body }) ->
         match body with
         | Some body ->
           let module GD' = GenDefinitions.Make (GT) in
           Some (name, GD'.{ filename; recursive; spec; name; iargs; oarg; c_types; body })
         | None -> None)
      ctx


  let get_call_graph (ctx : t) : Sym.Digraph.t =
    let module GC' = Make (GT) in
    ctx |> drop_nones |> GC'.get_call_graph
end
