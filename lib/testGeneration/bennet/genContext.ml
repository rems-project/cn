module Make (GT : GenTerms.T) = struct
  module GD = GenDefinitions.Make (GT)

  type t = (Sym.t * GD.t) list [@@deriving eq, ord]

  let empty = []

  let pp (ctx : t) : Pp.document =
    Sym.executable_spec_enabled := false;
    Sym.print_nums := true;
    let ret =
      let open Pp in
      ctx
      |> List.map snd
      |> surround_separate_map 2 1 empty lbracket (semi ^^ twice hardline) rbracket GD.pp
    in
    Sym.executable_spec_enabled := true;
    Sym.print_nums := false;
    ret
end

module MakeOptional (GT : GenTerms.T) = struct
  module GD = GenDefinitions.MakeOptional (GT)

  type t = (Sym.t * GD.t) list [@@deriving eq, ord]

  let empty = []

  let pp (ctx : t) : Pp.document =
    let open Pp in
    ctx
    |> List.map snd
    |> surround_separate_map 2 1 empty lbracket (semi ^^ twice hardline) rbracket GD.pp


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
      (fun (_, GD.{ filename; recursive; spec; name; iargs; oargs; body }) ->
         match body with
         | Some body ->
           let module GD' = GenDefinitions.Make (GT) in
           Some (name, GD'.{ filename; recursive; spec; name; iargs; oargs; body })
         | None -> None)
      ctx
end
