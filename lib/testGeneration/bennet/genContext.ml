module Make (GT : GenDefinitions.GEN_TERM) = struct
  module GD = GenDefinitions.Make (GT)

  type t = (Sym.t * GD.t) list [@@deriving eq, ord]

  let empty_context = []

  let pp (ctx : t) : Pp.document =
    let open Pp in
    ctx
    |> List.map snd
    |> surround_separate_map 2 1 empty lbracket (semi ^^ twice hardline) rbracket GD.pp


  let add_context (gd : GD.t) (ctx : t) : t =
    match (List.assoc_opt Sym.equal gd.name ctx, gd.body) with
    | Some { body = Some _; _ }, Some _ ->
      failwith ("Tried to add generator twice (`" ^ Sym.pp_string gd.name ^ "`)")
    | Some { body = None; _ }, Some _ ->
      (gd.name, gd) :: List.filter (fun (name, _) -> not (Sym.equal name gd.name)) ctx
    | Some { body = _; _ }, None -> ctx
    | None, _ -> (gd.name, gd) :: ctx
end
