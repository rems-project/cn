module BT = BaseTypes
module GT = GenTerms

module type GEN_TERM = sig
  type t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val pp : t -> Pp.document
end

module Make (GT : GEN_TERM) = struct
  type t =
    { filename : string;
      recursive : bool;
      spec : bool;
      name : Sym.t;
      iargs : (Sym.t * BT.t) list;
      oargs : (Sym.t * BT.t) list;
      body : GT.t option
    }
  [@@deriving eq, ord]

  let mangled_name
        { filename = _; recursive = _; spec = _; name; iargs; oargs = _; body = _ }
    : Sym.t
    =
    GenUtils.get_mangled_name (name :: List.map fst iargs)


  let pp (gd : t) : Pp.document =
    let open Pp in
    group
      (!^"generator"
       ^^^ braces
             (separate_map
                (comma ^^ space)
                (fun (x, ty) -> BT.pp ty ^^^ Sym.pp x)
                gd.oargs)
       ^^^ Sym.pp gd.name
       ^^ parens
            (separate_map
               (comma ^^ space)
               (fun (x, ty) -> BT.pp ty ^^^ Sym.pp x)
               gd.iargs)
       ^^
       match gd.body with
       | Some body -> space ^^ lbrace ^^ nest 2 (break 1 ^^ GT.pp body) ^/^ rbrace
       | None -> semi)


  type context = (Sym.t * t) list [@@deriving eq, ord]

  let empty_context = []

  let pp_context (ctx : context) : Pp.document =
    let open Pp in
    ctx
    |> List.map snd
    |> surround_separate_map 2 1 empty lbracket (semi ^^ twice hardline) rbracket pp


  let add_context (gd : t) (ctx : context) : context =
    match (List.assoc_opt Sym.equal gd.name ctx, gd.body) with
    | Some { body = Some _; _ }, Some _ ->
      failwith ("Tried to add generator twice (`" ^ Sym.pp_string gd.name ^ "`)")
    | Some { body = None; _ }, Some _ ->
      (gd.name, gd) :: List.filter (fun (name, _) -> not (Sym.equal name gd.name)) ctx
    | Some { body = _; _ }, None -> ctx
    | None, _ -> (gd.name, gd) :: ctx
end
