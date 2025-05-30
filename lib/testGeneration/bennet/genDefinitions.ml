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


  type context = (Sym.t * (Sym.t list * t) list) list [@@deriving eq, ord]

  let empty_context = []

  let pp_context (ctx : context) : Pp.document =
    let defns = ctx |> List.map snd |> List.flatten |> List.map snd in
    let open Pp in
    surround_separate_map 2 1 empty lbracket (semi ^^ twice hardline) rbracket pp defns


  let add_context
        ({ filename; recursive; spec; name; iargs; oargs; body } : t)
        (ctx : context)
    : context
    =
    let desired_iargs = List.map fst iargs in
    match List.assoc_opt Sym.equal name ctx with
    | Some iargs_defs ->
      let others = List.filter (fun (fsym, _) -> not (Sym.equal name fsym)) ctx in
      (match List.assoc_opt (List.equal Sym.equal) desired_iargs iargs_defs with
       | Some { body = None; _ } when Option.is_some body ->
         let others2 =
           List.filter
             (fun (xs, _) -> not (List.equal Sym.equal desired_iargs xs))
             iargs_defs
         in
         ( name,
           (desired_iargs, { filename; recursive; spec; name; iargs; oargs; body })
           :: others2 )
         :: others
       | Some _ -> ctx
       | None ->
         ( name,
           (desired_iargs, { filename; recursive; spec; name; iargs; oargs; body })
           :: iargs_defs )
         :: others)
    | None ->
      (name, [ (desired_iargs, { filename; recursive; spec; name; iargs; oargs; body }) ])
      :: ctx
end
