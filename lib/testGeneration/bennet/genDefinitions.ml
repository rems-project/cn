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
end
