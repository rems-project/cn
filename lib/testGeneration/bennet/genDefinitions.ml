module BT = BaseTypes

module Make (GT : GenTerms.T) = struct
  type t =
    { filename : string;
      recursive : bool;
      spec : bool;
      name : Sym.t;
      iargs : (Sym.t * BT.t) list;
      oargs : (Sym.t * BT.t) list;
      body : GT.t
    }
  [@@deriving eq, ord]

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
       ^^ space
       ^^ lbrace
       ^^ nest 2 (break 1 ^^ GT.pp gd.body)
       ^/^ rbrace)
end

module MakeOptional (GT : GenTerms.T) = struct
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
