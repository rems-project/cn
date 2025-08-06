module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

module Make (AD : GenTerms.Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  type stmt =
    | Asgn of (IT.t * Sctypes.t) * IT.t
    | LetStar of (Sym.t * Term.t)
    | Assert of LC.t

  and annot =
    | Stmt of stmt * (Locations.t[@equal fun _ _ -> true] [@compare fun _ _ -> 0])
  [@@deriving eq, ord]

  type t = annot

  let hash = Hashtbl.hash

  let stmts_of_gt (gt : Term.t) : annot list * Term.t =
    let rec aux (gt : Term.t) : annot list * Term.t =
      let (Annot (gt_, (), _, loc)) = gt in
      match gt_ with
      | `Arbitrary _ | `Pick _ | `Call _ | `Return _ | `ITE _ | `Map _ -> ([], gt)
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        let stmts, gt_last = aux gt_rest in
        (Stmt (Asgn ((it_addr, sct), it_val), loc) :: stmts, gt_last)
      | `LetStar ((x, gt'), gt_rest) ->
        let stmts, gt_last = aux gt_rest in
        (Stmt (LetStar (x, gt'), loc) :: stmts, gt_last)
      | `Assert (lc, gt_rest) ->
        let stmts, gt_last = aux gt_rest in
        (Stmt (Assert lc, loc) :: stmts, gt_last)
    in
    aux gt


  let gt_of_stmts (stmts : annot list) (gt_end : Term.t) : Term.t =
    List.fold_right
      (fun (stmt : annot) gt_rest ->
         let (Stmt (stmt_, loc)) = stmt in
         match stmt_ with
         | Asgn ((it_addr, sct), it_val) ->
           Term.asgn_ ((it_addr, sct), it_val, gt_rest) () loc
         | LetStar (x, gt') -> Term.let_star_ ((x, gt'), gt_rest) () loc
         | Assert lc -> Term.assert_ (lc, gt_rest) () loc)
      stmts
      gt_end


  let pp_ (stmt : stmt) : Pp.document =
    let open Pp in
    match stmt with
    | Asgn ((it_addr, ty), it_val) ->
      Sctypes.pp ty ^^^ IT.pp it_addr ^^^ !^":=" ^^^ IT.pp it_val
    | LetStar (x, gt) ->
      !^"let*"
      ^^^ Sym.pp x
      ^^^ colon
      ^^^ BT.pp (Term.basetype gt)
      ^^^ equals
      ^^ nest 2 (break 1 ^^ Term.pp gt)
    | Assert lc -> !^"assert" ^^ parens (nest 2 (break 1 ^^ LC.pp lc) ^^ break 1)


  let pp (Stmt (stmt_, _)) : Pp.document = pp_ stmt_
end
