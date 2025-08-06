module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module CF = Cerb_frontend

module Make (AD : GenTerms.Domain.T) = struct
  include GenTerms.Make (AD)
  module AD = AD

  type tag_t = unit

  type 'ast annot = (unit, 'ast) GenTerms.annot [@@deriving eq, ord]

  type 'recur ast =
    [ `Arbitrary of AD.t (** Generate arbitrary values *)
    | `Call of Sym.t * IT.t list
      (** `Call a defined generator according to a [Sym.t] with arguments [IT.t list] *)
    | `Asgn of (IT.t * Sctypes.t) * IT.t * 'recur annot
      (** Claim ownership and assign a value to a memory location *)
    | `LetStar of (Sym.t * 'recur annot) * 'recur annot (** Backtrack point *)
    | `Return of IT.t (** Monadic return *)
    | `Assert of LC.t * 'recur annot
      (** `Assert some [LC.t] are true, backtracking otherwise *)
    | `ITE of IT.t * 'recur annot * 'recur annot (** If-then-else *)
    | `Map of (Sym.t * BT.t * IT.t) * 'recur annot
    | `Pick of 'recur annot list
    ]
  [@@deriving eq, ord]

  type t_ = t_ ast [@@deriving eq, ord]

  type t = t_ annot [@@deriving eq, ord]

  let rec subst_ (su : [ `Term of IT.t | `Rename of Sym.t ] Subst.t) (gt_ : t_) : t_ =
    match gt_ with
    | `Arbitrary d -> `Arbitrary d
    | `Pick gts -> `Pick (List.map (subst su) gts)
    | `Call (fsym, iargs) -> `Call (fsym, List.map (IT.subst su) iargs)
    | `Asgn ((it_addr, bt), it_val, g') ->
      `Asgn ((IT.subst su it_addr, bt), IT.subst su it_val, subst su g')
    | `LetStar ((x, gt1), gt2) ->
      let x, gt2 = suitably_alpha_rename_gen su.relevant x gt2 in
      `LetStar ((x, subst su gt1), subst su gt2)
    | `Return it -> `Return (IT.subst su it)
    | `Assert (lc, gt') -> `Assert (LC.subst su lc, subst su gt')
    | `ITE (it, gt_then, gt_else) ->
      `ITE (IT.subst su it, subst su gt_then, subst su gt_else)
    | `Map ((i, bt, it_perm), gt') ->
      let i', it_perm = IT.suitably_alpha_rename su.relevant i it_perm in
      let gt' = subst (IT.make_rename ~from:i ~to_:i') gt' in
      `Map ((i', bt, IT.subst su it_perm), subst su gt')


  and subst (su : [ `Term of IT.t | `Rename of Sym.t ] Subst.t) (gt : t) : t =
    let (Annot (gt_, (), bt, here)) = gt in
    Annot (subst_ su gt_, (), bt, here)


  and alpha_rename_gen x gt =
    let x' = Sym.fresh_same x in
    (x', subst (IT.make_rename ~from:x ~to_:x') gt)


  and suitably_alpha_rename_gen syms x gt =
    if Sym.Set.mem x syms then
      alpha_rename_gen x gt
    else
      (x, gt)


  let rec map_gen_pre (f : t -> t) (g : t) : t =
    let (Annot (gt_, (), bt, here)) = f g in
    let gt_ =
      match gt_ with
      | `Arbitrary d -> `Arbitrary d
      | `Pick gts -> `Pick (List.map (map_gen_pre f) gts)
      | `Call (fsym, its) -> `Call (fsym, its)
      | `Asgn ((it_addr, sct), it_val, gt') ->
        `Asgn ((it_addr, sct), it_val, map_gen_pre f gt')
      | `LetStar ((x, gt), gt') -> `LetStar ((x, map_gen_pre f gt), map_gen_pre f gt')
      | `Return it -> `Return it
      | `Assert (lcs, gt') -> `Assert (lcs, map_gen_pre f gt')
      | `ITE (it, gt_then, gt_else) ->
        `ITE (it, map_gen_pre f gt_then, map_gen_pre f gt_else)
      | `Map ((i, bt, it_perm), gt') -> `Map ((i, bt, it_perm), map_gen_pre f gt')
    in
    Annot (gt_, (), bt, here)


  let rec map_gen_post (f : t -> t) (g : t) : t =
    let (Annot (gt_, (), bt, here)) = g in
    let gt_ =
      match gt_ with
      | `Arbitrary d -> `Arbitrary d
      | `Pick gts -> `Pick (List.map (map_gen_post f) gts)
      | `Call (fsym, its) -> `Call (fsym, its)
      | `Asgn ((it_addr, sct), it_val, gt') ->
        `Asgn ((it_addr, sct), it_val, map_gen_post f gt')
      | `LetStar ((x, gt), gt') -> `LetStar ((x, map_gen_post f gt), map_gen_post f gt')
      | `Return it -> `Return it
      | `Assert (lcs, gt') -> `Assert (lcs, map_gen_post f gt')
      | `ITE (it, gt_then, gt_else) ->
        `ITE (it, map_gen_post f gt_then, map_gen_post f gt_else)
      | `Map ((i, bt, it_perm), gt') -> `Map ((i, bt, it_perm), map_gen_post f gt')
    in
    f (Annot (gt_, (), bt, here))
end
