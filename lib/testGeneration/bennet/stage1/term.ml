module BT = BaseTypes
module T = Terms.Normal
module LC = LogicalConstraints
module CF = Cerb_frontend

module Make (AD : Domain.T) = struct
  open struct
    module Inner = struct
      module AD = AD

      type tag_t = unit

      type 'ast annot = (unit, 'ast) GenTerms.annot [@@deriving eq, ord]

      type 'recur ast =
        [ `Arbitrary (** Generate arbitrary values *)
        | `Symbolic (** Generate symbolic values *)
        | `Call of Sym.t * T.t list
          (** `Call a defined generator according to a [Sym.t] with arguments [T.t list] *)
        | `Asgn of (T.t * Sctypes.t) * T.t * 'recur annot
          (** Claim ownership and assign a value to a memory location *)
        | `LetStar of (Sym.t * 'recur annot) * 'recur annot (** Backtrack point *)
        | `Return of T.t (** Monadic return *)
        | `Assert of LC.t * 'recur annot
          (** `Assert some [LC.t] are true, backtracking otherwise *)
        | `ITE of T.t * 'recur annot * 'recur annot (** If-then-else *)
        | `Map of (Sym.t * BT.t * T.t) * 'recur annot
        ]
      [@@deriving eq, ord]

      type t_ = t_ ast [@@deriving eq, ord]

      type t = t_ annot [@@deriving eq, ord]

      let basetype (GenTerms.Annot (_, _, bt, _) : t) : BT.t = bt

      (* Include defaults for all unsupported smart constructors *)
      include GenTerms.Defaults (struct
          let name = "Stage 1"
        end)

      let arbitrary_ (tag : tag_t) (bt : BT.t) (loc : Locations.t) : t =
        Annot (`Arbitrary, tag, bt, loc)


      let symbolic_ (tag : tag_t) (bt : BT.t) (loc : Locations.t) : t =
        Annot (`Symbolic, tag, bt, loc)


      let call_ ((fsym, its) : Sym.t * T.t list) (tag : tag_t) (bt : BT.t) loc : t =
        Annot (`Call (fsym, its), tag, bt, loc)


      let asgn_
            (((it_addr, ct), it_val, gt') : (T.t * Sctypes.t) * T.t * t)
            (tag : tag_t)
            (loc : Locations.t)
        : t
        =
        Annot (`Asgn ((it_addr, ct), it_val, gt'), tag, basetype gt', loc)


      let let_star_ (((x, gt1), gt2) : (Sym.t * t) * t) (tag : tag_t) (loc : Locations.t)
        : t
        =
        Annot (`LetStar ((x, gt1), gt2), tag, basetype gt2, loc)


      let return_ (it : T.t) (tag : tag_t) (loc : Locations.t) : t =
        Annot (`Return it, tag, T.get_bt it, loc)


      let assert_ ((lc, gt') : LC.t * t) (tag : tag_t) (loc : Locations.t) : t =
        Annot (`Assert (lc, gt'), tag, basetype gt', loc)


      let ite_ ((it_if, gt_then, gt_else) : T.t * t * t) (tag : tag_t) loc : t =
        let bt = basetype gt_then in
        assert (BT.equal bt (basetype gt_else));
        Annot (`ITE (it_if, gt_then, gt_else), tag, bt, loc)


      let map_
            (((i, i_bt, it_perm), gt_inner) : (Sym.t * BT.t * T.t) * t)
            (tag : tag_t)
            loc
        : t
        =
        Annot
          ( `Map ((i, i_bt, it_perm), gt_inner),
            tag,
            BT.make_map_bt i_bt (basetype gt_inner),
            loc )

      (* All unsupported constructors now provided by included Defaults module *)
    end
  end

  include GenTerms.Make (Inner)
end
