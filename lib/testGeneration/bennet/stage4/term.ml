module BT = BaseTypes
module IT = IndexTerms
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
        | `Lazy (** Lazily generate values *)
        | `ArbitrarySpecialized of
            (IT.t option * IT.t option) * (IT.t option * IT.t option)
          (** Generate arbitrary values: ((min_inc, min_ex), (max_inc, max_ex)) *)
        | `ArbitraryDomain of AD.Relative.t
        | `Call of Sym.t * IT.t list
          (** `Call a defined generator according to a [Sym.t] with arguments [IT.t list] *)
        | `Asgn of (IT.t * Sctypes.t) * IT.t * 'recur annot
          (** Claim ownership and assign a value to a memory location *)
        | `LetStar of (Sym.t * 'recur annot) * 'recur annot (** Backtrack point *)
        | `Return of IT.t (** Monadic return *)
        | `Assert of LC.t * 'recur annot
          (** `Assert some [LC.t] are true, backtracking otherwise *)
        | `AssertDomain of AD.t * 'recur annot
        | `ITE of IT.t * 'recur annot * 'recur annot (** If-then-else *)
        | `Map of (Sym.t * BT.t * IT.t) * 'recur annot
        | `Pick of 'recur annot list
        | `Instantiate of (Sym.t * 'recur annot) * 'recur annot
          (** Instantiate a lazily-evaluated value, then continue with rest *)
        ]
      [@@deriving eq, ord]

      type t_ = t_ ast [@@deriving eq, ord]

      type t = t_ annot [@@deriving eq, ord]

      let basetype (GenTerms.Annot (_, _, bt, _) : t) : BT.t = bt

      let arbitrary_ (tag : tag_t) (bt : BT.t) (loc : Locations.t) : t =
        Annot (`Arbitrary, tag, bt, loc)


      let symbolic_ (tag : tag_t) (bt : BT.t) (loc : Locations.t) : t =
        Annot (`Symbolic, tag, bt, loc)


      (* Include defaults for all unsupported smart constructors *)
      include GenTerms.Defaults (struct
          let name = "Stage 4"
        end)

      let lazy_ (tag : tag_t) (bt : BT.t) (loc : Locations.t) : t =
        Annot (`Lazy, tag, bt, loc)


      let arbitrary_domain_
            (d : AD.Relative.t)
            (tag : tag_t)
            (bt : BT.t)
            (loc : Locations.t)
        : t
        =
        Annot (`ArbitraryDomain d, tag, bt, loc)


      let arbitrary_specialized_
            ((mins, maxs) : (IT.t option * IT.t option) * (IT.t option * IT.t option))
            (tag : tag_t)
            (bt : BT.t)
            (loc : Locations.t)
        : t
        =
        Annot (`ArbitrarySpecialized (mins, maxs), tag, bt, loc)


      let call_ ((fsym, its) : Sym.t * IT.t list) (tag : tag_t) (bt : BT.t) loc : t =
        Annot (`Call (fsym, its), tag, bt, loc)


      let asgn_
            (((it_addr, ct), it_val, gt') : (IT.t * Sctypes.t) * IT.t * t)
            (tag : tag_t)
            (loc : Locations.t)
        : t
        =
        Annot (`Asgn ((it_addr, ct), it_val, gt'), tag, basetype gt', loc)


      let let_star_ (((x, gt1), gt2) : (Sym.t * t) * t) (tag : tag_t) (loc : Locations.t)
        : t
        =
        Annot (`LetStar ((x, gt1), gt2), tag, basetype gt2, loc)


      let return_ (it : IT.t) (tag : tag_t) (loc : Locations.t) : t =
        Annot (`Return it, tag, IT.get_bt it, loc)


      let assert_ ((lc, gt') : LC.t * t) (tag : tag_t) (loc : Locations.t) : t =
        Annot (`Assert (lc, gt'), tag, basetype gt', loc)


      let assert_domain_ ((ad, gt') : AD.t * t) (tag : tag_t) (loc : Locations.t) : t =
        Annot (`AssertDomain (ad, gt'), tag, basetype gt', loc)


      let ite_ ((it_if, gt_then, gt_else) : IT.t * t * t) (tag : tag_t) loc : t =
        let bt = basetype gt_then in
        assert (BT.equal bt (basetype gt_else));
        Annot (`ITE (it_if, gt_then, gt_else), tag, bt, loc)


      let map_
            (((i, i_bt, it_perm), gt_inner) : (Sym.t * BT.t * IT.t) * t)
            (tag : tag_t)
            loc
        : t
        =
        Annot
          ( `Map ((i, i_bt, it_perm), gt_inner),
            tag,
            BT.make_map_bt i_bt (basetype gt_inner),
            loc )


      let pick_ (gts : t list) (tag : tag_t) bt (loc : Locations.t) : t =
        let bt =
          List.fold_left
            (fun bt gt ->
               assert (BT.equal bt (basetype gt));
               bt)
            bt
            gts
        in
        Annot (`Pick gts, tag, bt, loc)


      let instantiate_
            (((x, gt_inner), gt_rest) : (Sym.t * t) * t)
            (tag : tag_t)
            (loc : Locations.t)
        : t
        =
        Annot (`Instantiate ((x, gt_inner), gt_rest), tag, basetype gt_rest, loc)
    end
  end

  include GenTerms.Make (Inner)
end
