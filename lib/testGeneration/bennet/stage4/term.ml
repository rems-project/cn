module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module StringMap = Map.Make (String)

module Make (AD : Domain.T) = struct
  open struct
    module Inner = struct
      module AD = AD

      type tag_t = unit

      type 'ast annot = (unit, 'ast) GenTerms.annot [@@deriving eq, ord]

      type 'recur ast =
        [ `Arbitrary (** Generate arbitrary values *)
        | `Symbolic (** Generate symbolic values *)
        | `ArbitrarySpecialized of
            (IT.t option * IT.t option) * (IT.t option * IT.t option)
          (** Generate arbitrary values: ((min_inc, min_ex), (max_inc, max_ex)) *)
        | `ArbitraryDomain of AD.Relative.t (** Generate arbitrary values from domain *)
        | `PickSized of (Z.t * 'recur annot) list
          (** Pick among a list of options, weighted by the provided [Z.t]s *)
        | `Call of Sym.t * IT.t list
          (** Call a defined generator according to a [Sym.t] with arguments [IT.t list] *)
        | `CallSized of Sym.t * IT.t list * (int * Sym.t)
          (** Call a defined generator according to a [Sym.t] with arguments [IT.t list] *)
        | `Asgn of (IT.t * Sctypes.t) * IT.t * 'recur annot
          (** Claim ownership and assign a value to a memory location *)
        | `LetStar of (Sym.t * 'recur annot) * 'recur annot (** Backtrack point *)
        | `Return of IT.t (** Monadic return *)
        | `Assert of LC.t * 'recur annot
          (** Assert some [LC.t] are true, backtracking otherwise *)
        | `AssertDomain of AD.t * 'recur annot (** Assert domain constraints *)
        | `ITE of IT.t * 'recur annot * 'recur annot (** If-then-else *)
        | `Map of (Sym.t * BT.t * IT.t) * 'recur annot
        | `SplitSize of Sym.Set.t * 'recur annot
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

      let arbitrary_specialized_
            (((min_inc, min_ex), (max_inc, max_ex)) :
              (IT.t option * IT.t option) * (IT.t option * IT.t option))
            (tag : tag_t)
            (bt : BT.t)
            (loc : Locations.t)
        : t
        =
        Annot (`ArbitrarySpecialized ((min_inc, min_ex), (max_inc, max_ex)), tag, bt, loc)


      let arbitrary_domain_
            (d : AD.Relative.t)
            (tag : tag_t)
            (bt : BT.t)
            (loc : Locations.t)
        : t
        =
        Annot (`ArbitraryDomain d, tag, bt, loc)


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


      let pick_sized_ (wgts : (Z.t * t) list) (tag : tag_t) bt (loc : Locations.t) : t =
        let bt =
          List.fold_left
            (fun bt (_, gt) ->
               assert (BT.equal bt (basetype gt));
               bt)
            bt
            wgts
        in
        Annot (`PickSized wgts, tag, bt, loc)


      let split_size_ ((syms, gt') : Sym.Set.t * t) (tag : tag_t) (loc : Locations.t) : t =
        Annot (`SplitSize (syms, gt'), tag, basetype gt', loc)


      let call_sized_
            ((fsym, its, sz) : Sym.t * IT.t list * (int * Sym.t))
            (tag : tag_t)
            (bt : BT.t)
            (loc : Locations.t)
        : t
        =
        Annot (`CallSized (fsym, its, sz), tag, bt, loc)
    end
  end

  include GenTerms.Make (Inner)
end
