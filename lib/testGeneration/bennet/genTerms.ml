module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

type ('tag, 'ast) annot =
  | Annot of
      ('ast * 'tag * BT.t * (Locations.t[@equal fun _ _ -> true] [@compare fun _ _ -> 0]))
[@@deriving eq, ord]

module [@warning "-60"] Make (AD : sig end) = struct
  module Inner = struct
    type ('tag, 'recur) ast =
      [ `Arbitrary (** Generate arbitrary values *)
      | `Call of Sym.t * IT.t list
        (** Call a defined generator according to a [Sym.t] with arguments [IT.t list] *)
      | `Asgn of (IT.t * Sctypes.t) * IT.t * ('tag, 'recur) annot
        (** Claim ownership and assign a value to a memory location *)
      | `LetStar of (Sym.t * ('tag, 'recur) annot) * ('tag, 'recur) annot
        (** Backtrack point *)
      | `Return of IT.t (** Monadic return *)
      | `Assert of LC.t * ('tag, 'recur) annot
        (** Assert some [LC.t] are true, backtracking otherwise *)
      | `ITE of IT.t * ('tag, 'recur) annot * ('tag, 'recur) annot (** If-then-else *)
      | `Map of (Sym.t * BT.t * IT.t) * ('tag, 'recur) annot
      | `Pick of ('tag, 'recur) annot list (** Pick among a list of options *)
      | `CallSized of Sym.t * IT.t list * (int * Sym.t)
      | `PickSized of (Z.t * ('tag, 'recur) annot) list
        (** Pick among a list of options, weighted by the provided [Z.t]s *)
      | `SplitSize of Sym.Set.t * ('tag, 'recur) annot
      | `AssertDomain of Sym.t * BT.t * Abstract.domain * ('tag, 'recur) annot
        (** Domain assertion *)
      | `AsgnElab of
          Sym.t * (((Sym.t * BT.t) * IT.t) * Sctypes.t) * IT.t * ('tag, 'recur) annot
      | `MapElab of (Sym.t * BT.t * (IT.t * IT.t) * IT.t) * ('tag, 'recur) annot
      | `PickSizedElab of Sym.t * (Z.t * ('tag, 'recur) annot) list
      | `SplitSizeElab of Sym.t * Sym.Set.t * ('tag, 'recur) annot
      ]
    [@@deriving eq, ord]
  end

  open Inner

  let basetype (Annot (_, _, bt, _) : ('tag, 'ast) annot) : BT.t = bt

  let loc (Annot (_, _, _, loc) : ('tag, 'ast) annot) : Locations.t = loc

  (* Smart constructors *)
  let arbitrary_ (tag : 'tag) (bt : BT.t) (loc : Locations.t) : ('tag, _) annot =
    Annot (`Arbitrary, tag, bt, loc)


  let call_ (fsym, xits) (tag : 'tag) (bt : BT.t) loc : ('tag, _) annot =
    Annot (`Call (fsym, xits), tag, bt, loc)


  let asgn_ ((it_addr, ct), it_val, gt') (tag : 'tag) loc =
    Annot (`Asgn ((it_addr, ct), it_val, gt'), tag, basetype gt', loc)


  let let_star_
        (((x, gt1), gt2) : (Sym.t * ('tag, 'ast) annot) * ('tag, 'ast) annot)
        (tag : 'tag)
        (loc : Locations.t)
    : ('tag, 'ast) annot
    =
    Annot (`LetStar ((x, gt1), gt2), tag, basetype gt2, loc)


  let return_ (it : IT.t) (tag : 'tag) (loc : Locations.t) : ('tag, _) annot =
    Annot (`Return it, tag, IT.get_bt it, loc)


  let assert_ ((lc, gt') : LC.t * ('tag, 'ast) annot) (tag : 'tag) (loc : Locations.t)
    : ('tag, 'ast) annot
    =
    Annot (`Assert (lc, gt'), tag, basetype gt', loc)


  let ite_
        ((it_if, gt_then, gt_else) : IT.t * ('tag, 'ast) annot * ('tag, 'ast) annot)
        (tag : 'tag)
        loc
    : ('tag, _) annot
    =
    let bt = basetype gt_then in
    assert (BT.equal bt (basetype gt_else));
    Annot (`ITE (it_if, gt_then, gt_else), tag, bt, loc)


  let map_
        (((i, i_bt, it_perm), gt_inner) : (Sym.t * BT.t * IT.t) * ('tag, 'ast) annot)
        (tag : 'tag)
        loc
    : ('tag, 'ast) annot
    =
    Annot
      ( `Map ((i, i_bt, it_perm), gt_inner),
        tag,
        BT.make_map_bt i_bt (basetype gt_inner),
        loc )


  let pick_ (gts : ('tag, 'ast) annot list) (tag : 'tag) (bt : BT.t) (loc : Locations.t)
    : ('tag, 'ast) annot
    =
    match gts with
    | gt :: gts' ->
      let bt =
        List.fold_left
          (fun bt gt ->
             assert (BT.equal bt (basetype gt));
             bt)
          (basetype gt)
          gts'
      in
      Annot (`Pick gts, tag, bt, loc)
    | [] ->
      assert_ (LC.T (IT.bool_ false loc), return_ (IT.default_ bt loc) tag loc) tag loc


  (* Constructor-checking functions *)

  let is_arbitrary_ (gt_ : [< ('tag, 'recur) ast ] as 'recur) : unit option =
    match gt_ with `Arbitrary -> Some () | _ -> None


  let is_arbitrary (gt : ('tag, [< `Arbitrary ]) annot) : unit option =
    let (Annot (gt_, _, _, _)) = gt in
    is_arbitrary_ gt_


  let is_call_ (gt_ : [< ('tag, 'recur) ast ] as 'recur) : (Sym.t * IT.t list) option =
    match gt_ with `Call x -> Some x | _ -> None


  let is_call (gt : ('tag, ([< ('tag, 'recur) ast ] as 'recur)) annot) =
    let (Annot (gt_, _, _, _)) = gt in
    is_call_ gt_


  let is_asgn_ (gt_ : [< ('tag, 'recur) ast ] as 'recur)
    : ((IT.t * Sctypes.ctype) * IT.t * ('tag, 'recur) annot) option
    =
    match gt_ with `Asgn x -> Some x | _ -> None


  let is_asgn (gt : ('tag, ([< ('tag, 'recur) ast ] as 'recur)) annot) =
    let (Annot (gt_, _, _, _)) = gt in
    is_asgn_ gt_


  let is_let_star_ (gt_ : [< ('tag, 'recur) ast ] as 'recur)
    : ((Sym.t * ('tag, 'recur) annot) * ('tag, 'recur) annot) option
    =
    match gt_ with `LetStar x -> Some x | _ -> None


  let is_let_star (gt : ('tag, ([< ('tag, 'recur) ast ] as 'recur)) annot) =
    let (Annot (gt_, _, _, _)) = gt in
    is_let_star_ gt_


  (* Other utility functions *)

  let rec pp (tm : ('tag, ([< ('tag, 'recur) ast ] as 'recur)) annot) : Pp.document =
    let open Pp in
    let (Annot (tm_, _, bt, _)) = tm in
    match tm_ with
    | `Arbitrary -> !^"arbitrary" ^^ angles (BT.pp bt) ^^ parens empty
    | `Call (fsym, iargs) ->
      Sym.pp fsym ^^ parens (nest 2 (separate_map (comma ^^ break 1) IT.pp iargs))
    | `Asgn ((it_addr, ty), it_val, gt') ->
      Sctypes.pp ty ^^^ IT.pp it_addr ^^^ !^":=" ^^^ IT.pp it_val ^^ semi ^/^ pp gt'
    | `LetStar ((x, gt1), gt2) ->
      !^"let*"
      ^^^ Sym.pp x
      ^^^ colon
      ^^^ BT.pp (basetype gt1)
      ^^^ equals
      ^^ nest 2 (break 1 ^^ pp gt1)
      ^^ semi
      ^/^ pp gt2
    | `Return it -> !^"return" ^^^ IT.pp it
    | `Assert (lc, gt') ->
      !^"assert" ^^ parens (nest 2 (break 1 ^^ LC.pp lc) ^^ break 1) ^^ semi ^/^ pp gt'
    | `ITE (it_if, gt_then, gt_else) ->
      !^"if"
      ^^^ parens (IT.pp it_if)
      ^^^ braces (nest 2 (break 1 ^^ pp gt_then) ^^ break 1)
      ^^^ !^"else"
      ^^^ braces (nest 2 (break 1 ^^ pp gt_else) ^^ break 1)
    | `Map ((i, i_bt, it_perm), gt') ->
      !^"map"
      ^^^ parens (BT.pp i_bt ^^^ Sym.pp i ^^ semi ^^^ IT.pp it_perm)
      ^^ braces (nest 2 (break 1 ^^ pp gt') ^^ break 1)
    | `Pick gts ->
      !^"pick"
      ^^ parens
           (brackets
              (separate_map
                 (semi ^^ break 1)
                 (fun gt -> braces (nest 2 (break 1 ^^ pp gt)))
                 gts))
    | `CallSized (fsym, iargs, (n, size_sym)) ->
      Sym.pp fsym
      ^^ brackets (int n ^^ comma ^^^ Sym.pp size_sym)
      ^^ parens (nest 2 (separate_map (comma ^^ break 1) IT.pp iargs))
    | `PickSized wgts ->
      !^"pick"
      ^^ parens
           (brackets
              (separate_map
                 (semi ^^ break 1)
                 (fun (w, gt) ->
                    parens (z w ^^ comma ^^ braces (nest 2 (break 1 ^^ pp gt))))
                 wgts))
    | `SplitSize (syms, gt_rest) ->
      !^"split_size"
      ^^ parens
           (separate_map (comma ^^ space) Sym.pp (syms |> Sym.Set.to_seq |> List.of_seq))
      ^^ semi
      ^/^ pp gt_rest
    | `AssertDomain (x, _x_bt, _domain, gt_rest) ->
      !^"assert_domain"
      ^^ brackets (Sym.pp x)
      ^^ parens (nest 2 (break 1 ^^ !^"TODO: `Abstract.pp_domain`") ^^ break 1)
      ^^ semi
      ^/^ pp gt_rest
    | `MapElab ((i, i_bt, (it_min, it_max), it_perm), gt_inner) ->
      !^"map"
      ^^^ parens
            (BT.pp i_bt
             ^^^ Sym.pp i
             ^^ semi
             ^^^ IT.pp it_perm
             ^^ c_comment
                  (IT.pp it_min ^^ !^" <= " ^^ Sym.pp i ^^ !^" <= " ^^ IT.pp it_max))
      ^^ braces (c_comment (BT.pp bt) ^^ nest 2 (break 1 ^^ pp gt_inner) ^^ break 1)
    | `PickSizedElab (choice_var, wgts) ->
      !^"pick"
      ^^ parens
           (c_comment (!^"chosen by " ^^ Sym.pp choice_var)
            ^/^ brackets
                  (separate_map
                     (semi ^^ break 1)
                     (fun (w, gt) ->
                        parens (z w ^^ comma ^^ braces (nest 2 (break 1 ^^ pp gt))))
                     wgts))
    | `SplitSizeElab (marker_var, syms, gt_rest) ->
      !^"split_size"
      ^^ brackets (Sym.pp marker_var)
      ^^ parens
           (separate_map (comma ^^ space) Sym.pp (syms |> Sym.Set.to_seq |> List.of_seq))
      ^^ semi
      ^/^ pp gt_rest
    | `AsgnElab (backtrack_var, (((p_sym, p_bt), it_addr), sct), it_val, gt_rest) ->
      Sctypes.pp sct
      ^^^ IT.pp it_addr
      ^^^ !^":="
      ^^^ IT.pp it_val
      ^^ semi
      ^^^ c_comment
            (!^"can be backtracked to as"
             ^^^ Sym.pp backtrack_var
             ^^ !^" allocs via "
             ^^ Sym.pp p_sym
             ^^^ colon
             ^^^ BT.pp p_bt)
      ^/^ pp gt_rest


  let rec free_vars_bts_ (gt_ : [< ('tag, 'recur) ast ] as 'recur) : BT.t Sym.Map.t =
    let loc = Locations.other __LOC__ in
    match gt_ with
    | `Arbitrary -> Sym.Map.empty
    | `Call (_, iargs) -> IT.free_vars_bts_list iargs
    | `Asgn ((it_addr, _), it_val, gt') ->
      free_vars_bts_list [ return_ it_addr () loc; return_ it_val () loc; gt' ]
    | `LetStar ((x, gt_inner), gt_rest) ->
      Sym.Map.union
        (fun _ bt1 bt2 ->
           assert (BT.equal bt1 bt2);
           Some bt1)
        (free_vars_bts gt_inner)
        (Sym.Map.remove x (free_vars_bts gt_rest))
    | `Return it -> IT.free_vars_bts it
    | `Assert (lc, gt') ->
      (Sym.Map.union (fun _ bt1 bt2 ->
         assert (BT.equal bt1 bt2);
         Some bt1))
        (free_vars_bts gt')
        (LC.free_vars_bts lc)
    | `ITE (it_if, gt_then, gt_else) ->
      free_vars_bts_list [ return_ it_if () loc; gt_then; gt_else ]
    | `Map ((i, _bt, it_perm), gt') ->
      Sym.Map.remove i (free_vars_bts_list [ return_ it_perm () loc; gt' ])
    | `Pick gts -> free_vars_bts_list gts


  and free_vars_bts (Annot (gt_, _, _, _) : ('tag, 'ast) annot) : BT.t Sym.Map.t =
    free_vars_bts_ gt_


  and free_vars_bts_list : ('tag, 'ast) annot list -> BT.t Sym.Map.t =
    fun xs ->
    List.fold_left
      (fun ss t ->
         Sym.Map.union
           (fun _ bt1 bt2 ->
              assert (BT.equal bt1 bt2);
              Some bt1)
           ss
           (free_vars_bts t))
      Sym.Map.empty
      xs


  let free_vars gt : Sym.Set.t =
    gt |> free_vars_bts |> Sym.Map.bindings |> List.map fst |> Sym.Set.of_list


  let rec contains_call (gt : ('tag, ([< ('tag, 'recur) ast ] as 'recur)) annot) : bool =
    let (Annot (gt_, (), _, _)) = gt in
    match gt_ with
    | `Arbitrary | `Return _ -> false
    | `Call _ | `CallSized _ -> true
    | `LetStar ((_, gt1), gt2) | `ITE (_, gt1, gt2) ->
      contains_call gt1 || contains_call gt2
    | `Asgn (_, _, gt')
    | `AsgnElab (_, _, gt')
    | `Assert (_, gt')
    | `AssertDomain (_, _, _, gt')
    | `Map (_, gt')
    | `MapElab (_, gt')
    | `SplitSize (_, gt')
    | `SplitSizeElab (_, _, gt') ->
      contains_call gt'
    | `Pick gts -> List.exists contains_call gts
    | `PickSized wgts | `PickSizedElab (_, wgts) ->
      List.exists (fun (_, g) -> contains_call g) wgts


  let rec contains_constraint (gt : ('tag, ([< ('tag, 'recur) ast ] as 'recur)) annot)
    : bool
    =
    let (Annot (gt_, (), _, _)) = gt in
    match gt_ with
    | `Arbitrary | `Return _ -> false
    | `Asgn _ | `AsgnElab _ | `Assert _ | `AssertDomain _ -> true
    | `Call _ | `CallSized _ -> true (* Could be less conservative... *)
    | `LetStar ((_, gt1), gt2) | `ITE (_, gt1, gt2) ->
      contains_constraint gt1 || contains_constraint gt2
    | `Map (_, gt') | `MapElab (_, gt') | `SplitSize (_, gt') | `SplitSizeElab (_, _, gt')
      ->
      contains_constraint gt'
    | `Pick gts -> List.exists contains_constraint gts
    | `PickSized wgts | `PickSizedElab (_, wgts) ->
      List.exists (fun (_, g) -> contains_constraint g) wgts


  (* Elaboration *)
  let elaborate_asgn_ (`Asgn ((it_addr, sct), it_value, gt_rest))
    : [> `AsgnElab of
           Sym.t * (((Sym.t * BT.t) * IT.t) * Sctypes.t) * IT.t * ('tag, 'recur) annot
      ]
    =
    let rec pointer_of (it : IT.t) : Sym.t * BT.t =
      match it with
      | IT (CopyAllocId { loc = ptr; _ }, _, _)
      | IT (ArrayShift { base = ptr; _ }, _, _)
      | IT (MemberShift (ptr, _, _), _, _) ->
        pointer_of ptr
      | IT (Sym x, bt, _) | IT (Cast (_, IT (Sym x, bt, _)), _, _) -> (x, bt)
      | _ ->
        let pointers =
          it_addr
          |> IT.free_vars_bts
          |> Sym.Map.filter (fun _ bt -> BT.equal bt (BT.Loc ()))
        in
        if not (Sym.Map.cardinal pointers == 1) then
          Cerb_debug.print_debug 2 [] (fun () ->
            Pp.(
              plain
                (braces
                   (separate_map
                      (comma ^^ space)
                      (fun (x, bt) -> Sym.pp x ^^ colon ^^^ BT.pp bt)
                      (List.of_seq (Sym.Map.to_seq pointers)))
                 ^^^ !^" in "
                 ^^ IT.pp it_addr)));
        if Sym.Map.is_empty pointers then (
          print_endline (Pp.plain (IT.pp it));
          failwith __LOC__);
        Sym.Map.choose pointers
    in
    let backtrack_var = Sym.fresh_anon () in
    let pointer = pointer_of it_addr in
    `AsgnElab (backtrack_var, ((pointer, it_addr), sct), it_value, gt_rest)


  let elaborate_asgn (Annot (gt_, tag, bt, loc)) =
    Annot (elaborate_asgn_ gt_, tag, bt, loc)


  let elaborate_map_ (`Map ((i, i_bt, it_perm), gt_inner)) =
    let it_min, it_max = IndexTerms.Bounds.get_bounds (i, i_bt) it_perm in
    `MapElab ((i, i_bt, (it_min, it_max), it_perm), gt_inner)


  let elaborate_map (Annot (gt_, tag, bt, loc)) = Annot (elaborate_map_ gt_, tag, bt, loc)

  let elaborate_pick_ (`PickSized wgts) =
    let choice_var = Sym.fresh_anon () in
    `PickSizedElab (choice_var, wgts)


  let elaborate_pick (Annot (gt_, tag, bt, loc)) =
    Annot (elaborate_pick_ gt_, tag, bt, loc)


  let elaborate_split_size_ (`SplitSize (syms, gt_rest)) =
    let marker_var = Sym.fresh_anon () in
    `SplitSizeElab (marker_var, syms, gt_rest)


  let elaborate_split_size (Annot (gt_, tag, bt, loc)) =
    Annot (elaborate_split_size_ gt_, tag, bt, loc)
end

module Domain = struct
  let ret_sym = Sym.fresh "return"

  module type T = sig
    type t [@@deriving eq, ord]

    (** The bottom element of the domain *)
    val bottom : t

    (** The top element of the domain *)
    val top : t

    (** Partial order: [leq x y] holds if x ≤ y in the lattice *)
    val leq : t -> t -> bool

    (** Least upper bound (join) *)
    val join : t -> t -> t

    (** Greatest lower bound (meet) *)
    val meet : t -> t -> t

    (* (** Widening operation to ensure fixpoint convergence.
      [widen ~prev ~next] returns an over-approximation of the union
      of [prev] and [next]. *)
  val widen : prev:t -> next:t -> t *)

    (* (** Narrowing operation to refine after widening.
      [narrow ~prev ~next] returns a refined approximation of the intersection
      of [prev] and [next]. *)
  val narrow : prev:t -> next:t -> t *)

    (** Rename a variable *)
    val rename : from:Sym.t -> to_:Sym.t -> t -> t

    (** Remove a variable *)
    val remove : Sym.t -> t -> t

    val pp : t -> Pp.document
  end
end

module type T = sig
  module AD : Domain.T

  type tag_t

  type t_ = private [< (tag_t, 'recur) Make(AD).Inner.ast ] as 'recur

  type t = (tag_t, t_) annot

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val pp : t -> Pp.document
end
