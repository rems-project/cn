module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

type ('tag, 'ast) annot =
  | Annot of
      ('ast * 'tag * BT.t * (Locations.t[@equal fun _ _ -> true] [@compare fun _ _ -> 0]))
[@@deriving eq, ord]

module Base (AD : Domain.T) = struct
  type ('tag, 'recur) ast =
    [ `Arbitrary (** Generate arbitrary values *)
    | `Symbolic (** Generate symbolic values *)
    | `ArbitrarySpecialized of (IT.t option * IT.t option) * (IT.t option * IT.t option)
      (** Generate arbitrary values: ((min_inc, min_ex), (max_inc, max_ex)) *)
    | `ArbitraryDomain of AD.Relative.t
    | `Call of Sym.t * IT.t list
      (** Call a defined generator according to a [Sym.t] with arguments [IT.t list] *)
    | `Asgn of (IT.t * Sctypes.t) * IT.t * ('tag, 'recur) annot
      (** Claim ownership and assign a value to a memory location *)
    | `LetStar of (Sym.t * ('tag, 'recur) annot) * ('tag, 'recur) annot
      (** Backtrack point *)
    | `Return of IT.t (** Monadic return *)
    | `Assert of LC.t * ('tag, 'recur) annot
      (** Assert some [LC.t] are true, backtracking otherwise *)
    | `AssertDomain of AD.t * ('tag, 'recur) annot
      (** Assert domain constraints are satisfied, backtracking otherwise *)
    | `ITE of IT.t * ('tag, 'recur) annot * ('tag, 'recur) annot (** If-then-else *)
    | `Map of (Sym.t * BT.t * IT.t) * ('tag, 'recur) annot
    | `Pick of ('tag, 'recur) annot list (** Pick among a list of options *)
    | `CallSized of Sym.t * IT.t list * (int * Sym.t)
    | `PickSized of (Z.t * ('tag, 'recur) annot) list
      (** Pick among a list of options, weighted by the provided [Z.t]s *)
    | `SplitSize of Sym.Set.t * ('tag, 'recur) annot
    | `AsgnElab of
        Sym.t * (((Sym.t * BT.t) * IT.t) * Sctypes.t) * IT.t * ('tag, 'recur) annot
    | `MapElab of (Sym.t * BT.t * (IT.t * IT.t) * IT.t) * ('tag, 'recur) annot
    | `PickSizedElab of Sym.t * (Z.t * ('tag, 'recur) annot) list
    | `SplitSizeElab of Sym.t * Sym.Set.t * ('tag, 'recur) annot
    ]
  [@@deriving eq, ord]
end

module type T = sig
  module AD : Domain.T

  type tag_t

  type t_ = private [< (tag_t, 'recur) Base(AD).ast ] as 'recur

  type t = (tag_t, t_) annot

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val arbitrary_ : tag_t -> BT.t -> Locations.t -> t

  val symbolic_ : tag_t -> BT.t -> Locations.t -> t

  val arbitrary_specialized_
    :  (IT.t option * IT.t option) * (IT.t option * IT.t option) ->
    tag_t ->
    BT.t ->
    Locations.t ->
    t

  val arbitrary_domain_ : AD.Relative.t -> tag_t -> BT.t -> Locations.t -> t

  val call_ : Sym.t * IT.t list -> tag_t -> BT.t -> Locations.t -> t

  val asgn_ : (IT.t * Sctypes.t) * IT.t * t -> tag_t -> Locations.t -> t

  val let_star_ : (Sym.t * t) * t -> tag_t -> Locations.t -> t

  val return_ : IT.t -> tag_t -> Locations.t -> t

  val assert_ : LC.t * t -> tag_t -> Locations.t -> t

  val assert_domain_ : AD.t * t -> tag_t -> Locations.t -> t

  val ite_ : IT.t * t * t -> tag_t -> Locations.t -> t

  val map_ : (Sym.t * BT.t * IT.t) * t -> tag_t -> Locations.t -> t

  val map_elab_ : (Sym.t * BT.t * (IT.t * IT.t) * IT.t) * t -> tag_t -> Locations.t -> t

  val pick_ : t list -> tag_t -> BT.t -> Locations.t -> t

  val pick_sized_ : (Z.t * t) list -> tag_t -> BT.t -> Locations.t -> t

  val pick_sized_elab_ : Sym.t -> (Z.t * t) list -> tag_t -> BT.t -> Locations.t -> t

  val call_sized_ : Sym.t * IT.t list * (int * Sym.t) -> tag_t -> BT.t -> Locations.t -> t

  val asgn_elab_
    :  Sym.t * (((Sym.t * BT.t) * IT.t) * Sctypes.t) * IT.t * t ->
    tag_t ->
    Locations.t ->
    t

  val split_size_ : Sym.Set.t * t -> tag_t -> Locations.t -> t

  val split_size_elab_ : Sym.t * Sym.Set.t * t -> tag_t -> Locations.t -> t
end

module Make (GT : T) = struct
  include Base (GT.AD)
  include GT

  let basetype (Annot (_, _, bt, _) : ('tag, 'ast) annot) : BT.t = bt

  let loc (Annot (_, _, _, loc) : ('tag, 'ast) annot) : Locations.t = loc

  (* Constructor-checking functions *)

  let is_arbitrary_ (gt_ : GT.t_) : unit option =
    match gt_ with `Arbitrary -> Some () | _ -> None


  let is_arbitrary (gt : GT.t) : unit option =
    let (Annot (gt_, _, _, _)) = gt in
    is_arbitrary_ gt_


  let is_call_ (gt_ : GT.t_) : (Sym.t * IT.t list) option =
    match gt_ with `Call x -> Some x | _ -> None


  let is_call (gt : GT.t) =
    let (Annot (gt_, _, _, _)) = gt in
    is_call_ gt_


  let is_asgn_ (gt_ : GT.t_) : ((IT.t * Sctypes.ctype) * IT.t * GT.t) option =
    match gt_ with `Asgn x -> Some x | _ -> None


  let is_asgn (gt : GT.t) =
    let (Annot (gt_, _, _, _)) = gt in
    is_asgn_ gt_


  let is_let_star_ (gt_ : GT.t_) : ((Sym.t * GT.t) * GT.t) option =
    match gt_ with `LetStar x -> Some x | _ -> None


  let is_let_star (gt : GT.t) =
    let (Annot (gt_, _, _, _)) = gt in
    is_let_star_ gt_


  (* Other utility functions *)

  let rec pp (tm : GT.t) : Pp.document =
    let open Pp in
    let (Annot (tm_, _, bt, _)) = tm in
    match tm_ with
    | `Arbitrary -> !^"arbitrary" ^^ angles (BT.pp bt) ^^ parens empty
    | `Symbolic -> !^"symbolic" ^^ angles (BT.pp bt) ^^ parens empty
    | `ArbitrarySpecialized ((min_inc, min_ex), (max_inc, max_ex)) ->
      let pp_opt = function
        | None -> !^"None"
        | Some it -> !^"Some" ^^^ parens (IT.pp it)
      in
      !^"arbitrary_specialized"
      ^^ angles (BT.pp bt)
      ^^ parens
           (parens (pp_opt min_inc ^^ comma ^^^ pp_opt min_ex)
            ^^ comma
            ^^^ parens (pp_opt max_inc ^^ comma ^^^ pp_opt max_ex))
    | `ArbitraryDomain d ->
      !^"arbitrary_domain" ^^ angles (BT.pp bt) ^^ parens (AD.Relative.pp d)
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
    | `AssertDomain (d, gt') ->
      !^"assert_domain"
      ^^ parens (nest 2 (break 1 ^^ AD.pp d) ^^ break 1)
      ^^ semi
      ^/^ pp gt'
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


  let rec free_vars_bts_ (gt_ : GT.t_) : BT.t Sym.Map.t =
    match gt_ with
    | `Arbitrary | `ArbitraryDomain _ | `Symbolic -> Sym.Map.empty
    | `ArbitrarySpecialized ((min_inc, min_ex), (max_inc, max_ex)) ->
      IT.free_vars_bts_list (List.filter_map Fun.id [ min_inc; min_ex; max_inc; max_ex ])
    | `Call (_, iargs) | `CallSized (_, iargs, _) -> IT.free_vars_bts_list iargs
    | `Asgn ((it_addr, _), it_val, gt') | `AsgnElab (_, ((_, it_addr), _), it_val, gt') ->
      Sym.Map.union
        (fun _ bt1 bt2 ->
           assert (BT.equal bt1 bt2);
           Some bt1)
        (IT.free_vars_bts_list [ it_addr; it_val ])
        (free_vars_bts gt')
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
    | `AssertDomain (_, gt') -> free_vars_bts gt'
    | `ITE (it_if, gt_then, gt_else) ->
      Sym.Map.union
        (fun _ bt1 bt2 ->
           assert (BT.equal bt1 bt2);
           Some bt1)
        (IT.free_vars_bts it_if)
        (free_vars_bts_list [ gt_then; gt_else ])
    | `Map ((i, _, it_perm), gt') | `MapElab ((i, _, _, it_perm), gt') ->
      Sym.Map.remove
        i
        (Sym.Map.union
           (fun _ bt1 bt2 ->
              assert (BT.equal bt1 bt2);
              Some bt1)
           (IT.free_vars_bts it_perm)
           (free_vars_bts gt'))
    | `Pick gts -> free_vars_bts_list gts
    | `PickSized wgts | `PickSizedElab (_, wgts) ->
      wgts |> List.map snd |> free_vars_bts_list
    | `SplitSize (_, gt') | `SplitSizeElab (_, _, gt') -> free_vars_bts gt'


  and free_vars_bts (Annot (gt_, _, _, _) : GT.t) : BT.t Sym.Map.t = free_vars_bts_ gt_

  and free_vars_bts_list : GT.t list -> BT.t Sym.Map.t =
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


  let rec contains_call (gt : GT.t) : bool =
    let (Annot (gt_, _, _, _)) = gt in
    match gt_ with
    | `Arbitrary | `ArbitraryDomain _ | `ArbitrarySpecialized _ | `Symbolic | `Return _ ->
      false
    | `Call _ | `CallSized _ -> true
    | `LetStar ((_, gt1), gt2) | `ITE (_, gt1, gt2) ->
      contains_call gt1 || contains_call gt2
    | `Asgn (_, _, gt')
    | `AsgnElab (_, _, _, gt')
    | `Assert (_, gt')
    | `AssertDomain (_, gt')
    | `Map (_, gt')
    | `MapElab (_, gt')
    | `SplitSize (_, gt')
    | `SplitSizeElab (_, _, gt') ->
      contains_call gt'
    | `Pick gts -> List.exists contains_call gts
    | `PickSized wgts | `PickSizedElab (_, wgts) ->
      List.exists (fun (_, g) -> contains_call g) wgts


  let rec contains_constraint (gt : GT.t) : bool =
    let (Annot (gt_, _, _, _)) = gt in
    match gt_ with
    | `Arbitrary | `ArbitraryDomain _ | `ArbitrarySpecialized _ | `Symbolic | `Return _ ->
      false
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


  let get_pure_functions (gt : GT.t) : Sym.Set.t =
    let rec aux (gt : GT.t) : Sym.Set.t =
      let (Annot (gt_, _, _, _)) = gt in
      match gt_ with
      | `Arbitrary | `Symbolic | `ArbitraryDomain _ -> Sym.Set.empty
      | `ArbitrarySpecialized ((min_inc, min_ex), (max_inc, max_ex)) ->
        [ min_inc; min_ex; max_inc; max_ex ]
        |> List.filter_map Fun.id
        |> List.map IT.preds_of
        |> List.fold_left Sym.Set.union Sym.Set.empty
      | `Return it -> IT.preds_of it
      | `Call (_, its) | `CallSized (_, its, _) ->
        its |> List.map IT.preds_of |> List.fold_left Sym.Set.union Sym.Set.empty
      | `Asgn ((it_addr, _), it_val, gt_rest)
      | `AsgnElab (_, ((_, it_addr), _), it_val, gt_rest) ->
        [ it_addr; it_val ]
        |> List.map IT.preds_of
        |> List.fold_left Sym.Set.union (aux gt_rest)
      | `LetStar ((_, gt_inner), gt_rest) -> Sym.Set.union (aux gt_inner) (aux gt_rest)
      | `Assert (lc, gt_rest) -> Sym.Set.union (LC.preds_of lc) (aux gt_rest)
      | `AssertDomain (_, gt_rest)
      | `SplitSize (_, gt_rest)
      | `SplitSizeElab (_, _, gt_rest) ->
        aux gt_rest
      | `ITE (it_if, gt_then, gt_else) ->
        List.fold_left Sym.Set.union (IT.preds_of it_if) [ aux gt_then; aux gt_else ]
      | `Map ((_, _, it_perm), gt_inner) | `MapElab ((_, _, _, it_perm), gt_inner) ->
        Sym.Set.union (IT.preds_of it_perm) (aux gt_inner)
      | `Pick gts -> gts |> List.map aux |> List.fold_left Sym.Set.union Sym.Set.empty
      | `PickSized wgts | `PickSizedElab (_, wgts) ->
        wgts |> List.map snd |> List.map aux |> List.fold_left Sym.Set.union Sym.Set.empty
    in
    aux gt


  let rec subst (su : [ `Term of IT.t | `Rename of Sym.t ] Subst.t) (gt : t) : t =
    let (Annot (gt_, tag, bt, loc)) = gt in
    match gt_ with
    | `Arbitrary -> arbitrary_ tag bt loc
    | `Symbolic -> symbolic_ tag bt loc
    | `ArbitrarySpecialized ((min_inc, min_ex), (max_inc, max_ex)) ->
      arbitrary_specialized_
        ( (Option.map (IT.subst su) min_inc, Option.map (IT.subst su) min_ex),
          (Option.map (IT.subst su) max_inc, Option.map (IT.subst su) max_ex) )
        tag
        bt
        loc
    | `ArbitraryDomain ad -> arbitrary_domain_ ad tag bt loc
    | `Call (fsym, iargs) -> call_ (fsym, List.map (IT.subst su) iargs) tag bt loc
    | `CallSized (fsym, iargs, sz) ->
      call_sized_ (fsym, List.map (IT.subst su) iargs, sz) tag bt loc
    | `Asgn ((it_addr, sct), it_val, g') ->
      asgn_ ((IT.subst su it_addr, sct), IT.subst su it_val, subst su g') tag loc
    | `AsgnElab (backtrack_var, ((pointer, it_addr), sct), it_val, g') ->
      asgn_elab_
        ( backtrack_var,
          ((pointer, IT.subst su it_addr), sct),
          IT.subst su it_val,
          subst su g' )
        tag
        loc
    | `LetStar ((x, gt1), gt2) ->
      let x, gt2 = suitably_alpha_rename_gen su.relevant x gt2 in
      let_star_ ((x, subst su gt1), subst su gt2) tag loc
    | `Return it -> return_ (IT.subst su it) tag loc
    | `Assert (lc, gt') -> assert_ (LC.subst su lc, subst su gt') tag loc
    | `AssertDomain (ad, gt') -> assert_domain_ (ad, subst su gt') tag loc
    | `ITE (it, gt_then, gt_else) ->
      ite_ (IT.subst su it, subst su gt_then, subst su gt_else) tag loc
    | `Map ((i, i_bt, it_perm), gt') ->
      let i', it_perm = IT.suitably_alpha_rename su.relevant i it_perm in
      let gt' = subst (IT.make_rename ~from:i ~to_:i') gt' in
      map_ ((i', i_bt, IT.subst su it_perm), subst su gt') tag loc
    | `MapElab ((i, i_bt, (it_min, it_max), it_perm), gt') ->
      let i', it_min = IT.suitably_alpha_rename su.relevant i it_min in
      let it_max = IT.subst (IT.make_rename ~from:i ~to_:i') it_max in
      let it_perm = IT.subst (IT.make_rename ~from:i ~to_:i') it_perm in
      let gt' = subst (IT.make_rename ~from:i ~to_:i') gt' in
      map_elab_
        ( (i', i_bt, (IT.subst su it_min, IT.subst su it_max), IT.subst su it_perm),
          subst su gt' )
        tag
        loc
    | `Pick gts -> pick_ (List.map (subst su) gts) tag bt loc
    | `PickSized choices ->
      pick_sized_ (List.map (fun (w, g) -> (w, subst su g)) choices) tag bt loc
    | `PickSizedElab (pick_var, choices) ->
      pick_sized_elab_
        pick_var
        (List.map (fun (w, g) -> (w, subst su g)) choices)
        tag
        bt
        loc
    | `SplitSize (syms, gt') -> split_size_ (syms, subst su gt') tag loc
    | `SplitSizeElab (split_var, syms, gt') ->
      split_size_elab_ (split_var, syms, subst su gt') tag loc


  and alpha_rename_gen x gt =
    let x' = Sym.fresh_same x in
    (x', subst (IT.make_rename ~from:x ~to_:x') gt)


  and suitably_alpha_rename_gen syms x gt =
    if Sym.Set.mem x syms then
      alpha_rename_gen x gt
    else
      (x, gt)


  let rec map_gen_pre (f : t -> t) (g : t) : t =
    let (Annot (gt_, tag, bt, loc)) = f g in
    match gt_ with
    | `Arbitrary -> arbitrary_ tag bt loc
    | `Symbolic -> symbolic_ tag bt loc
    | `ArbitrarySpecialized bounds -> arbitrary_specialized_ bounds tag bt loc
    | `ArbitraryDomain ad -> arbitrary_domain_ ad tag bt loc
    | `Call (fsym, its) -> call_ (fsym, its) tag bt loc
    | `CallSized (fsym, its, sz) -> call_sized_ (fsym, its, sz) tag bt loc
    | `Asgn ((it_addr, sct), it_val, gt') ->
      asgn_ ((it_addr, sct), it_val, map_gen_pre f gt') tag loc
    | `AsgnElab (backtrack_var, ((pointer, it_addr), sct), it_val, gt') ->
      asgn_elab_
        (backtrack_var, ((pointer, it_addr), sct), it_val, map_gen_pre f gt')
        tag
        loc
    | `LetStar ((x, gt1), gt2) ->
      let_star_ ((x, map_gen_pre f gt1), map_gen_pre f gt2) tag loc
    | `Return it -> return_ it tag loc
    | `Assert (lc, gt') -> assert_ (lc, map_gen_pre f gt') tag loc
    | `AssertDomain (ad, gt') -> assert_domain_ (ad, map_gen_pre f gt') tag loc
    | `ITE (it, gt_then, gt_else) ->
      ite_ (it, map_gen_pre f gt_then, map_gen_pre f gt_else) tag loc
    | `Map ((i, i_bt, it_perm), gt') ->
      map_ ((i, i_bt, it_perm), map_gen_pre f gt') tag loc
    | `MapElab ((i, i_bt, (it_min, it_max), it_perm), gt') ->
      map_elab_ ((i, i_bt, (it_min, it_max), it_perm), map_gen_pre f gt') tag loc
    | `Pick gts ->
      let new_gts = List.map (map_gen_pre f) gts in
      let new_bt = match new_gts with [] -> bt | hd :: _ -> basetype hd in
      pick_ new_gts tag new_bt loc
    | `PickSized choices ->
      let new_choices = List.map (fun (w, g) -> (w, map_gen_pre f g)) choices in
      let new_bt = match new_choices with [] -> bt | (_, hd) :: _ -> basetype hd in
      pick_sized_ new_choices tag new_bt loc
    | `PickSizedElab (pick_var, choices) ->
      let new_choices = List.map (fun (w, g) -> (w, map_gen_pre f g)) choices in
      let new_bt = match new_choices with [] -> bt | (_, hd) :: _ -> basetype hd in
      pick_sized_elab_ pick_var new_choices tag new_bt loc
    | `SplitSize (syms, gt') -> split_size_ (syms, map_gen_pre f gt') tag loc
    | `SplitSizeElab (split_var, syms, gt') ->
      split_size_elab_ (split_var, syms, map_gen_pre f gt') tag loc


  let rec map_gen_post (f : t -> t) (g : t) : t =
    let (Annot (gt_, tag, bt, loc)) = g in
    let result =
      match gt_ with
      | `Arbitrary -> arbitrary_ tag bt loc
      | `Symbolic -> symbolic_ tag bt loc
      | `ArbitrarySpecialized bounds -> arbitrary_specialized_ bounds tag bt loc
      | `ArbitraryDomain ad -> arbitrary_domain_ ad tag bt loc
      | `Call (fsym, its) -> call_ (fsym, its) tag bt loc
      | `CallSized (fsym, its, sz) -> call_sized_ (fsym, its, sz) tag bt loc
      | `Asgn ((it_addr, sct), it_val, gt') ->
        asgn_ ((it_addr, sct), it_val, map_gen_post f gt') tag loc
      | `AsgnElab (backtrack_var, ((pointer, it_addr), sct), it_val, gt') ->
        asgn_elab_
          (backtrack_var, ((pointer, it_addr), sct), it_val, map_gen_post f gt')
          tag
          loc
      | `LetStar ((x, gt1), gt2) ->
        let_star_ ((x, map_gen_post f gt1), map_gen_post f gt2) tag loc
      | `Return it -> return_ it tag loc
      | `Assert (lc, gt') -> assert_ (lc, map_gen_post f gt') tag loc
      | `AssertDomain (ad, gt') -> assert_domain_ (ad, map_gen_post f gt') tag loc
      | `ITE (it, gt_then, gt_else) ->
        ite_ (it, map_gen_post f gt_then, map_gen_post f gt_else) tag loc
      | `Map ((i, i_bt, it_perm), gt') ->
        map_ ((i, i_bt, it_perm), map_gen_post f gt') tag loc
      | `MapElab ((i, i_bt, (it_min, it_max), it_perm), gt') ->
        map_elab_ ((i, i_bt, (it_min, it_max), it_perm), map_gen_post f gt') tag loc
      | `Pick gts ->
        let new_gts = List.map (map_gen_post f) gts in
        let new_bt = match new_gts with [] -> bt | hd :: _ -> basetype hd in
        pick_ new_gts tag new_bt loc
      | `PickSized choices ->
        let new_choices = List.map (fun (w, g) -> (w, map_gen_post f g)) choices in
        let new_bt = match new_choices with [] -> bt | (_, hd) :: _ -> basetype hd in
        pick_sized_ new_choices tag new_bt loc
      | `PickSizedElab (pick_var, choices) ->
        let new_choices = List.map (fun (w, g) -> (w, map_gen_post f g)) choices in
        let new_bt = match new_choices with [] -> bt | (_, hd) :: _ -> basetype hd in
        pick_sized_elab_ pick_var new_choices tag new_bt loc
      | `SplitSize (syms, gt') -> split_size_ (syms, map_gen_post f gt') tag loc
      | `SplitSizeElab (split_var, syms, gt') ->
        split_size_elab_ (split_var, syms, map_gen_post f gt') tag loc
    in
    f result


  let rec upcast_ (tm : t_) : ('tag, 'recur) ast as 'recur =
    match tm with
    | `Arbitrary -> `Arbitrary
    | `Symbolic -> `Symbolic
    | `ArbitrarySpecialized bounds -> `ArbitrarySpecialized bounds
    | `ArbitraryDomain ad -> `ArbitraryDomain ad
    | `Call (fsym, iargs) -> `Call (fsym, iargs)
    | `CallSized (fsym, iargs, sz) -> `CallSized (fsym, iargs, sz)
    | `Asgn (addr_sct, it_val, g') -> `Asgn (addr_sct, it_val, upcast g')
    | `AsgnElab (backtrack_var, pointer_addr_sct, it_val, g') ->
      `AsgnElab (backtrack_var, pointer_addr_sct, it_val, upcast g')
    | `LetStar (x_gt1, gt2) ->
      let x, gt1 = x_gt1 in
      `LetStar ((x, upcast gt1), upcast gt2)
    | `Return it -> `Return it
    | `Assert (lc, gt') -> `Assert (lc, upcast gt')
    | `AssertDomain (ad, gt') -> `AssertDomain (ad, upcast gt')
    | `ITE (it, gt_then, gt_else) -> `ITE (it, upcast gt_then, upcast gt_else)
    | `Map (i_bt_perm, gt') -> `Map (i_bt_perm, upcast gt')
    | `MapElab (i_bt_bounds_perm, gt') -> `MapElab (i_bt_bounds_perm, upcast gt')
    | `Pick gts -> `Pick (List.map upcast gts)
    | `PickSized choices -> `PickSized (List.map (fun (w, g) -> (w, upcast g)) choices)
    | `PickSizedElab (pick_var, choices) ->
      `PickSizedElab (pick_var, List.map (fun (w, g) -> (w, upcast g)) choices)
    | `SplitSize (syms, gt') -> `SplitSize (syms, upcast gt')
    | `SplitSizeElab (split_var, syms, gt') -> `SplitSizeElab (split_var, syms, upcast gt')


  and upcast (Annot (tm_, tag, bt, loc)) : ('tag, (('tag, 'recur) ast as 'recur)) annot =
    Annot (upcast_ tm_, tag, bt, loc)


  let rec downcast (tm : ('tag, (('tag, 'recur) ast as 'recur)) annot) : t =
    let (Annot (tm_, tag, bt, loc)) = tm in
    match tm_ with
    | `Arbitrary -> arbitrary_ tag bt loc
    | `Symbolic -> symbolic_ tag bt loc
    | `ArbitrarySpecialized bounds -> arbitrary_specialized_ bounds tag bt loc
    | `ArbitraryDomain ad -> arbitrary_domain_ ad tag bt loc
    | `Call (fsym, iargs) -> call_ (fsym, iargs) tag bt loc
    | `CallSized (fsym, iargs, sz) -> call_sized_ (fsym, iargs, sz) tag bt loc
    | `Asgn (addr_sct, it_val, g') -> asgn_ (addr_sct, it_val, downcast g') tag loc
    | `AsgnElab (backtrack_var, pointer_addr_sct, it_val, g') ->
      asgn_elab_ (backtrack_var, pointer_addr_sct, it_val, downcast g') tag loc
    | `LetStar (x_gt1, gt2) ->
      let x, gt1 = x_gt1 in
      let_star_ ((x, downcast gt1), downcast gt2) tag loc
    | `Return it -> return_ it tag loc
    | `Assert (lc, gt') -> assert_ (lc, downcast gt') tag loc
    | `AssertDomain (ad, gt') -> assert_domain_ (ad, downcast gt') tag loc
    | `ITE (it, gt_then, gt_else) -> ite_ (it, downcast gt_then, downcast gt_else) tag loc
    | `Map (i_bt_perm, gt') -> map_ (i_bt_perm, downcast gt') tag loc
    | `MapElab (i_bt_bounds_perm, gt') ->
      map_elab_ (i_bt_bounds_perm, downcast gt') tag loc
    | `Pick gts -> pick_ (List.map downcast gts) tag bt loc
    | `PickSized choices -> pick_sized_ (List.map_snd downcast choices) tag bt loc
    | `PickSizedElab (pick_var, choices) ->
      pick_sized_elab_ pick_var (List.map_snd downcast choices) tag bt loc
    | `SplitSize (syms, gt') -> split_size_ (syms, downcast gt') tag loc
    | `SplitSizeElab (split_var, syms, gt') ->
      split_size_elab_ (split_var, syms, downcast gt') tag loc


  (***************)
  (* Elaboration *)
  (***************)

  let elaborate_asgn_ (`Asgn ((it_addr, sct), it_value, gt_rest))
    : [> `AsgnElab of Sym.t * (((Sym.t * BT.t) * IT.t) * Sctypes.t) * IT.t * GT.t ]
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
    let it_min, it_max = IT.Bounds.get_bounds (i, i_bt) it_perm in
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

(** Module providing default implementations for unsupported stage constructors *)
module Defaults (StageName : sig
    val name : string
  end) =
struct
  let unsupported name = failwith (name ^ " not supported in " ^ StageName.name ^ " DSL")

  let arbitrary_specialized_ _ _ _ _ = unsupported "arbitrary_specialized_"

  let arbitrary_domain_ _ _ _ _ = unsupported "arbitrary_domain_"

  let pick_ _ _ _ _ = unsupported "pick_"

  let pick_sized_ _ _ _ _ = unsupported "pick_sized_"

  let pick_sized_elab_ _ _ _ _ _ = unsupported "pick_sized_elab_"

  let asgn_elab_ _ _ _ = unsupported "asgn_elab_"

  let split_size_ _ _ _ = unsupported "split_size_"

  let split_size_elab_ _ _ _ = unsupported "split_size_elab_"

  let map_elab_ _ _ _ = unsupported "map_elab_"

  let call_sized_ _ _ _ _ = unsupported "call_sized_"

  let assert_domain_ _ _ _ = unsupported "assert_domain_"

  let map_ _ _ _ = unsupported "map_"

  let asgn_ _ _ _ = unsupported "asgn_"
end
