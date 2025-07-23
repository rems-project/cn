module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module SymGraph = Graph.Persistent.Digraph.Concrete (Sym)
module StringMap = Map.Make (String)

type t_ =
  [ `Arbitrary (** Generate arbitrary values *)
  | `Pick of Sym.t * (Z.t * t) list
    (** Pick among a list of options, weighted by the provided [Z.t]s *)
  | `Call of Sym.t * IT.t list * (int * Sym.t) option
    (** Call a defined generator according to a [Sym.t] with arguments [IT.t list] *)
  | `Asgn of ((Sym.t * (Sym.t * BT.t) * IT.t) * Sctypes.t) * IT.t * t
    (** Claim ownership and assign a value to a memory location *)
  | `LetStar of (Sym.t * t) * t (** Backtrack point *)
  | `Return of IT.t (** Monadic return *)
  | `Assert of LC.t * t (** Assert some [LC.t] are true, backtracking otherwise *)
  | `AssertDomain of Sym.t * BT.t * Abstract.domain * t (** Domain assertion *)
  | `ITE of IT.t * t * t (** If-then-else *)
  | `Map of (Sym.t * BT.t * (IT.t * IT.t) * IT.t) * t
  | `SplitSize of Sym.t * Sym.Set.t * t
  ]
[@@deriving eq, ord]

and t =
  | GT of
      t_
      * BT.t
      * (Sym.Set.t * Sym.t)
      * (Locations.t[@equal fun _ _ -> true] [@compare fun _ _ -> 0])
[@@deriving eq, ord]

let basetype (GT (_, bt, _, _) : t) = bt

let is_return (GT (tm_, _, _, _) : t) : bool =
  match tm_ with `Return _ -> true | _ -> false


let rec free_vars (tm : t) : Sym.Set.t =
  let (GT (tm_, _, _, _)) = tm in
  match tm_ with
  | `Arbitrary -> Sym.Set.empty
  | `Pick (_, wgts) -> free_vars_list (List.map snd wgts)
  | `Call (_, iargs, _) ->
    iargs |> List.map IT.free_vars |> List.fold_left Sym.Set.union Sym.Set.empty
  | `Asgn (((_backtrack_var, _pointer, it_addr), _sct), it_val, gt_rest) ->
    Sym.Set.union (IT.free_vars_list [ it_addr; it_val ]) (free_vars gt_rest)
  | `LetStar ((x, gt_inner), gt_rest) ->
    Sym.Set.union (free_vars gt_inner) (Sym.Set.remove x (free_vars gt_rest))
  | `Return it -> IT.free_vars it
  | `Assert (lc, gt_rest) -> Sym.Set.union (LC.free_vars lc) (free_vars gt_rest)
  | `AssertDomain (x, _x_bt, domain, gt_rest) ->
    let unwrap = function
      | Some it -> Sym.Set.union (IT.free_vars it)
      | None -> fun y -> y
    in
    Sym.Set.singleton x
    |> unwrap domain.lower_bound_inc
    |> unwrap domain.lower_bound_ex
    |> unwrap domain.upper_bound_inc
    |> unwrap domain.upper_bound_ex
    |> unwrap domain.multiple
    |> Sym.Set.union (free_vars gt_rest)
    |> Sym.Set.add x
  | `ITE (it_if, gt_then, gt_else) ->
    Sym.Set.union (IT.free_vars it_if) (free_vars_list [ gt_then; gt_else ])
  | `Map ((i, _bt, (it_min, it_max), it_perm), gt_inner) ->
    Sym.Set.remove
      i
      (Sym.Set.union (IT.free_vars_list [ it_min; it_max; it_perm ]) (free_vars gt_inner))
  | `SplitSize (_, _syms, gt_rest) -> free_vars gt_rest


and free_vars_list : t list -> Sym.Set.t =
  fun xs -> List.fold_left (fun ss t -> Sym.Set.union ss (free_vars t)) Sym.Set.empty xs


let rec pp (tm : t) : Pp.document =
  let open Pp in
  let (GT (tm_, bt, (path_vars, last_var), _)) = tm in
  match tm_ with
  | `Arbitrary -> !^"arbitrary" ^^ angles (BT.pp bt) ^^ parens empty
  | `Pick (choice_var, wgts) ->
    !^"pick"
    ^^ parens
         (c_comment (!^"chosen by " ^^ Sym.pp choice_var)
          ^^ comma
          ^/^ twice slash
          ^^^ !^"backtracks to"
          ^^^ Sym.pp last_var
          ^/^ brackets
                (nest
                   2
                   (break 1
                    ^^ c_comment (BT.pp bt)
                    ^/^ separate_map
                          (semi ^^ break 1)
                          (fun (w, gt) ->
                             parens (z w ^^ comma ^^ braces (nest 2 (break 1 ^^ pp gt))))
                          wgts)))
  | `Call (fsym, iargs, sized) ->
    parens
      (Sym.pp fsym
       ^^ optional (fun (n, sym) -> brackets (int n ^^ comma ^^^ Sym.pp sym)) sized
       ^^ parens (nest 2 (separate_map (comma ^^ break 1) IT.pp iargs))
       ^^^ colon
       ^^^ BT.pp bt
       ^^ c_comment
            (!^"path affected by"
             ^^^ separate_map
                   (comma ^^ space)
                   Sym.pp
                   (path_vars |> Sym.Set.to_seq |> List.of_seq)
             ^^ !^"and backtracks to"
             ^^^ Sym.pp last_var))
  | `Asgn (((backtrack_var, (p_sym, p_bt), it_addr), sct), it_val, gt_rest) ->
    Sctypes.pp sct
    ^^^ IT.pp it_addr
    ^^^ !^":="
    ^^^ IT.pp it_val
    ^^ semi
    ^^^ c_comment
          (!^"backtracks as"
           ^^^ Sym.pp backtrack_var
           ^^^ !^"to"
           ^^^ Sym.pp last_var
           ^^ !^" allocs via "
           ^^ Sym.pp p_sym
           ^^^ colon
           ^^^ BT.pp p_bt)
    ^/^ pp gt_rest
  | `LetStar ((x, gt_inner), gt_rest) ->
    !^"let*"
    ^^^ Sym.pp x
    ^^^ colon
    ^^^ BT.pp (basetype gt_inner)
    ^^^ equals
    ^^ nest 2 (break 1 ^^ pp gt_inner)
    ^^ semi
    ^^^ twice slash
    ^^^ !^"backtracks to"
    ^^^ Sym.pp last_var
    ^/^ pp gt_rest
  | `Return it -> !^"return" ^^^ IT.pp it
  | `Assert (lc, gt_rest) ->
    !^"assert"
    ^^ parens (nest 2 (break 1 ^^ LC.pp lc) ^^ break 1)
    ^^ semi
    ^^^ twice slash
    ^^^ !^"backtracks to"
    ^^^ Sym.pp last_var
    ^/^ pp gt_rest
  | `AssertDomain (x, _x_bt, _domain, gt_rest) ->
    !^"assert_domain"
    ^^ brackets (Sym.pp x)
    ^^ parens (nest 2 (break 1 ^^ !^"TODO: `Abstract.pp_domain`") ^^ break 1)
    ^^ semi
    ^^^ twice slash
    ^^^ !^"backtracks to"
    ^^^ Sym.pp last_var
    ^/^ pp gt_rest
  | `ITE (it_if, gt_then, gt_else) ->
    !^"if"
    ^^^ parens (IT.pp it_if)
    ^^^ braces (nest 2 (break 1 ^^ c_comment (BT.pp bt) ^/^ pp gt_then) ^^ break 1)
    ^^^ !^"else"
    ^^^ braces (nest 2 (break 1 ^^ pp gt_else) ^^ break 1)
  | `Map ((i, _bt, (it_min, it_max), it_perm), gt_inner) ->
    let i_bt, _ = BT.map_bt bt in
    !^"map"
    ^^^ parens
          (BT.pp i_bt
           ^^^ Sym.pp i
           ^^ semi
           ^^^ IT.pp it_perm
           ^^ c_comment (IT.pp it_min ^^ !^" <= " ^^ Sym.pp i ^^ !^" <= " ^^ IT.pp it_max)
           ^^ c_comment (!^"backtracks to" ^^^ Sym.pp last_var))
    ^^ braces (c_comment (BT.pp bt) ^^ nest 2 (break 1 ^^ pp gt_inner) ^^ break 1)
  | `SplitSize (marker_var, syms, gt_rest) ->
    !^"split_size"
    ^^ brackets (Sym.pp marker_var)
    ^^ parens
         (separate_map (comma ^^ space) Sym.pp (syms |> Sym.Set.to_seq |> List.of_seq))
    ^^^ c_comment
          (!^"backtracks to"
           ^^^ Sym.pp last_var
           ^^ comma
           ^^^ !^"path affected by"
           ^^^ separate_map
                 (comma ^^ space)
                 Sym.pp
                 (path_vars |> Sym.Set.to_seq |> List.of_seq))
    ^^ semi
    ^/^ pp gt_rest
