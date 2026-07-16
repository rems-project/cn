type const =
  | Z of Z.t
  | Bits of (BaseTypes.sign * int) * Z.t
  | Q of Q.t
  | MemByte of
      { alloc_id : Z.t option;
        value : Z.t
      }
  | Pointer of
      { alloc_id : Z.t;
        addr : Z.t
      }
  | Alloc_id of Z.t
  | Bool of bool
  | Unit
  | Null
  | CType_const of Sctypes.ctype
  | Default of BaseTypes.t
  (** Default bt: equivalent to a unique variable of
                               base type bt, that we know nothing about other
                               than Default bt = Default bt *)
[@@deriving eq, ord]

type unop =
  | Not
  | Negate
  | BW_CLZ_NoSMT
  | BW_CTZ_NoSMT
  | BW_FFS_NoSMT
  | BW_FLS_NoSMT
  | BW_Compl
[@@deriving eq, ord, show]

type binop =
  | And
  | Or
  | Implies
  | Add
  | Sub
  | Mul
  | MulNoSMT
  | Div
  | DivNoSMT
  | Exp
  | ExpNoSMT
  | Rem
  | RemNoSMT
  | Mod
  | ModNoSMT
  | BW_Xor
  | BW_And
  | BW_Or
  | ShiftLeft
  | ShiftRight
  | LT
  | LE
  | Min
  | Max
  | EQ
  | LTPointer
  | LEPointer
  | SetUnion
  | SetIntersection
  | SetDifference
  | SetMember
  | Subset
[@@deriving eq, ord, show]

type 'bt pattern_ =
  | PSym of Sym.t
  | PWild
  | PConstructor of Sym.t * (Id.t * 'bt pattern) list

and 'bt pattern =
  | Pat of
      'bt pattern_ * 'bt * (Locations.t[@equal fun _ _ -> true] [@compare fun _ _ -> 0])
[@@deriving eq, ord, map]

type 'bt term =
  | Const of const
  | Sym of Sym.t
  | Unop of unop * 'bt annot
  | Binop of binop * 'bt annot * 'bt annot
  | ITE of 'bt annot * 'bt annot * 'bt annot
  | EachI of (int * (Sym.t * BaseTypes.t) * int) * 'bt annot
  (* add Z3's Distinct for separation facts *)
  | Tuple of 'bt annot list
  | NthTuple of int * 'bt annot
  | Struct of Sym.t * (Id.t * 'bt annot) list
  | StructMember of 'bt annot * Id.t
  | StructUpdate of ('bt annot * Id.t) * 'bt annot
  | Record of (Id.t * 'bt annot) list
  | RecordMember of 'bt annot * Id.t
  | RecordUpdate of ('bt annot * Id.t) * 'bt annot (* this is currently unused *)
  | Constructor of Sym.t * (Id.t * 'bt annot) list
  | MemberShift of 'bt annot * Sym.t * Id.t
  | ArrayShift of
      { base : 'bt annot;
        ct : Sctypes.t;
        index : 'bt annot
      }
  | CopyAllocId of
      { addr : 'bt annot;
        loc : 'bt annot
      }
  | HasAllocId of 'bt annot
  | SizeOf of Sctypes.t
  | OffsetOf of Sym.t * Id.t
  | Nil of BaseTypes.t
  | Cons of 'bt annot * 'bt annot
  | Head of 'bt annot
  | Tail of 'bt annot
  | Representable of Sctypes.t * 'bt annot
  | Good of Sctypes.t * 'bt annot
  | Aligned of
      { t : 'bt annot;
        align : 'bt annot
      }
  | WrapI of Sctypes.IntegerTypes.t * 'bt annot
  | MapConst of BaseTypes.t * 'bt annot
  | MapSet of 'bt annot * 'bt annot * 'bt annot
  | MapGet of 'bt annot * 'bt annot
  | MapDef of (Sym.t * BaseTypes.t) * 'bt annot
  | Apply of Sym.t * 'bt annot list
  | Let of (Sym.t * 'bt annot) * 'bt annot
  | Match of 'bt annot * ('bt pattern * 'bt annot) list
  | Cast of BaseTypes.t * 'bt annot
  | CN_None of BaseTypes.t
  | CN_Some of 'bt annot
  | IsSome of 'bt annot
  | GetOpt of 'bt annot

and 'bt annot =
  | IT of 'bt term * 'bt * (Locations.t[@equal fun _ _ -> true] [@compare fun _ _ -> 0])
[@@deriving eq, ord, map]

let get_bt : 'bt. 'bt annot -> 'bt = function IT (_, bt, _) -> bt

let get_term (IT (t, _, _)) = t

let get_loc (IT (_, _, l)) = l

let rec pp_pattern (Pat (pat_, _bt, _)) =
  match pat_ with
  | PSym s -> Sym.pp s
  | PWild -> Pp.underscore
  | PConstructor (c, args) ->
    let open Pp in
    Sym.pp c
    ^^^ braces
          (separate_map
             (comma ^^ space)
             (fun (id, pat) -> Id.pp id ^^ colon ^^^ pp_pattern pat)
             args)


(* Precedences:
   Reference: https://en.cppreference.com/w/c/language/operator_precedence
   The numbers we use are `16 - p`, where `p` is the precedence in the table.
   We do this so bigger numbers are higher precedence.

   Highest

   15: . [] ->         selectors
   14: ! - ~ & cast    unary
   13: * / %
   12: + -
   10: < <= > >=
   9: == !=
   8: &
   7: ^
   6: |
   5: &&
   4: ||
   3: ? :              ternary

   Lowest
*)

let pp
  : 'bt 'a.
  ?prec:int -> ?f:('bt annot -> Pp.document -> Pp.document) -> 'bt annot -> Pp.document
  =
  fun ?(prec = 0) ?(f = fun _ x -> x) ->
  let rec aux prec (IT (it, _, _)) =
    let aux prec x = f x (aux prec x) in
    let open Pp in
    (* Without the `lparen` inside `nest 2`, the printed `rparen` is indented by 2 (wrt to
       the lparen). I don't quite understand it, but it works. *)
    let parens pped =
      group ((nest 2 @@ lparen ^^ break 0 ^^ pped) ^^ break 0 ^^ rparen)
    in
    let braces pped =
      group ((nest 2 @@ lbrace ^^ break 0 ^^ pped) ^^ break 0 ^^ rbrace)
    in
    let wrap_after tgt doc = if prec > tgt then parens doc else doc in
    let break_op x = break 1 ^^ x ^^ space in
    let alloc_id i = !^("@" ^ Z.to_string i) in
    let opt_alloc_id i = Option.fold ~none:!^"@empty" ~some:alloc_id i in
    match it with
    | Const const ->
      (match const with
       | Z i -> !^(Z.to_string i)
       | Bits ((sign, n), v) ->
         let dec =
           !^(Z.to_string v)
           ^^ (match sign with Unsigned -> !^"'u" | Signed -> !^"'i")
           ^^ !^(string_of_int n)
         in
         if Z.lt v (Z.of_int 16) then
           dec
         else
           dec
           ^^^ !^"/*"
           ^^^ !^("0x" ^ Z.format "%x" (BaseTypes.normalise_to_range (Unsigned, n) v))
           ^^^ !^"*/"
       | Q q -> !^(Q.to_string q)
       | MemByte { alloc_id = id; value } ->
         braces (opt_alloc_id id ^^ semi ^^ space ^^ !^("0x" ^ Z.format "%x" value))
       | Pointer { alloc_id = id; addr } ->
         braces (alloc_id id ^^ semi ^^ space ^^ !^("0x" ^ Z.format "%x" addr))
       | Alloc_id i -> !^("@" ^ Z.to_string i)
       | Bool true -> !^"true"
       | Bool false -> !^"false"
       | Unit -> !^"void"
       | Default bt -> c_app !^"default" [ BaseTypes.pp bt ]
       | Null -> !^"NULL"
       | CType_const ct -> squotes (Sctypes.pp ct))
    | Sym sym -> Sym.pp sym
    | Unop (uop, it1) ->
      let prefix x op p = wrap_after x (!^op ^^ aux p it1) in
      (match uop with
       | BW_CLZ_NoSMT -> c_app !^"bw_clz_uf" [ aux 0 it1 ]
       | BW_CTZ_NoSMT -> c_app !^"bw_ctz_uf" [ aux 0 it1 ]
       | BW_FFS_NoSMT -> c_app !^"bw_ffs_uf" [ aux 0 it1 ]
       | BW_FLS_NoSMT -> c_app !^"bw_fls_uf" [ aux 0 it1 ]
       | Not ->
         let infix p op l r =
           wrap_after p (flow (break 1 ^^ op ^^ space) [ aux p l; aux p r ])
         in
         (match it1 with
          | IT (Binop (EQ, l, r), _, _) -> infix 9 (!^"!" ^^ equals) l r
          | IT (Binop (LT, l, r), _, _) -> infix 10 (rangle () ^^ equals) l r
          | IT (Binop (LE, l, r), _, _) -> infix 10 (rangle ()) l r
          | IT (Binop (LTPointer, l, r), _, _) -> infix 10 (rangle () ^^ equals) l r
          | IT (Binop (LEPointer, l, r), _, _) -> infix 10 (rangle ()) l r
          | _ -> prefix 14 "!" 14)
       | Negate -> prefix 14 "-" 14
       | BW_Compl -> prefix 14 "~" 14)
    | Binop (bop, it1, it2) ->
      let infix x op l r = wrap_after x (flow (break_op op) [ aux l it1; aux r it2 ]) in
      let prefix x = c_app !^x [ aux 0 it1; aux 0 it2 ] in
      (match bop with
       | And -> infix 5 (ampersand ^^ ampersand) 5 5
       | Or -> infix 4 (bar ^^ bar) 4 4
       | Implies -> prefix "implies"
       | Add -> infix 12 !^"+" 12 12
       | Sub -> infix 12 !^"-" 12 13
       | Mul -> infix 13 !^"*" 13 13
       | MulNoSMT -> prefix "mul_uf"
       | Div -> infix 13 slash 14 14
       | DivNoSMT -> prefix "div_uf"
       | Exp -> prefix "power"
       | ExpNoSMT -> prefix "power_uf"
       | Rem -> infix 13 !^"%" 14 14
       | RemNoSMT -> prefix "rem_uf"
       | Mod -> prefix "mod"
       | ModNoSMT -> prefix "mod_uf"
       | EQ -> infix 9 (equals ^^ equals) 9 9
       | LT -> infix 10 (langle ()) 10 10
       | LE -> infix 10 (langle () ^^ equals) 10 10
       | LTPointer -> infix 10 (langle ()) 10 10
       | LEPointer -> infix 10 (langle () ^^ equals) 10 10
       | Min -> prefix "min"
       | Max -> prefix "max"
       | BW_Xor -> infix 7 !^"^" 7 7
       | BW_And -> infix 8 ampersand 8 8
       | BW_Or -> infix 6 bar 6 6
       | ShiftLeft ->
         infix 0 (langle () ^^ langle ()) 1 1 (* easier to read with parens *)
       | ShiftRight ->
         infix 0 (rangle () ^^ rangle ()) 1 1 (* easier to read with parens *)
       | SetMember -> prefix "member"
       | SetUnion -> prefix "union"
       | SetIntersection -> prefix "inter"
       | SetDifference -> prefix "difference"
       | Subset -> prefix "subset")
    | ITE (o1, o2, o3) ->
      wrap_after 2 (flow (break 1) [ aux 3 o1; !^"?"; aux 3 o2; colon; aux 3 o3 ])
    | EachI ((i1, (s, _), i2), t) ->
      group
      @@ group (c_app !^"for" [ int i1; Sym.pp s; int i2 ])
      ^/^ group ((nest 2 @@ lbrace ^^ break 0 ^^ aux 0 t) ^^ break 0 ^^ rbrace)
    | NthTuple (n, it2) ->
      wrap_after 15 (aux 15 it2 ^^ dot ^^ !^("member" ^ string_of_int n))
    | Tuple its -> braces (separate_map (semi ^^ space) (aux 0) its)
    | Struct (_tag, members) ->
      lbrace
      ^^ hardline
      ^^ flow_map
           (comma ^^ hardline)
           (fun (member, it) ->
              group @@ (group @@ dot ^^ Id.pp member ^^^ equals) ^^^ align (aux 0 it))
           members
      ^^^ rbrace
    | StructMember (t, member) -> wrap_after 15 (aux 15 t ^^ dot ^^ Id.pp member)
    | StructUpdate ((t, member), v) ->
      braces (dot ^^ Id.pp member ^^ colon ^^^ aux 0 v ^^ comma ^^^ !^".." ^^ aux 0 t)
    | Record members ->
      align
      @@ lbrace
      ^^^ flow_map
            (break 0 ^^ comma ^^ space)
            (fun (member, it) ->
               group @@ (group @@ dot ^^ Id.pp member ^^^ equals) ^^^ align (aux 0 it))
            members
      ^^^ rbrace
    | RecordMember (t, member) -> wrap_after 15 (aux 15 t ^^ dot ^^ Id.pp member)
    | RecordUpdate ((t, member), v) ->
      braces (dot ^^ Id.pp member ^^ colon ^^^ aux 0 v ^^ comma ^^^ !^".." ^^ aux 0 t)
    | Cast (cbt, t) -> wrap_after 14 (align @@ parens (BaseTypes.pp cbt) ^^ aux 14 t)
    | MemberShift (t, _tag, member) ->
      wrap_after 14 (ampersand ^^ aux 15 t ^^ (!^"-" ^^ rangle ()) ^^ Id.pp member)
    | ArrayShift { base; ct = _ct; index } ->
      wrap_after 14 (ampersand ^^ aux 15 base ^^ brackets (aux 0 index))
    | CopyAllocId { addr; loc } -> c_app !^"copy_alloc_id" [ aux 0 addr; aux 0 loc ]
    | HasAllocId loc -> c_app !^"has_alloc_id" [ aux 0 loc ]
    | SizeOf t -> c_app !^"sizeof" [ Sctypes.pp t ]
    | OffsetOf (tag, member) -> c_app !^"offsetof" [ Sym.pp tag; Id.pp member ]
    | Aligned t -> c_app !^"aligned" [ aux 0 t.t; aux 0 t.align ]
    | Representable (rt, t) -> c_app (!^"repr" ^^ angles (Sctypes.pp rt)) [ aux 0 t ]
    | Good (rt, t) -> c_app (!^"good" ^^ angles (Sctypes.pp rt)) [ aux 0 t ]
    | WrapI (ity, t) -> c_app (!^"wrapI" ^^ angles (Sctypes.pp (Integer ity))) [ aux 0 t ]
    | Head o1 -> c_app !^"hd" [ aux 0 o1 ]
    | Tail o1 -> c_app !^"tl" [ aux 0 o1 ]
    | Nil bt -> !^"nil" ^^ angles (BaseTypes.pp bt)
    | Cons (t1, t2) -> c_app !^"cons" [ aux 0 t1; aux 0 t2 ]
    | MapConst (_bt, t) -> c_app !^"const" [ aux 0 t ]
    | MapGet (t1, t2) -> wrap_after 15 (aux 15 t1 ^^ brackets (aux 0 t2))
    | MapSet (t1, t2, t3) ->
      wrap_after 15 (aux 15 t1 ^^ brackets (aux 0 t2 ^^^ equals ^^^ aux 0 t3))
    | MapDef ((s, _), t) -> brackets (Sym.pp s ^^^ (!^"-" ^^ rangle ()) ^^^ aux 0 t)
    | Apply (name, args) -> c_app (Sym.pp name) (List.map (aux 0) args)
    | Let ((name, x1), x2) ->
      parens (!^"let" ^^^ Sym.pp name ^^^ equals ^^^ aux 0 x1 ^^^ !^"in" ^^^ aux 0 x2)
    | Match (e, cases) ->
      !^"match"
      ^^^ aux 0 e
      ^^^ braces
            ((* copying from mparens *)
             group
               (nest 2
                @@ separate_map
                     (break 0)
                     (fun (pattern, body) ->
                        pp_pattern pattern ^^^ !^"=>" ^^^ braces (aux 0 body))
                     cases))
    | Constructor (s, args) ->
      Sym.pp s
      ^^^ braces
            (separate_map
               (comma ^^ space)
               (fun (id, e) -> Id.pp id ^^ colon ^^^ aux 0 e)
               args)
    | CN_None bt -> c_app !^"None" [ BaseTypes.pp bt ]
    | CN_Some t -> c_app !^"Some" [ aux 0 t ]
    | IsSome t -> c_app !^"is_some" [ aux 0 t ]
    | GetOpt t -> c_app !^"get_opt" [ aux 0 t ]
  in
  fun (it : 'bt annot) -> aux prec it


let rec dtree_of_pat (Pat (pat_, _bt, _)) =
  let open Cerb_frontend.Pp_ast in
  match pat_ with
  | PSym s -> Dnode (pp_ctor "PSym", [ Dleaf (Sym.pp s) ])
  | PWild -> Dleaf (pp_ctor "PWild")
  | PConstructor (s, pats) ->
    Dnode
      ( pp_ctor "PConstructor",
        Dleaf (Sym.pp s)
        :: List.map
             (fun (id, pat) ->
                Dnode (pp_ctor "Arg", [ Dleaf (Id.pp id); dtree_of_pat pat ]))
             pats )


let rec dtree (IT (it_, bt, loc)) =
  let open Cerb_frontend.Pp_ast in
  let open Pp.Infix in
  let alloc_id z = Dnode (pp_ctor "alloc_id", [ Dleaf !^(Z.to_string z) ]) in
  let opt_alloc_id z =
    let none = Dnode (pp_ctor "alloc_id", [ Dleaf !^"Empty" ]) in
    Option.fold ~none ~some:alloc_id z
  in
  let dtree =
    match it_ with
    | Sym s -> Dleaf (Sym.pp s)
    | Const const ->
      (match const with
       | Z z -> Dleaf !^(Z.to_string z)
       | Bits _ -> Dleaf (pp (IT (it_, bt, loc)))
       | Q q -> Dleaf !^(Q.to_string q)
       | MemByte { alloc_id = id; value } ->
         Dnode (pp_ctor "mem_byte", [ opt_alloc_id id; Dleaf !^(Z.to_string value) ])
       | Pointer { alloc_id = id; addr } ->
         Dnode (pp_ctor "pointer", [ alloc_id id; Dleaf !^(Z.to_string addr) ])
       | Bool b -> Dleaf !^(if b then "true" else "false")
       | Unit -> Dleaf !^"unit"
       | Default _ -> Dleaf !^"default"
       | Null -> Dleaf !^"null"
       | Alloc_id z -> alloc_id z
       | CType_const ct -> Dleaf (Sctypes.pp ct))
    | Unop (op, t1) -> Dnode (pp_ctor (show_unop op), [ dtree t1 ])
    | Binop (op, t1, t2) -> Dnode (pp_ctor (show_binop op), [ dtree t1; dtree t2 ])
    | ITE (t1, t2, t3) -> Dnode (pp_ctor "Implies", [ dtree t1; dtree t2; dtree t3 ])
    | EachI ((starti, (i, _), endi), body) ->
      Dnode
        ( pp_ctor "EachI",
          [ Dleaf !^(string_of_int starti);
            Dleaf (Sym.pp i);
            Dleaf !^(string_of_int endi);
            dtree body
          ] )
    | Tuple its -> Dnode (pp_ctor "Tuple", List.map dtree its)
    | NthTuple (i, t) -> Dnode (pp_ctor "NthTuple", [ Dleaf !^(string_of_int i); dtree t ])
    | Struct (tag, members) ->
      Dnode
        ( pp_ctor ("Struct(" ^ Sym.pp_string tag ^ ")"),
          List.map
            (fun (member, e) ->
               Dnode (pp_ctor "Member", [ Dleaf (Id.pp member); dtree e ]))
            members )
    | StructMember (e, member) ->
      Dnode (pp_ctor "StructMember", [ dtree e; Dleaf (Id.pp member) ])
    | StructUpdate ((base, member), v) ->
      Dnode (pp_ctor "StructUpdate", [ dtree base; Dleaf (Id.pp member); dtree v ])
    | Record members ->
      Dnode
        ( pp_ctor "Record",
          List.map
            (fun (member, e) ->
               Dnode (pp_ctor "Member", [ Dleaf (Id.pp member); dtree e ]))
            members )
    | RecordMember (e, member) ->
      Dnode (pp_ctor "RecordMember", [ dtree e; Dleaf (Id.pp member) ])
    | RecordUpdate ((base, member), v) ->
      Dnode (pp_ctor "RecordUpdate", [ dtree base; Dleaf (Id.pp member); dtree v ])
    | Cast (cbt, t) -> Dnode (pp_ctor "Cast", [ Dleaf (BaseTypes.pp cbt); dtree t ])
    | MemberShift (t, tag, id) ->
      Dnode (pp_ctor "MemberShift", [ dtree t; Dleaf (Sym.pp tag); Dleaf (Id.pp id) ])
    | ArrayShift { base; ct = ty; index = t } ->
      Dnode (pp_ctor "ArrayShift", [ Dleaf (Sctypes.pp ty); dtree base; dtree t ])
    | CopyAllocId { addr; loc } -> Dnode (pp_ctor "CopyAllocId", [ dtree addr; dtree loc ])
    | HasAllocId loc -> Dnode (pp_ctor "HasAllocId", [ dtree loc ])
    | Representable (ty, t) ->
      Dnode (pp_ctor "Representable", [ Dleaf (Sctypes.pp ty); dtree t ])
    | Good (ty, t) -> Dnode (pp_ctor "Good", [ Dleaf (Sctypes.pp ty); dtree t ])
    | Aligned a -> Dnode (pp_ctor "Aligned", [ dtree a.t; dtree a.align ])
    | MapConst (_bt, t) -> Dnode (pp_ctor "MapConst", [ dtree t ])
    | MapSet (t1, t2, t3) -> Dnode (pp_ctor "MapSet", [ dtree t1; dtree t2; dtree t3 ])
    | MapGet (t1, t2) -> Dnode (pp_ctor "MapGet", [ dtree t1; dtree t2 ])
    | MapDef ((s, _bt), t) -> Dnode (pp_ctor "MapDef", [ Dleaf (Sym.pp s); dtree t ])
    | Apply (f, args) -> Dnode (pp_ctor "Apply", Dleaf (Sym.pp f) :: List.map dtree args)
    | Constructor (s, args) ->
      Dnode
        ( pp_ctor "Constructor",
          Dleaf (Sym.pp s)
          :: List.map
               (fun (id, t) -> Dnode (pp_ctor "Arg", [ Dleaf (Id.pp id); dtree t ]))
               args )
    | Match (t, pats) ->
      Dnode
        ( pp_ctor "Match",
          dtree t
          :: List.map
               (fun (pat, body) ->
                  Dnode (pp_ctor "Case", [ dtree_of_pat pat; dtree body ]))
               pats )
    | Nil bt -> Dleaf (!^"Nil" ^^ Pp.angles (BaseTypes.pp bt))
    | Cons (t1, t2) -> Dnode (pp_ctor "Cons", [ dtree t1; dtree t2 ])
    | Head t -> Dnode (pp_ctor "Head", [ dtree t ])
    | Tail t -> Dnode (pp_ctor "Tail", [ dtree t ])
    | WrapI (it, t) ->
      Dnode (pp_ctor "WrapI", [ Dleaf (Sctypes.pp (Integer it)); dtree t ])
    | SizeOf ct -> Dnode (pp_ctor "SizeOf", [ Dleaf (Sctypes.pp ct) ])
    | OffsetOf (tag, member) ->
      Dnode (pp_ctor "OffsetOf", [ Dleaf (Sym.pp tag); Dleaf (Id.pp member) ])
    | Let ((s, t1), t2) -> Dnode (pp_ctor "Let", [ Dleaf (Sym.pp s); dtree t1; dtree t2 ])
    | CN_None _ -> Dleaf !^"None"
    | CN_Some it -> Dnode (pp_ctor "Some", [ dtree it ])
    | IsSome it -> Dnode (pp_ctor "IsSome", [ dtree it ])
    | GetOpt it -> Dnode (pp_ctor "GetOpt", [ dtree it ])
  in
  let loc_doc = Pp.parens !^(Locations.to_string loc) in
  match dtree with
  | Dnode (doc, dtrees) -> Dnode (doc ^^^ loc_doc, dtrees)
  | Dleaf doc -> Dleaf (doc ^^^ loc_doc)
  | _ -> assert false


let rec bound_by_pattern (Pat (pat_, bt, _)) =
  match pat_ with
  | PSym s -> [ (s, bt) ]
  | PWild -> []
  | PConstructor (_s, args) ->
    List.concat_map (fun (_id, pat) -> bound_by_pattern pat) args


let rec free_vars_bts bt_equals (it : 'a annot) : 'bt Sym.Map.t =
  match get_term it with
  | Const _ -> Sym.Map.empty
  | Sym s -> Sym.Map.singleton s (get_bt it)
  | Unop (_uop, t1) -> free_vars_bts bt_equals t1
  | Binop (_bop, t1, t2) -> free_vars_bts_list bt_equals [ t1; t2 ]
  | ITE (t1, t2, t3) -> free_vars_bts_list bt_equals [ t1; t2; t3 ]
  | EachI ((_, (s, _), _), t) -> Sym.Map.remove s (free_vars_bts bt_equals t)
  | Tuple ts -> free_vars_bts_list bt_equals ts
  | NthTuple (_, t) -> free_vars_bts bt_equals t
  | Struct (_tag, members) -> free_vars_bts_list bt_equals (List.map snd members)
  | StructMember (t, _member) -> free_vars_bts bt_equals t
  | StructUpdate ((t1, _member), t2) -> free_vars_bts_list bt_equals [ t1; t2 ]
  | Record members -> free_vars_bts_list bt_equals (List.map snd members)
  | RecordMember (t, _member) -> free_vars_bts bt_equals t
  | RecordUpdate ((t1, _member), t2) -> free_vars_bts_list bt_equals [ t1; t2 ]
  | Cast (_cbt, t) -> free_vars_bts bt_equals t
  | MemberShift (t, _tag, _id) -> free_vars_bts bt_equals t
  | ArrayShift { base; ct = _; index } -> free_vars_bts_list bt_equals [ base; index ]
  | CopyAllocId { addr; loc } -> free_vars_bts_list bt_equals [ addr; loc ]
  | HasAllocId loc -> free_vars_bts_list bt_equals [ loc ]
  | SizeOf _t -> Sym.Map.empty
  | OffsetOf (_tag, _member) -> Sym.Map.empty
  | Nil _bt -> Sym.Map.empty
  | Cons (t1, t2) -> free_vars_bts_list bt_equals [ t1; t2 ]
  | Head t -> free_vars_bts bt_equals t
  | Tail t -> free_vars_bts bt_equals t
  | Representable (_sct, t) -> free_vars_bts bt_equals t
  | Good (_sct, t) -> free_vars_bts bt_equals t
  | WrapI (_ity, t) -> free_vars_bts bt_equals t
  | Aligned { t; align } -> free_vars_bts_list bt_equals [ t; align ]
  | MapConst (_bt, t) -> free_vars_bts bt_equals t
  | MapSet (t1, t2, t3) -> free_vars_bts_list bt_equals [ t1; t2; t3 ]
  | MapGet (t1, t2) -> free_vars_bts_list bt_equals [ t1; t2 ]
  | MapDef ((s, _bt), t) -> Sym.Map.remove s (free_vars_bts bt_equals t)
  | Apply (_pred, ts) -> free_vars_bts_list bt_equals ts
  | Let ((nm, t1), t2) ->
    Sym.Map.union
      (fun _ bt1 bt2 ->
         assert (bt_equals bt1 bt2);
         Some bt1)
      (free_vars_bts bt_equals t1)
      (Sym.Map.remove nm (free_vars_bts bt_equals t2))
  | Match (e, cases) ->
    let rec aux acc = function
      | [] -> acc
      | (pat, body) :: cases ->
        let bound = Sym.Set.of_list (List.map fst (bound_by_pattern pat)) in
        let more =
          Sym.Map.filter
            (fun x _ -> not (Sym.Set.mem x bound))
            (free_vars_bts bt_equals body)
        in
        aux
          (Sym.Map.union
             (fun _ bt1 bt2 ->
                assert (bt_equals bt1 bt2);
                Some bt1)
             more
             acc)
          cases
    in
    aux (free_vars_bts bt_equals e) cases
  | Constructor (_s, args) -> free_vars_bts_list bt_equals (List.map snd args)
  | CN_None _ -> Sym.Map.empty
  | CN_Some t -> free_vars_bts bt_equals t
  | IsSome t -> free_vars_bts bt_equals t
  | GetOpt t -> free_vars_bts bt_equals t


and free_vars_bts_list bt_equals : 'a annot list -> 'bt Sym.Map.t =
  fun xs ->
  List.fold_left
    (fun ss t ->
       Sym.Map.union
         (fun _ bt1 bt2 ->
            assert (bt_equals bt1 bt2);
            Some bt1)
         ss
         (free_vars_bts bt_equals t))
    Sym.Map.empty
    xs


let free_vars bt_equals (it : 'a annot) : Sym.Set.t =
  it |> free_vars_bts bt_equals |> Sym.Map.bindings |> List.map fst |> Sym.Set.of_list


let free_vars_list bt_equals (its : 'a annot list) : Sym.Set.t =
  its
  |> free_vars_bts_list bt_equals
  |> Sym.Map.bindings
  |> List.map fst
  |> Sym.Set.of_list


let free_vars_with_rename
      (bt_equal : 'bt -> 'bt -> bool)
      (t_or_rename : [ `Term of 'bt annot | `Rename of Sym.t ])
  =
  match t_or_rename with
  | `Term t -> free_vars bt_equal t
  | `Rename s -> Sym.Set.singleton s


let make_rename bt_equal ~from ~to_ =
  Subst.make (free_vars_with_rename bt_equal) [ (from, `Rename to_) ]


let make_subst bt_equal assoc =
  Subst.make
    (free_vars_with_rename bt_equal)
    (List.map (fun (s, t) -> (s, `Term t)) assoc)


let rec subst
          bteq
          (su : [ `Term of 'bt annot | `Rename of Sym.t ] Subst.t)
          (IT (it, bt, loc))
  =
  match it with
  | Sym sym ->
    (match List.assoc_opt Sym.equal sym su.replace with
     | Some (`Term after) ->
       if bteq bt (get_bt after) then
         ()
       else
         failwith
           ("ill-typed substitution: " ^ Pp.plain (Pp.list pp [ IT (it, bt, loc); after ]));
       after
     | Some (`Rename sym) -> IT (Sym sym, bt, loc)
     | None -> IT (Sym sym, bt, loc))
  | Const const -> IT (Const const, bt, loc)
  | Unop (uop, it) -> IT (Unop (uop, subst bteq su it), bt, loc)
  | Binop (bop, t1, t2) -> IT (Binop (bop, subst bteq su t1, subst bteq su t2), bt, loc)
  | ITE (it, it', it'') ->
    IT (ITE (subst bteq su it, subst bteq su it', subst bteq su it''), bt, loc)
  | EachI ((i1, (s, s_bt), i2), t) ->
    let s, t = suitably_alpha_rename bteq su.relevant s t in
    IT (EachI ((i1, (s, s_bt), i2), subst bteq su t), bt, loc)
  | Tuple its -> IT (Tuple (List.map (subst bteq su) its), bt, loc)
  | NthTuple (n, it') -> IT (NthTuple (n, subst bteq su it'), bt, loc)
  | Struct (tag, members) ->
    IT (Struct (tag, List.map_snd (subst bteq su) members), bt, loc)
  | StructMember (t, m) -> IT (StructMember (subst bteq su t, m), bt, loc)
  | StructUpdate ((t, m), v) ->
    IT (StructUpdate ((subst bteq su t, m), subst bteq su v), bt, loc)
  | Record members -> IT (Record (List.map_snd (subst bteq su) members), bt, loc)
  | RecordMember (t, m) -> IT (RecordMember (subst bteq su t, m), bt, loc)
  | RecordUpdate ((t, m), v) ->
    IT (RecordUpdate ((subst bteq su t, m), subst bteq su v), bt, loc)
  | Cast (cbt, t) -> IT (Cast (cbt, subst bteq su t), bt, loc)
  | MemberShift (t, tag, member) ->
    IT (MemberShift (subst bteq su t, tag, member), bt, loc)
  | ArrayShift { base; ct; index } ->
    IT (ArrayShift { base = subst bteq su base; ct; index = subst bteq su index }, bt, loc)
  | CopyAllocId { addr; loc = ptr } ->
    IT (CopyAllocId { addr = subst bteq su addr; loc = subst bteq su ptr }, bt, loc)
  | HasAllocId ptr -> IT (HasAllocId (subst bteq su ptr), bt, loc)
  | SizeOf t -> IT (SizeOf t, bt, loc)
  | OffsetOf (tag, member) -> IT (OffsetOf (tag, member), bt, loc)
  | Aligned t ->
    IT (Aligned { t = subst bteq su t.t; align = subst bteq su t.align }, bt, loc)
  | Representable (rt, t) -> IT (Representable (rt, subst bteq su t), bt, loc)
  | Good (rt, t) -> IT (Good (rt, subst bteq su t), bt, loc)
  | WrapI (ity, t) -> IT (WrapI (ity, subst bteq su t), bt, loc)
  | Nil bt' -> IT (Nil bt', bt, loc)
  | Cons (it1, it2) -> IT (Cons (subst bteq su it1, subst bteq su it2), bt, loc)
  | Head it -> IT (Head (subst bteq su it), bt, loc)
  | Tail it -> IT (Tail (subst bteq su it), bt, loc)
  | MapConst (arg_bt, t) -> IT (MapConst (arg_bt, subst bteq su t), bt, loc)
  | MapSet (t1, t2, t3) ->
    IT (MapSet (subst bteq su t1, subst bteq su t2, subst bteq su t3), bt, loc)
  | MapGet (it, arg) -> IT (MapGet (subst bteq su it, subst bteq su arg), bt, loc)
  | MapDef ((s, abt), body) ->
    let s, body = suitably_alpha_rename bteq su.relevant s body in
    IT (MapDef ((s, abt), subst bteq su body), bt, loc)
  | Apply (name, args) -> IT (Apply (name, List.map (subst bteq su) args), bt, loc)
  | Let ((name, t1), t2) ->
    let name, t2 = suitably_alpha_rename bteq su.relevant name t2 in
    IT (Let ((name, subst bteq su t1), subst bteq su t2), bt, loc)
  | Match (e, cases) ->
    let e = subst bteq su e in
    let cases = List.map (subst_under_pattern bteq su) cases in
    IT (Match (e, cases), bt, loc)
  | Constructor (s, args) ->
    let args = List.map (fun (id, e) -> (id, subst bteq su e)) args in
    IT (Constructor (s, args), bt, loc)
  | CN_None bt -> IT (CN_None bt, bt, loc)
  | CN_Some it -> IT (CN_Some (subst bteq su it), bt, loc)
  | IsSome it -> IT (IsSome (subst bteq su it), bt, loc)
  | GetOpt it -> IT (GetOpt (subst bteq su it), bt, loc)


and alpha_rename bteq s body =
  let s' = Sym.fresh_same s in
  (s', subst bteq (make_rename bteq ~from:s ~to_:s') body)


and suitably_alpha_rename bteq syms s body =
  if Sym.Set.mem s syms then
    alpha_rename bteq s body
  else
    (s, body)


and subst_under_pattern bteq su (pat, body) =
  let pat, body = suitably_alpha_rename_pattern bteq su (pat, body) in
  (pat, subst bteq su body)


and suitably_alpha_rename_pattern bteq su (Pat (pat_, bt, loc), body) =
  match pat_ with
  | PSym s ->
    let s, body = suitably_alpha_rename bteq su.relevant s body in
    (Pat (PSym s, bt, loc), body)
  | PWild -> (Pat (PWild, bt, loc), body)
  | PConstructor (s, args) ->
    let body, args =
      List.fold_left_map
        (fun body (id, pat') ->
           let pat', body = suitably_alpha_rename_pattern bteq su (pat', body) in
           (body, (id, pat')))
        body
        args
    in
    (Pat (PConstructor (s, args), bt, loc), body)


type 'bt bindings = ('bt pattern * 'bt annot option) list

let rec fold_ f binders acc = function
  | Sym _s -> acc
  | Const _c -> acc
  | Unop (_uop, t1) -> fold f binders acc t1
  | Binop (_bop, t1, t2) -> fold_list f binders acc [ t1; t2 ]
  | ITE (t1, t2, t3) -> fold_list f binders acc [ t1; t2; t3 ]
  | EachI ((_, (s, bt), _), t) ->
    (* TODO - add location information to binders *)
    let here = Locations.other __LOC__ in
    fold f (binders @ [ (Pat (PSym s, bt, here), None) ]) acc t
  | Tuple ts -> fold_list f binders acc ts
  | NthTuple (_, t) -> fold f binders acc t
  | Struct (_tag, members) -> fold_list f binders acc (List.map snd members)
  | StructMember (t, _member) -> fold f binders acc t
  | StructUpdate ((t1, _member), t2) -> fold_list f binders acc [ t1; t2 ]
  | Record members -> fold_list f binders acc (List.map snd members)
  | RecordMember (t, _member) -> fold f binders acc t
  | RecordUpdate ((t1, _member), t2) -> fold_list f binders acc [ t1; t2 ]
  | Cast (_cbt, t) -> fold f binders acc t
  | MemberShift (t, _tag, _id) -> fold f binders acc t
  | ArrayShift { base; ct = _; index } -> fold_list f binders acc [ base; index ]
  | CopyAllocId { addr; loc } -> fold_list f binders acc [ addr; loc ]
  | HasAllocId loc -> fold_list f binders acc [ loc ]
  | SizeOf _ct -> acc
  | OffsetOf (_tag, _member) -> acc
  | Nil _bt -> acc
  | Cons (t1, t2) -> fold_list f binders acc [ t1; t2 ]
  | Head t -> fold f binders acc t
  | Tail t -> fold f binders acc t
  | Representable (_sct, t) -> fold f binders acc t
  | Good (_sct, t) -> fold f binders acc t
  | WrapI (_ity, t) -> fold f binders acc t
  | Aligned { t; align } -> fold_list f binders acc [ t; align ]
  | MapConst (_bt, t) -> fold f binders acc t
  | MapSet (t1, t2, t3) -> fold_list f binders acc [ t1; t2; t3 ]
  | MapGet (t1, t2) -> fold_list f binders acc [ t1; t2 ]
  | MapDef ((s, bt), t) ->
    (* TODO - add location information to binders *)
    let here = Locations.other __LOC__ in
    fold f (binders @ [ (Pat (PSym s, bt, here), None) ]) acc t
  | Apply (_pred, ts) -> fold_list f binders acc ts
  | Let ((nm, t1), t2) ->
    let acc' = fold f binders acc t1 in
    (* TODO - add location information to binders *)
    let here = Locations.other __LOC__ in
    fold f (binders @ [ (Pat (PSym nm, get_bt t1, here), Some t1) ]) acc' t2
  | Match (e, cases) ->
    (* TODO: check this is good *)
    let acc' = fold f binders acc e in
    let rec aux acc = function
      | [] -> acc
      | (pat, body) :: cases ->
        let acc' = fold f (binders @ [ (pat, Some e) ]) acc body in
        aux acc' cases
    in
    aux acc' cases
  | Constructor (_sym, args) -> fold_list f binders acc (List.map snd args)
  | CN_None _ -> acc
  | CN_Some t -> fold f binders acc t
  | IsSome t -> fold f binders acc t
  | GetOpt t -> fold f binders acc t


and fold f binders acc (IT (term_, _bt, loc)) =
  let acc' = fold_ f binders acc term_ in
  f binders acc' (IT (term_, _bt, loc))


and fold_list f binders acc xs =
  match xs with
  | [] -> acc
  | x :: xs ->
    let acc' = fold f binders acc x in
    fold_list f binders acc' xs


let fold_subterms
  : 'a.
  ?bindings:'bt bindings ->
  ('bt bindings -> 'a -> 'bt annot -> 'a) ->
  'a ->
  'bt annot ->
  'a
  =
  fun ?(bindings = []) f acc t -> fold f bindings acc t


let rec map_term_pre (f : 'bt annot -> 'bt annot) (it : 'bt annot) : 'bt annot =
  let (IT (it_, bt, here)) = f it in
  let loop = map_term_pre f in
  let it_ =
    match it_ with
    | Const _ | Sym _ | Nil _ | SizeOf _ | OffsetOf _ -> it_
    | Unop (op, it') -> Unop (op, loop it')
    | Binop (op, it1, it2) -> Binop (op, loop it1, loop it2)
    | ITE (it_if, it_then, it_else) -> ITE (loop it_if, loop it_then, loop it_else)
    | EachI (range, it') -> EachI (range, loop it')
    | Tuple its -> Tuple (List.map loop its)
    | NthTuple (i, it') -> NthTuple (i, loop it')
    | Struct (tag, xits) -> Struct (tag, List.map_snd loop xits)
    | StructMember (it', member) -> StructMember (loop it', member)
    | StructUpdate ((it_struct, member), it_value) ->
      StructUpdate ((loop it_struct, member), loop it_value)
    | Record xits -> Record (List.map_snd loop xits)
    | RecordMember (it', member) -> RecordMember (loop it', member)
    | RecordUpdate ((it_struct, member), it_value) ->
      RecordUpdate ((loop it_struct, member), loop it_value)
    | Constructor (constr, xits) -> Constructor (constr, List.map_snd loop xits)
    | MemberShift (it', tag, member) -> MemberShift (loop it', tag, member)
    | ArrayShift { base; ct; index } ->
      ArrayShift { base = loop base; ct; index = loop index }
    | CopyAllocId { addr; loc } -> CopyAllocId { addr = loop addr; loc = loop loc }
    | Cons (it_head, it_tail) -> Cons (loop it_head, loop it_tail)
    | Head it' -> Head (loop it')
    | Tail it' -> Tail (loop it')
    | Representable (ct, it') -> Representable (ct, loop it')
    | Good (ct, it') -> Good (ct, loop it')
    | Aligned { t; align } -> Aligned { t = loop t; align = loop align }
    | WrapI (ct, it') -> WrapI (ct, loop it')
    | MapConst (bt', it') -> MapConst (bt', loop it')
    | MapSet (it_m, it_k, it_v) -> MapSet (loop it_m, loop it_k, loop it_v)
    | MapGet (it_m, it_key) -> MapGet (loop it_m, loop it_key)
    | MapDef (sbt, it') -> MapDef (sbt, loop it')
    | Apply (fsym, its) -> Apply (fsym, List.map loop its)
    | Let ((x, it_v), it_rest) -> Let ((x, loop it_v), loop it_rest)
    | Match (it', pits) -> Match (loop it', List.map_snd loop pits)
    | Cast (bt', it') -> Cast (bt', loop it')
    | HasAllocId it' -> HasAllocId (loop it')
    | CN_None bt' -> CN_None bt'
    | CN_Some it' -> CN_Some (loop it')
    | IsSome it' -> IsSome (loop it')
    | GetOpt it' -> GetOpt (loop it')
  in
  IT (it_, bt, here)


let rec map_term_post (f : 'bt annot -> 'bt annot) (it : 'bt annot) : 'bt annot =
  let (IT (it_, bt, here)) = it in
  let loop = map_term_post f in
  let it_ =
    match it_ with
    | Const _ | Sym _ | Nil _ | SizeOf _ | OffsetOf _ -> it_
    | Unop (op, it') -> Unop (op, loop it')
    | Binop (op, it1, it2) -> Binop (op, loop it1, loop it2)
    | ITE (it_if, it_then, it_else) -> ITE (loop it_if, loop it_then, loop it_else)
    | EachI (range, it') -> EachI (range, loop it')
    | Tuple its -> Tuple (List.map loop its)
    | NthTuple (i, it') -> NthTuple (i, loop it')
    | Struct (tag, xits) -> Struct (tag, List.map_snd loop xits)
    | StructMember (it', member) -> StructMember (loop it', member)
    | StructUpdate ((it_struct, member), it_value) ->
      StructUpdate ((loop it_struct, member), loop it_value)
    | Record xits -> Record (List.map_snd loop xits)
    | RecordMember (it', member) -> RecordMember (loop it', member)
    | RecordUpdate ((it_struct, member), it_value) ->
      RecordUpdate ((loop it_struct, member), loop it_value)
    | Constructor (constr, xits) -> Constructor (constr, List.map_snd loop xits)
    | MemberShift (it', tag, member) -> MemberShift (loop it', tag, member)
    | ArrayShift { base; ct; index } ->
      ArrayShift { base = loop base; ct; index = loop index }
    | CopyAllocId { addr; loc } -> CopyAllocId { addr = loop addr; loc = loop loc }
    | HasAllocId it' -> HasAllocId (loop it')
    | Cons (it_head, it_tail) -> Cons (loop it_head, loop it_tail)
    | Head it' -> Head (loop it')
    | Tail it' -> Tail (loop it')
    | Representable (ct, it') -> Representable (ct, loop it')
    | Good (ct, it') -> Good (ct, loop it')
    | Aligned { t; align } -> Aligned { t = loop t; align = loop align }
    | WrapI (ct, it') -> WrapI (ct, loop it')
    | MapConst (bt', it') -> MapConst (bt', loop it')
    | MapSet (it_m, it_k, it_v) -> MapSet (loop it_m, loop it_k, loop it_v)
    | MapGet (it_m, it_key) -> MapGet (loop it_m, loop it_key)
    | MapDef (sbt, it') -> MapDef (sbt, loop it')
    | Apply (fsym, its) -> Apply (fsym, List.map loop its)
    | Let ((x, it_v), it_rest) -> Let ((x, loop it_v), loop it_rest)
    | Match (it', pits) -> Match (loop it', List.map_snd loop pits)
    | Cast (bt', it') -> Cast (bt', loop it')
    | CN_None bt' -> CN_None bt'
    | CN_Some it' -> CN_Some (loop it')
    | IsSome it' -> IsSome (loop it')
    | GetOpt it' -> GetOpt (loop it')
  in
  f (IT (it_, bt, here))
