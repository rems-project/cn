module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module GT = GenTerms
module GD = GenDefinitions.Make (GenTerms)
module GC = GenContext.Make (GenTerms)
module GA = GenAnalysis
module SymGraph = Graph.Persistent.Digraph.Concrete (Sym)
module StringMap = Map.Make (String)

type t =
  | Uniform of { bt : BT.t }
  | Pick of
      { bt : BT.t;
        choices : (Z.t * t) list
      }
  | Alloc
  | Call of
      { fsym : Sym.t;
        iargs : (Sym.t * Sym.t) list;
        oarg_bt : BT.t
      }
  | Asgn of
      { addr : IT.t;
        sct : Sctypes.t;
        value : IT.t;
        rest : t
      }
  | LetStar of
      { x : Sym.t;
        x_bt : BT.t;
        value : t;
        rest : t
      }
  | Return of { value : IT.t }
  | Assert of
      { prop : LC.t;
        rest : t
      }
  | ITE of
      { bt : BT.t;
        cond : IT.t;
        t : t;
        f : t
      }
  | Map of
      { i : Sym.t;
        bt : BT.t;
        perm : IT.t;
        inner : t
      }
[@@deriving eq, ord]

let is_return (tm : t) : bool = match tm with Return _ -> true | _ -> false

let rec free_vars (tm : t) : Sym.Set.t =
  match tm with
  | Uniform _ | Alloc -> Sym.Set.empty
  | Pick { bt = _; choices } -> free_vars_list (List.map snd choices)
  | Call { fsym = _; iargs; oarg_bt = _ } -> Sym.Set.of_list (List.map snd iargs)
  | Asgn { addr; sct = _; value; rest } ->
    Sym.Set.union (IT.free_vars_list [ addr; value ]) (free_vars rest)
  | LetStar { x; x_bt = _; value; rest } ->
    Sym.Set.union (free_vars value) (Sym.Set.remove x (free_vars rest))
  | Return { value } -> IT.free_vars value
  | Assert { prop; rest } -> Sym.Set.union (LC.free_vars prop) (free_vars rest)
  | ITE { bt = _; cond; t; f } ->
    Sym.Set.union (IT.free_vars cond) (free_vars_list [ t; f ])
  | Map { i; bt = _; perm; inner } ->
    Sym.Set.remove i (Sym.Set.union (IT.free_vars_list [ perm ]) (free_vars inner))


and free_vars_list : t list -> Sym.Set.t =
  fun xs -> List.fold_left (fun ss t -> Sym.Set.union ss (free_vars t)) Sym.Set.empty xs


let rec pp (tm : t) : Pp.document =
  let open Pp in
  match tm with
  | Uniform { bt } -> !^"uniform" ^^ angles (BT.pp bt) ^^ parens empty
  | Pick { bt; choices } ->
    !^"pick"
    ^^ parens
         (brackets
            (nest
               2
               (break 1
                ^^ c_comment (BT.pp bt)
                ^/^ separate_map
                      (semi ^^ break 1)
                      (fun (w, gt) ->
                         parens (z w ^^ comma ^^ braces (nest 2 (break 1 ^^ pp gt))))
                      choices)))
  | Alloc -> !^"alloc" ^^ parens empty
  | Call { fsym; iargs; oarg_bt } ->
    parens
      (Sym.pp fsym
       ^^ parens
            (nest
               2
               (separate_map
                  (comma ^^ break 1)
                  (fun (x, y) -> Sym.pp x ^^ colon ^^^ Sym.pp y)
                  iargs))
       ^^^ colon
       ^^^ BT.pp oarg_bt)
  | Asgn { addr; sct; value; rest } ->
    Sctypes.pp sct ^^^ IT.pp addr ^^^ !^":=" ^^^ IT.pp value ^^ semi ^/^ pp rest
  | LetStar { x : Sym.t; x_bt : BT.t; value : t; rest : t } ->
    !^"let*"
    ^^^ Sym.pp x
    ^^^ colon
    ^^^ BT.pp x_bt
    ^^^ equals
    ^^ nest 2 (break 1 ^^ pp value)
    ^^ semi
    ^/^ pp rest
  | Return { value : IT.t } -> !^"return" ^^^ IT.pp value
  | Assert { prop : LC.t; rest : t } ->
    !^"assert" ^^ parens (nest 2 (break 1 ^^ LC.pp prop) ^^ break 1) ^^ semi ^/^ pp rest
  | ITE { bt : BT.t; cond : IT.t; t : t; f : t } ->
    !^"if"
    ^^^ parens (IT.pp cond)
    ^^^ braces (nest 2 (break 1 ^^ c_comment (BT.pp bt) ^/^ pp t) ^^ break 1)
    ^^^ !^"else"
    ^^^ braces (nest 2 (break 1 ^^ pp f) ^^ break 1)
  | Map { i; bt; perm; inner } ->
    let i_bt, _ = BT.map_bt bt in
    !^"map"
    ^^^ parens (BT.pp i_bt ^^^ Sym.pp i ^^ semi ^^^ IT.pp perm)
    ^^ braces (c_comment (BT.pp bt) ^^ nest 2 (break 1 ^^ pp inner) ^^ break 1)


let nice_names (inputs : Sym.Set.t) (gt : GT.t) : GT.t =
  let basename (sym : Sym.t) : string =
    let open Sym in
    match description sym with
    | SD_Id name | SD_CN_Id name | SD_ObjectAddress name | SD_FunArgValue name -> name
    | SD_None -> "fresh"
    | _ -> failwith Pp.(plain (Sym.pp_debug sym ^^^ at ^^^ !^__LOC__))
  in
  let rec aux (vars : int StringMap.t) (gt : GT.t) : int StringMap.t * GT.t =
    let (GT (gt_, _, loc)) = gt in
    match gt_ with
    | Arbitrary | Uniform | Alloc | Call _ | Return _ -> (vars, gt)
    | Pick wgts ->
      let vars, wgts =
        List.fold_right
          (fun (w, gr') (vars', choices') ->
             let vars'', gr'' = aux vars' gr' in
             (vars'', (w, gr'') :: choices'))
          wgts
          (vars, [])
      in
      (vars, GT.pick_ wgts loc)
    | Asgn ((it_addr, sct), it_val, gt') ->
      let vars', gt' = aux vars gt' in
      (vars', GT.asgn_ ((it_addr, sct), it_val, gt') loc)
    | Let (backtracks, (x, gt_inner), gt') ->
      let vars, gt_inner = aux vars gt_inner in
      let name = basename x in
      let vars, x, gt' =
        match StringMap.find_opt name vars with
        | Some n ->
          let name' = name ^ "_" ^ string_of_int n in
          let y = Sym.fresh name' in
          ( StringMap.add name (n + 1) vars,
            y,
            GT.subst
              (IT.make_subst [ (x, IT.sym_ (y, GT.bt gt_inner, GT.loc gt_inner)) ])
              gt' )
        | None -> (StringMap.add name 1 vars, x, gt')
      in
      let vars, gt' = aux vars gt' in
      (vars, GT.let_ (backtracks, (x, gt_inner), gt') loc)
    | Assert (lc, gt') ->
      let vars, gt' = aux vars gt' in
      (vars, GT.assert_ (lc, gt') loc)
    | ITE (it_if, gt_then, gt_else) ->
      let vars, gt_then = aux vars gt_then in
      let vars, gt_else = aux vars gt_else in
      (vars, GT.ite_ (it_if, gt_then, gt_else) loc)
    | Map ((i_sym, i_bt, it_perm), gt_inner) ->
      let vars, gt_inner = aux vars gt_inner in
      let name = basename i_sym in
      let vars, i_sym, it_perm, gt_inner =
        match StringMap.find_opt name vars with
        | Some n ->
          let name' = name ^ "_" ^ string_of_int n in
          let j = Sym.fresh name' in
          let su = IT.make_subst [ (i_sym, IT.sym_ (j, i_bt, loc)) ] in
          (StringMap.add name (n + 1) vars, j, IT.subst su it_perm, GT.subst su gt_inner)
        | None -> (StringMap.add name 1 vars, i_sym, it_perm, gt_inner)
      in
      (vars, GT.map_ ((i_sym, i_bt, it_perm), gt_inner) loc)
  in
  snd
    (aux
       (inputs |> Sym.Set.to_seq |> Seq.map (fun x -> (basename x, 1)) |> StringMap.of_seq)
       gt)
