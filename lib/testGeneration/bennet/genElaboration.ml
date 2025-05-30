module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module GT = GenTerms
module GD = GenDefinitions
module GA = GenAnalysis
module SymGraph = Graph.Persistent.Digraph.Concrete (Sym)
module StringMap = Map.Make (String)

let bennet = Sym.fresh "bennet"

type term =
  | Uniform of
      { bt : BT.t;
        sz : int
      }
  | Pick of
      { bt : BT.t;
        choice_var : Sym.t;
        choices : (int * term) list;
        last_var : Sym.t
      }
  | Alloc of { bytes : IT.t }
  | Call of
      { fsym : Sym.t;
        iargs : (Sym.t * Sym.t) list;
        oarg_bt : BT.t;
        path_vars : Sym.Set.t;
        sized : (int * Sym.t) option
      }
  | Asgn of
      { pointer : Sym.t;
        addr : IT.t;
        sct : Sctypes.t;
        value : IT.t;
        last_var : Sym.t;
        rest : term
      }
  | Let of
      { backtracks : int;
        x : Sym.t;
        x_bt : BT.t;
        value : term;
        last_var : Sym.t;
        rest : term
      }
  | Return of { value : IT.t }
  | Assert of
      { prop : LC.t;
        last_var : Sym.t;
        rest : term
      }
  | ITE of
      { bt : BT.t;
        cond : IT.t;
        t : term;
        f : term
      }
  | Map of
      { i : Sym.t;
        bt : BT.t;
        min : IT.t;
        max : IT.t;
        perm : IT.t;
        inner : term;
        last_var : Sym.t
      }
  | SplitSize of
      { marker_var : Sym.t;
        syms : Sym.Set.t;
        path_vars : Sym.Set.t;
        last_var : Sym.t;
        rest : term
      }
[@@deriving eq, ord]

let is_return (tm : term) : bool = match tm with Return _ -> true | _ -> false

let rec free_vars_term (tm : term) : Sym.Set.t =
  match tm with
  | Uniform _ -> Sym.Set.empty
  | Pick { bt = _; choice_var = _; choices; last_var = _ } ->
    free_vars_term_list (List.map snd choices)
  | Alloc { bytes } -> IT.free_vars bytes
  | Call { fsym = _; iargs; oarg_bt = _; path_vars = _; sized = _ } ->
    Sym.Set.of_list (List.map snd iargs)
  | Asgn { pointer = _; addr; sct = _; value; last_var = _; rest } ->
    Sym.Set.union (IT.free_vars_list [ addr; value ]) (free_vars_term rest)
  | Let { backtracks = _; x; x_bt = _; value; last_var = _; rest } ->
    Sym.Set.union (free_vars_term value) (Sym.Set.remove x (free_vars_term rest))
  | Return { value } -> IT.free_vars value
  | Assert { prop; last_var = _; rest } ->
    Sym.Set.union (LC.free_vars prop) (free_vars_term rest)
  | ITE { bt = _; cond; t; f } ->
    Sym.Set.union (IT.free_vars cond) (free_vars_term_list [ t; f ])
  | Map { i; bt = _; min; max; perm; inner; last_var = _ } ->
    Sym.Set.remove
      i
      (Sym.Set.union (IT.free_vars_list [ min; max; perm ]) (free_vars_term inner))
  | SplitSize { marker_var = _; syms = _; path_vars = _; last_var = _; rest } ->
    free_vars_term rest


and free_vars_term_list : term list -> Sym.Set.t =
  fun xs ->
  List.fold_left (fun ss t -> Sym.Set.union ss (free_vars_term t)) Sym.Set.empty xs


let rec pp_term (tm : term) : Pp.document =
  let open Pp in
  match tm with
  | Uniform { bt; sz } -> !^"uniform" ^^ angles (BT.pp bt) ^^ parens (int sz)
  | Pick { bt; choice_var; choices; last_var } ->
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
                             parens
                               (int w ^^ comma ^^ braces (nest 2 (break 1 ^^ pp_term gt))))
                          choices)))
  | Alloc { bytes } -> !^"alloc" ^^ parens (IT.pp bytes)
  | Call { fsym; iargs; oarg_bt; path_vars; sized } ->
    parens
      (Sym.pp fsym
       ^^ optional (fun (n, sym) -> brackets (int n ^^ comma ^^^ Sym.pp sym)) sized
       ^^ parens
            (nest
               2
               (separate_map
                  (comma ^^ break 1)
                  (fun (x, y) -> Sym.pp x ^^ colon ^^^ Sym.pp y)
                  iargs))
       ^^^ colon
       ^^^ BT.pp oarg_bt
       ^^ c_comment
            (!^"path affected by"
             ^^^ separate_map
                   (comma ^^ space)
                   Sym.pp
                   (path_vars |> Sym.Set.to_seq |> List.of_seq)))
  | Asgn { pointer; addr; sct; value; last_var; rest } ->
    Sctypes.pp sct
    ^^^ IT.pp addr
    ^^^ !^":="
    ^^^ IT.pp value
    ^^ semi
    ^^^ c_comment
          (!^"backtracks to" ^^^ Sym.pp last_var ^^ !^" allocs via " ^^ Sym.pp pointer)
    ^/^ pp_term rest
  | Let
      { backtracks : int;
        x : Sym.t;
        x_bt : BT.t;
        value : term;
        last_var : Sym.t;
        rest : term
      } ->
    !^"let"
    ^^ (if backtracks <> 0 then parens (int backtracks) else empty)
    ^^ (if is_return value then empty else star)
    ^^^ Sym.pp x
    ^^^ colon
    ^^^ BT.pp x_bt
    ^^^ equals
    ^^ nest 2 (break 1 ^^ pp_term value)
    ^^ semi
    ^^^ twice slash
    ^^^ !^"backtracks to"
    ^^^ Sym.pp last_var
    ^/^ pp_term rest
  | Return { value : IT.t } -> !^"return" ^^^ IT.pp value
  | Assert { prop : LC.t; last_var : Sym.t; rest : term } ->
    !^"assert"
    ^^ parens (nest 2 (break 1 ^^ LC.pp prop) ^^ break 1)
    ^^ semi
    ^^^ twice slash
    ^^^ !^"backtracks to"
    ^^^ Sym.pp last_var
    ^/^ pp_term rest
  | ITE { bt : BT.t; cond : IT.t; t : term; f : term } ->
    !^"if"
    ^^^ parens (IT.pp cond)
    ^^^ braces (nest 2 (break 1 ^^ c_comment (BT.pp bt) ^/^ pp_term t) ^^ break 1)
    ^^^ !^"else"
    ^^^ braces (nest 2 (break 1 ^^ pp_term f) ^^ break 1)
  | Map { i; bt; min; max; perm; inner; last_var } ->
    let i_bt, _ = BT.map_bt bt in
    !^"map"
    ^^^ parens
          (BT.pp i_bt
           ^^^ Sym.pp i
           ^^ semi
           ^^^ IT.pp perm
           ^^ c_comment (IT.pp min ^^ !^" <= " ^^ Sym.pp i ^^ !^" <= " ^^ IT.pp max)
           ^^ c_comment (!^"backtracks to" ^^^ Sym.pp last_var))
    ^^ braces (c_comment (BT.pp bt) ^^ nest 2 (break 1 ^^ pp_term inner) ^^ break 1)
  | SplitSize { marker_var; syms; path_vars; last_var; rest } ->
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
    ^/^ pp_term rest


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
    | Arbitrary | Uniform _ | Alloc _ | Call _ | Return _ -> (vars, gt)
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


let elaborate_gt (inputs : Sym.Set.t) (gt : GT.t) : term =
  let rec aux (vars : Sym.t list) (path_vars : Sym.Set.t) (gt : GT.t) : term =
    let last_var = match vars with v :: _ -> v | [] -> bennet in
    let (GT (gt_, bt, loc)) = gt in
    match gt_ with
    | Arbitrary ->
      failwith
        Pp.(plain (!^"Value from " ^^ Locations.pp loc ^^ !^" is still `arbitrary`"))
    | Uniform sz -> Uniform { bt; sz }
    | Pick wgts ->
      let choice_var = Sym.fresh_anon () in
      Pick
        { bt;
          choice_var;
          choices =
            (let wgts =
               let gcd =
                 List.fold_left
                   (fun x y -> Z.gcd x y)
                   (fst (List.hd wgts))
                   (List.map fst (List.tl wgts))
               in
               List.map_fst (fun x -> Z.div x gcd) wgts
             in
             let w_sum = List.fold_left Z.add Z.zero (List.map fst wgts) in
             let max_int = Z.of_int Int.max_int in
             let f =
               if Z.leq w_sum max_int then
                 fun w -> Z.to_int w
               else
                 fun w ->
               Z.to_int
                 (Z.max Z.one (Z.div w (Z.div (Z.add w_sum (Z.pred max_int)) max_int)))
             in
             List.map
               (fun (w, gt) ->
                  (f w, aux (choice_var :: vars) (Sym.Set.add choice_var path_vars) gt))
               wgts);
          last_var
        }
    | Alloc bytes -> Alloc { bytes }
    | Call (fsym, xits) ->
      let (iargs : (Sym.t * Sym.t) list), (gt_lets : Sym.t -> term -> term) =
        List.fold_right
          (fun (y, it) (yzs, f) ->
             let (IT.IT (it_, z_bt, _here)) = it in
             match it_ with
             | Sym z -> ((y, z) :: yzs, f)
             | _ ->
               let z = Sym.fresh_anon () in
               ( (y, z) :: yzs,
                 fun w gr ->
                   Let
                     { backtracks = 0;
                       x = z;
                       x_bt = z_bt;
                       value = Return { value = it };
                       last_var = w;
                       rest = f z gr
                     } ))
          xits
          ([], fun _ gr -> gr)
      in
      gt_lets last_var (Call { fsym; iargs; oarg_bt = bt; path_vars; sized = None })
    | Asgn ((addr, sct), value, rest) ->
      let rec pointer_of (it : IT.t) =
        match it with
        | IT (ArrayShift { base; _ }, _, _) -> pointer_of base
        | IT (Sym x, _, _) | IT (Cast (_, IT (Sym x, _, _)), _, _) -> x
        | _ ->
          let pointers =
            addr
            |> IT.free_vars_bts
            |> Sym.Map.filter (fun _ bt -> BT.equal bt (BT.Loc ()))
            |> Sym.Map.bindings
            |> List.map fst
            |> Sym.Set.of_list
          in
          if not (Sym.Set.cardinal pointers == 1) then
            Cerb_debug.print_debug 2 [] (fun () ->
              Pp.(
                plain
                  (braces
                     (separate_map
                        (comma ^^ space)
                        Sym.pp
                        (List.of_seq (Sym.Set.to_seq pointers)))
                   ^^^ !^" in "
                   ^^ IT.pp addr)));
          if Sym.Set.is_empty pointers then (
            print_endline (Pp.plain (IT.pp it));
            failwith __LOC__);
          Sym.Set.choose pointers
      in
      Asgn
        { pointer = pointer_of addr;
          addr;
          sct;
          value;
          last_var;
          rest = aux vars path_vars rest
        }
    | Let (backtracks, (x, gt1), gt2) ->
      Let
        { backtracks;
          x;
          x_bt = GT.bt gt1;
          value = aux vars path_vars gt1;
          last_var;
          rest = aux (x :: vars) path_vars gt2
        }
    | Return value -> Return { value }
    | Assert (prop, rest) -> Assert { prop; last_var; rest = aux vars path_vars rest }
    | ITE (cond, gt_then, gt_else) ->
      let path_vars = Sym.Set.union path_vars (IT.free_vars cond) in
      ITE { bt; cond; t = aux vars path_vars gt_then; f = aux vars path_vars gt_else }
    | Map ((i, i_bt, perm), inner) ->
      let min, max = IndexTerms.Bounds.get_bounds (i, i_bt) perm in
      Map
        { i;
          bt = Map (i_bt, GT.bt inner);
          min;
          max;
          perm;
          inner = aux (i :: vars) path_vars inner;
          last_var
        }
  in
  aux [] Sym.Set.empty (nice_names inputs gt)


type definition =
  { filename : string;
    sized : bool;
    name : Sym.t;
    iargs : (Sym.t * BT.t) list;
    oargs : (Sym.t * BT.t) list;
    body : term
  }
[@@deriving eq, ord]

let pp_definition (def : definition) : Pp.document =
  let open Pp in
  group
    (!^"generator"
     ^^^ braces
           (separate_map
              (comma ^^ space)
              (fun (x, ty) -> BT.pp ty ^^^ Sym.pp x)
              def.oargs)
     ^^^ Sym.pp def.name
     ^^ parens
          (separate_map (comma ^^ space) (fun (x, ty) -> BT.pp ty ^^^ Sym.pp x) def.iargs)
     ^^^ lbrace
     ^^ nest 2 (break 1 ^^ pp_term def.body)
     ^/^ rbrace)


let elaborate_gd ({ filename; recursive; spec = _; name; iargs; oargs; body } : GD.t)
  : definition
  =
  { filename;
    sized = recursive;
    name;
    iargs;
    oargs;
    body =
      Option.get body
      |> GenNormalize.MemberIndirection.transform
      |> elaborate_gt (Sym.Set.of_list (List.map fst iargs))
  }


type context = (A.ail_identifier * (A.ail_identifier list * definition) list) list

let pp (ctx : context) : Pp.document =
  let defns = ctx |> List.map snd |> List.flatten |> List.map snd in
  let open Pp in
  surround_separate_map
    2
    1
    empty
    lbracket
    (semi ^^ twice hardline)
    rbracket
    pp_definition
    defns


module Sizing = struct
  let count_recursive_calls (syms : Sym.Set.t) (gr : term) : int =
    let rec aux (gr : term) : int =
      match gr with
      | Uniform _ | Alloc _ | Return _ -> 0
      | Pick { choices; _ } ->
        choices |> List.map snd |> List.map aux |> List.fold_left max 0
      | Call { fsym; _ } -> if Sym.Set.mem fsym syms then 1 else 0
      | Asgn { rest; _ } -> aux rest
      | Let { value; rest; _ } -> aux value + aux rest
      | Assert { rest; _ } -> aux rest
      | ITE { t; f; _ } -> max (aux t) (aux f)
      | Map { inner; _ } -> aux inner
      | SplitSize _ -> failwith ("unreachable @ " ^ __LOC__)
    in
    aux gr


  let size_recursive_calls
        (marker_var : Sym.t)
        (syms : Sym.Set.t)
        (size : int)
        (gr : term)
    : term * Sym.Set.t
    =
    let rec aux (gr : term) : term * Sym.Set.t =
      match gr with
      | Call ({ fsym; path_vars; _ } as gr) when Sym.Set.mem fsym syms ->
        let sym = Sym.fresh_anon () in
        let gr' =
          if size > 1 && TestGenConfig.is_random_size_splits () then
            Call
              { gr with
                sized = Some (size, sym);
                path_vars = Sym.Set.add marker_var path_vars
              }
          else
            Call { gr with sized = Some (size, sym) }
        in
        (gr', Sym.Set.singleton sym)
      | Uniform _ | Call _ | Return _ -> (gr, Sym.Set.empty)
      | Alloc { bytes } -> (Alloc { bytes }, Sym.Set.empty)
      | Pick ({ choices; _ } as gr) ->
        let choices, syms =
          choices
          |> List.map (fun (w, gr) ->
            let gr, syms = aux gr in
            ((w, gr), syms))
          |> List.split
        in
        (Pick { gr with choices }, List.fold_left Sym.Set.union Sym.Set.empty syms)
      | Asgn ({ rest; _ } as gr) ->
        let rest, syms = aux rest in
        (Asgn { gr with rest }, syms)
      | Let ({ value; rest; _ } as gr) ->
        let value, syms = aux value in
        let rest, syms' = aux rest in
        (Let { gr with value; rest }, Sym.Set.union syms syms')
      | Assert ({ rest; _ } as gr) ->
        let rest, syms = aux rest in
        (Assert { gr with rest }, syms)
      | ITE ({ t; f; _ } as gr) ->
        let t, syms = aux t in
        let f, syms' = aux f in
        (ITE { gr with t; f }, Sym.Set.union syms syms')
      | Map ({ inner; _ } as gr) ->
        let inner, syms = aux inner in
        (Map { gr with inner }, syms)
      | SplitSize _ -> failwith ("unreachable @ " ^ __LOC__)
    in
    aux gr


  let transform_gr (syms : Sym.Set.t) (gr : term) : term =
    let rec aux (path_vars : Sym.Set.t) (gr : term) : term =
      match gr with
      | ITE { bt; cond; t; f } ->
        let path_vars = Sym.Set.union path_vars (IT.free_vars cond) in
        ITE { bt; cond; t = aux path_vars t; f = aux path_vars f }
      | Pick { bt; choice_var; choices; last_var } ->
        Pick
          { bt;
            choice_var;
            choices = List.map_snd (aux (Sym.Set.add choice_var path_vars)) choices;
            last_var
          }
      | _ ->
        let count = count_recursive_calls syms gr in
        let marker_var = Sym.fresh_anon () in
        let gr, syms = size_recursive_calls marker_var syms count gr in
        if count > 1 then
          SplitSize
            { marker_var; syms; last_var = Sym.fresh "bennet"; path_vars; rest = gr }
        else
          gr
    in
    aux Sym.Set.empty gr


  let transform_def
        (cg : SymGraph.t)
        ({ filename : string;
           sized : bool;
           name : Sym.Set.elt;
           iargs : (Sym.Set.elt * BT.t) list;
           oargs : (Sym.Set.elt * BT.t) list;
           body : term
         } :
          definition)
    : definition
    =
    { filename;
      sized;
      name;
      iargs;
      oargs;
      body = transform_gr (SymGraph.fold_pred Sym.Set.add cg name Sym.Set.empty) body
    }


  let transform (cg : SymGraph.t) (ctx : context) : context =
    List.map_snd
      (List.map_snd (fun ({ sized; _ } as def) ->
         if sized then transform_def cg def else def))
      ctx
end

let elaborate (gtx : GD.context) : context =
  let cg = GA.get_call_graph gtx in
  gtx |> List.map_snd (List.map_snd elaborate_gd) |> Sizing.transform cg
