module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module SymGraph = Graph.Persistent.Digraph.Concrete (Sym)
module StringMap = Map.Make (String)

let count_recursive_calls (syms : Sym.Set.t) (gr : Stage2.Term.t) : int =
  let rec aux (gr : Stage2.Term.t) : int =
    let (GT (gr_, _, _)) = gr in
    match gr_ with
    | Arbitrary | Return _ -> 0
    | Pick choices -> choices |> List.map snd |> List.map aux |> List.fold_left max 0
    | Call (fsym, _) -> if Sym.Set.mem fsym syms then 1 else 0
    | Asgn (_, _, rest) -> aux rest
    | LetStar ((_, value), rest) -> aux value + aux rest
    | Assert (_, rest) -> aux rest
    | ITE (_, t, f) -> max (aux t) (aux f)
    | Map (_, inner) -> aux inner
  in
  aux gr


let size_recursive_calls (syms : Sym.Set.t) (size : int) (gr : Stage2.Term.t)
  : Term.t * Sym.Set.t
  =
  let rec aux (gr : Stage2.Term.t) : Term.t * Sym.Set.t =
    let (GT (gr_, bt, _)) = gr in
    match gr_ with
    | Arbitrary -> (Arbitrary { bt }, Sym.Set.empty)
    | Pick choices ->
      let choices, sym_sets =
        choices
        |> List.map (fun (w, gr) ->
          let gr, syms = aux gr in
          ((w, gr), syms))
        |> List.split
      in
      (Pick { bt; choices }, List.fold_left Sym.Set.union Sym.Set.empty sym_sets)
    | Call (fsym, xits) ->
      let (iargs : (Sym.t * Sym.t) list), (gt_lets : Term.t -> Term.t) =
        List.fold_right
          (fun (y, it) (yzs, f) ->
             let (IT.IT (it_, z_bt, _here)) = it in
             match it_ with
             | Sym z -> ((y, z) :: yzs, f)
             | _ ->
               let z = Sym.fresh_anon () in
               ( (y, z) :: yzs,
                 fun gr ->
                   Term.LetStar
                     { x = z; x_bt = z_bt; value = Return { value = it }; rest = f gr } ))
          xits
          ([], fun gr -> gr)
      in
      let sized, set =
        if Sym.Set.mem fsym syms then (
          let sym = Sym.fresh_anon () in
          (Some (size, sym), Sym.Set.singleton sym))
        else
          (None, Sym.Set.empty)
      in
      (gt_lets (Term.Call { fsym; iargs; oarg_bt = bt; sized }), set)
    | Return value -> (Return { value }, Sym.Set.empty)
    | Asgn ((addr, sct), value, rest) ->
      let rest, syms = aux rest in
      (Asgn { addr; sct; value; rest }, syms)
    | LetStar ((x, value), rest) ->
      let x_bt = Stage2.Term.bt value in
      let value, syms = aux value in
      let rest, syms' = aux rest in
      (LetStar { x; x_bt; value; rest }, Sym.Set.union syms syms')
    | Assert (prop, rest) ->
      let rest, syms = aux rest in
      (Assert { prop; rest }, syms)
    | ITE (cond, t, f) ->
      let t, syms = aux t in
      let f, syms' = aux f in
      (ITE { bt; cond; t; f }, Sym.Set.union syms syms')
    | Map ((i, i_bt, perm), inner) ->
      let inner_bt = Stage2.Term.bt inner in
      let inner, syms = aux inner in
      (Map { i; bt = BT.make_map_bt i_bt inner_bt; perm; inner }, syms)
  in
  aux gr


let transform_gt (syms : Sym.Set.t) (gr : Stage2.Term.t) : Term.t =
  let rec aux (path_vars : Sym.Set.t) (gr : Stage2.Term.t) : Term.t =
    let (GT (gr_, bt, _)) = gr in
    match gr_ with
    | ITE (cond, t, f) ->
      let path_vars = Sym.Set.union path_vars (IT.free_vars cond) in
      ITE { bt; cond; t = aux path_vars t; f = aux path_vars f }
    | Pick choices -> Pick { bt; choices = List.map_snd (aux path_vars) choices }
    | _ ->
      let count = count_recursive_calls syms gr in
      let gr, sz_syms = size_recursive_calls syms count gr in
      if count > 1 then
        SplitSize { syms = sz_syms; rest = gr }
      else
        gr
  in
  aux Sym.Set.empty gr


let transform_gd
      (cg : SymGraph.t)
      ({ filename : string;
         recursive : bool;
         spec;
         name : Sym.Set.elt;
         iargs : (Sym.Set.elt * BT.t) list;
         oargs : (Sym.Set.elt * BT.t) list;
         body : Stage2.Term.t
       } :
        Stage2.Def.t)
  : Def.t
  =
  let recursive_syms =
    if recursive then
      SymGraph.fold_pred Sym.Set.add cg name Sym.Set.empty
    else
      Sym.Set.empty
  in
  { filename;
    recursive;
    spec;
    name;
    iargs;
    oargs;
    body = transform_gt recursive_syms body
  }


open struct
  let get_calls (gd : Stage2.Def.t) : Sym.Set.t =
    let rec aux (gt : Stage2.Term.t) : Sym.Set.t =
      let (GT (gt_, _, _)) = gt in
      match gt_ with
      | Arbitrary | Return _ -> Sym.Set.empty
      | Pick choices ->
        choices
        |> List.map snd
        |> List.map aux
        |> List.fold_left Sym.Set.union Sym.Set.empty
      | Call (fsym, _) -> Sym.Set.singleton fsym
      | Asgn (_, _, gt') | Assert (_, gt') | Map (_, gt') -> aux gt'
      | LetStar ((_, gt1), gt2) | ITE (_, gt1, gt2) -> Sym.Set.union (aux gt1) (aux gt2)
    in
    aux gd.body


  module Oper = Graph.Oper.P (SymGraph)
end

let get_call_graph (ctx : Stage2.Ctx.t) : SymGraph.t =
  ctx
  |> List.map_snd get_calls
  |> List.fold_left
       (fun cg (fsym, calls) ->
          Sym.Set.fold (fun fsym' cg' -> SymGraph.add_edge cg' fsym fsym') calls cg)
       SymGraph.empty
  |> Oper.transitive_closure


let transform (ctx : Stage2.Ctx.t) : Ctx.t =
  let cg = get_call_graph ctx in
  List.map_snd (transform_gd cg) ctx
