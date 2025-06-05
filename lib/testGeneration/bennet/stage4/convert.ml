module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module SymGraph = Graph.Persistent.Digraph.Concrete (Sym)
module StringMap = Map.Make (String)

let count_recursive_calls (syms : Sym.Set.t) (gr : Stage3.Term.t) : int =
  let rec aux (gr : Stage3.Term.t) : int =
    match gr with
    | Uniform _ | Alloc | Return _ -> 0
    | Pick { choices; _ } ->
      choices |> List.map snd |> List.map aux |> List.fold_left max 0
    | Call { fsym; _ } -> if Sym.Set.mem fsym syms then 1 else 0
    | Asgn { rest; _ } -> aux rest
    | LetStar { value; rest; _ } -> aux value + aux rest
    | Assert { rest; _ } -> aux rest
    | ITE { t; f; _ } -> max (aux t) (aux f)
    | Map { inner; _ } -> aux inner
  in
  aux gr


let size_recursive_calls (syms : Sym.Set.t) (size : int) (gr : Stage3.Term.t)
  : Term.t * Sym.Set.t
  =
  let rec aux (gr : Stage3.Term.t) : Term.t * Sym.Set.t =
    match gr with
    | Uniform { bt } -> (Uniform { bt }, Sym.Set.empty)
    | Pick { bt; choices } ->
      let choices, sym_sets =
        choices
        |> List.map (fun (w, gr) ->
          let gr, syms = aux gr in
          ((w, gr), syms))
        |> List.split
      in
      (Pick { bt; choices }, List.fold_left Sym.Set.union Sym.Set.empty sym_sets)
    | Alloc -> (Alloc, Sym.Set.empty)
    | Call { fsym; iargs; oarg_bt } ->
      let sized, set =
        if Sym.Set.mem fsym syms then (
          let sym = Sym.fresh_anon () in
          (Some (size, sym), Sym.Set.singleton sym))
        else
          (None, Sym.Set.empty)
      in
      (Term.Call { fsym; iargs; oarg_bt; sized }, set)
    | Return { value } -> (Return { value }, Sym.Set.empty)
    | Asgn { addr; sct; value; rest } ->
      let rest, syms = aux rest in
      (Asgn { addr; sct; value; rest }, syms)
    | LetStar { x; x_bt; value; rest } ->
      let value, syms = aux value in
      let rest, syms' = aux rest in
      (LetStar { x; x_bt; value; rest }, Sym.Set.union syms syms')
    | Assert { prop; rest } ->
      let rest, syms = aux rest in
      (Assert { prop; rest }, syms)
    | ITE { bt; cond; t; f } ->
      let t, syms = aux t in
      let f, syms' = aux f in
      (ITE { bt; cond; t; f }, Sym.Set.union syms syms')
    | Map { i; bt; perm; inner } ->
      let inner, syms = aux inner in
      (Map { i; bt; perm; inner }, syms)
  in
  aux gr


let transform_gt (syms : Sym.Set.t) (gr : Stage3.Term.t) : Term.t =
  let rec aux (path_vars : Sym.Set.t) (gr : Stage3.Term.t) : Term.t =
    match gr with
    | ITE { bt; cond; t; f } ->
      let path_vars = Sym.Set.union path_vars (IT.free_vars cond) in
      ITE { bt; cond; t = aux path_vars t; f = aux path_vars f }
    | Pick { bt; choices } -> Pick { bt; choices = List.map_snd (aux path_vars) choices }
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
         body : Stage3.Term.t
       } :
        Stage3.Def.t)
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
  let get_calls (gd : Stage3.Def.t) : Sym.Set.t =
    let rec aux (gt : Stage3.Term.t) : Sym.Set.t =
      match gt with
      | Uniform _ | Alloc | Return _ -> Sym.Set.empty
      | Pick { choices; _ } ->
        choices
        |> List.map snd
        |> List.map aux
        |> List.fold_left Sym.Set.union Sym.Set.empty
      | Call { fsym; _ } -> Sym.Set.singleton fsym
      | Asgn { rest = gt'; _ } | Assert { rest = gt'; _ } | Map { inner = gt'; _ } ->
        aux gt'
      | LetStar { value = gt1; rest = gt2; _ } | ITE { t = gt1; f = gt2; _ } ->
        Sym.Set.union (aux gt1) (aux gt2)
    in
    aux gd.body


  module Oper = Graph.Oper.P (SymGraph)
end

let get_call_graph (ctx : Stage3.Ctx.t) : SymGraph.t =
  ctx
  |> List.map_snd get_calls
  |> List.fold_left
       (fun cg (fsym, calls) ->
          Sym.Set.fold (fun fsym' cg' -> SymGraph.add_edge cg' fsym fsym') calls cg)
       SymGraph.empty
  |> Oper.transitive_closure


let transform (ctx : Stage3.Ctx.t) : Ctx.t =
  let cg = get_call_graph ctx in
  List.map_snd (transform_gd cg) ctx
