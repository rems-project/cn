module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module GA = GenAnalysis
module SymGraph = Graph.Persistent.Digraph.Concrete (Sym)
module StringMap = Map.Make (String)

let count_recursive_calls (syms : Sym.Set.t) (gr : Term.t) : int =
  let rec aux (gr : Term.t) : int =
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


let size_recursive_calls (syms : Sym.Set.t) (size : int) (gr : Term.t)
  : Term.t * Sym.Set.t
  =
  let rec aux (gr : Term.t) : Term.t * Sym.Set.t =
    match gr with
    | Call ({ fsym; _ } as gr) when Sym.Set.mem fsym syms ->
      let sym = Sym.fresh_anon () in
      let gr' =
        if size > 1 && TestGenConfig.is_random_size_splits () then
          Term.Call { gr with sized = Some (size, sym) }
        else
          Term.Call { gr with sized = Some (size, sym) }
      in
      (gr', Sym.Set.singleton sym)
    | Uniform _ | Call _ | Return _ -> (gr, Sym.Set.empty)
    | Alloc -> (Alloc, Sym.Set.empty)
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
    | LetStar ({ value; rest; _ } as gr) ->
      let value, syms = aux value in
      let rest, syms' = aux rest in
      (LetStar { gr with value; rest }, Sym.Set.union syms syms')
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
  in
  aux gr


let transform_gt (syms : Sym.Set.t) (gr : Term.t) : Term.t =
  let rec aux (path_vars : Sym.Set.t) (gr : Term.t) : Term.t =
    match gr with
    | ITE { bt; cond; t; f } ->
      let path_vars = Sym.Set.union path_vars (IT.free_vars cond) in
      ITE { bt; cond; t = aux path_vars t; f = aux path_vars f }
    | Pick { bt; choices } -> Pick { bt; choices = List.map_snd (aux path_vars) choices }
    | _ ->
      let count = count_recursive_calls syms gr in
      let gr, _syms = size_recursive_calls syms count gr in
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
         body : Term.t
       } :
        Def.t)
  : Def.t
  =
  { filename;
    recursive;
    spec;
    name;
    iargs;
    oargs;
    body = transform_gt (SymGraph.fold_pred Sym.Set.add cg name Sym.Set.empty) body
  }


open struct
  let get_calls (gd : Def.t) : Sym.Set.t =
    let rec aux (gt : Term.t) : Sym.Set.t =
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

let get_call_graph (ctx : Ctx.t) : SymGraph.t =
  ctx
  |> List.map_snd get_calls
  |> List.fold_left
       (fun cg (fsym, calls) ->
          Sym.Set.fold (fun fsym' cg' -> SymGraph.add_edge cg' fsym fsym') calls cg)
       SymGraph.empty
  |> Oper.transitive_closure


let transform (ctx : Ctx.t) : Ctx.t =
  let cg = get_call_graph ctx in
  List.map_snd
    (fun (Def.{ recursive; _ } as def) -> if recursive then transform_gd cg def else def)
    ctx
