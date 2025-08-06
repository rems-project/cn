module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module StringMap = Map.Make (String)

module Make (AD : GenTerms.Domain.T) = struct
  module Stage2 = Stage2.Make (AD)
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let count_recursive_calls (syms : Sym.Set.t) (gr : Stage2.Term.t) : int =
    let rec aux (gr : Stage2.Term.t) : int =
      let (Annot (gr_, (), _, _)) = gr in
      match gr_ with
      | `Arbitrary _ | `Return _ -> 0
      | `Pick choices -> choices |> List.map aux |> List.fold_left max 0
      | `Call (fsym, _) -> if Sym.Set.mem fsym syms then 1 else 0
      | `Asgn (_, _, rest) -> aux rest
      | `LetStar ((_, value), rest) -> aux value + aux rest
      | `Assert (_, rest) -> aux rest
      | `ITE (_, t, f) -> max (aux t) (aux f)
      | `Map (_, inner) -> aux inner
    in
    aux gr


  let size_recursive_calls (syms : Sym.Set.t) (size : int) (gr : Stage2.Term.t)
    : Term.t * Sym.Set.t
    =
    let rec aux (gr : Stage2.Term.t) : Term.t * Sym.Set.t =
      let (Annot (gr_, (), bt, loc)) = gr in
      match gr_ with
      | `Arbitrary d -> (GenTerms.Annot (`Arbitrary d, (), bt, loc), Sym.Set.empty)
      | `Pick wgts ->
        let wgts, sym_sets =
          wgts
          |> List.map (fun gt -> (Z.one, gt))
          |> List.map (fun (w, gr) ->
            let gr, syms = aux gr in
            ((w, gr), syms))
          |> List.split
        in
        ( GenTerms.Annot (`PickSized wgts, (), bt, loc),
          List.fold_left Sym.Set.union Sym.Set.empty sym_sets )
      | `Call (fsym, xits) when Sym.Set.mem fsym syms ->
        let sym = Sym.fresh_anon () in
        ( GenTerms.Annot (`CallSized (fsym, xits, (size, sym)), (), bt, loc),
          Sym.Set.singleton sym )
      | `Call (fsym, xits) ->
        (GenTerms.Annot (`Call (fsym, xits), (), bt, loc), Sym.Set.empty)
      | `Return it -> (GenTerms.Annot (`Return it, (), bt, loc), Sym.Set.empty)
      | `Asgn ((addr, sct), value, rest) ->
        let rest, syms = aux rest in
        (GenTerms.Annot (`Asgn ((addr, sct), value, rest), (), bt, loc), syms)
      | `LetStar ((x, value), rest) ->
        let value, syms = aux value in
        let rest, syms' = aux rest in
        ( GenTerms.Annot (`LetStar ((x, value), rest), (), bt, loc),
          Sym.Set.union syms syms' )
      | `Assert (lc, rest) ->
        let rest, syms = aux rest in
        (GenTerms.Annot (`Assert (lc, rest), (), bt, loc), syms)
      | `ITE (cond, t, f) ->
        let t, syms = aux t in
        let f, syms' = aux f in
        (GenTerms.Annot (`ITE (cond, t, f), (), bt, loc), Sym.Set.union syms syms')
      | `Map ((i, i_bt, perm), inner) ->
        let inner, syms = aux inner in
        (GenTerms.Annot (`Map ((i, i_bt, perm), inner), (), bt, loc), syms)
    in
    aux gr


  let transform_gt (syms : Sym.Set.t) (gr : Stage2.Term.t) : Term.t =
    let rec aux (path_vars : Sym.Set.t) (gr : Stage2.Term.t) : Term.t =
      let (Annot (gr_, (), bt, loc)) = gr in
      match gr_ with
      | `ITE (cond, t, f) ->
        let path_vars = Sym.Set.union path_vars (IT.free_vars cond) in
        GenTerms.Annot (`ITE (cond, aux path_vars t, aux path_vars f), (), bt, loc)
      | `Pick gts ->
        GenTerms.Annot
          (`PickSized (List.map (fun gt -> (Z.one, aux path_vars gt)) gts), (), bt, loc)
      | _ ->
        let count = count_recursive_calls syms gr in
        let gr, sz_syms = size_recursive_calls syms count gr in
        if count > 1 then
          GenTerms.Annot (`SplitSize (sz_syms, gr), (), bt, loc)
        else
          gr
    in
    aux Sym.Set.empty gr


  let transform_gd
        (cg : Sym.Digraph.t)
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
        Sym.Digraph.fold_pred Sym.Set.add cg name Sym.Set.empty
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


  let transform (ctx : Stage2.Ctx.t) : Ctx.t =
    let module Oper = Graph.Oper.P (Sym.Digraph) in
    let cg = Oper.transitive_closure (Stage2.Ctx.get_call_graph ctx) in
    List.map_snd (transform_gd cg) ctx
end
