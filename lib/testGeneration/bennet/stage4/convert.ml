module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module StringMap = Map.Make (String)

module Make (AD : GenTerms.Domain.T) = struct
  module Stage3 = Stage3.Make (AD)
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let bennet = Sym.fresh "bennet"

  let transform_gt (gt : Stage3.Term.t) : Term.t =
    let rec aux (vars : Sym.t list) (path_vars : Sym.Set.t) (gt : Stage3.Term.t) : Term.t =
      let last_var = match vars with v :: _ -> v | [] -> bennet in
      let (GenTerms.Annot (gt_, (), bt, loc)) = gt in
      match gt_ with
      | `Arbitrary -> GenTerms.Annot (`Arbitrary, (path_vars, last_var), bt, loc)
      | `PickSized wgts ->
        let (`PickSizedElab (choice_var, wgts)) =
          Term.elaborate_pick_ (`PickSized wgts)
        in
        let wgts =
          let gcd =
            List.fold_left
              (fun x y -> Z.gcd x y)
              (fst (List.hd wgts))
              (List.map fst (List.tl wgts))
          in
          List.map_fst (fun x -> Z.div x gcd) wgts
        in
        let wgts =
          let w_sum = List.fold_left Z.add Z.zero (List.map fst wgts) in
          let max_int = Z.of_int Int.max_int in
          let f =
            if Z.leq w_sum max_int then
              fun w -> w
            else
              fun w ->
            Z.max Z.one (Z.div w (Z.div (Z.add w_sum (Z.pred max_int)) max_int))
          in
          List.map
            (fun (w, gt) ->
               (f w, aux (choice_var :: vars) (Sym.Set.add choice_var path_vars) gt))
            wgts
        in
        GenTerms.Annot (`PickSizedElab (choice_var, wgts), (path_vars, last_var), bt, loc)
      | `Call (fsym, iargs) ->
        GenTerms.Annot (`Call (fsym, iargs), (path_vars, last_var), bt, loc)
      | `CallSized (fsym, iargs, sized) ->
        GenTerms.Annot (`CallSized (fsym, iargs, sized), (path_vars, last_var), bt, loc)
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        let gt_ =
          Term.elaborate_asgn_
            (`Asgn ((it_addr, sct), it_val, aux vars path_vars gt_rest))
        in
        GenTerms.Annot (gt_, (path_vars, last_var), bt, loc)
      | `LetStar ((x, gt_inner), gt_rest) ->
        let gt_inner = aux vars path_vars gt_inner in
        let gt_rest = aux (x :: vars) path_vars gt_rest in
        GenTerms.Annot (`LetStar ((x, gt_inner), gt_rest), (path_vars, last_var), bt, loc)
      | `Return it -> GenTerms.Annot (`Return it, (path_vars, last_var), bt, loc)
      | `Assert (lc, gt_rest) ->
        let gt_rest = aux vars path_vars gt_rest in
        GenTerms.Annot (`Assert (lc, gt_rest), (path_vars, last_var), bt, loc)
      | `ITE (it_if, gt_then, gt_else) ->
        let path_vars = Sym.Set.union path_vars (IT.free_vars it_if) in
        let gt_then = aux vars path_vars gt_then in
        let gt_else = aux vars path_vars gt_else in
        Annot (`ITE (it_if, gt_then, gt_else), (path_vars, last_var), bt, loc)
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        let gt_inner = aux (i :: vars) path_vars gt_inner in
        Term.elaborate_map
          (Annot (`Map ((i, i_bt, it_perm), gt_inner), (path_vars, last_var), bt, loc))
      | `SplitSize (syms, gt_rest) ->
        let (`SplitSizeElab (marker_var, syms, gt_rest)) =
          Term.elaborate_split_size_ (`SplitSize (syms, gt_rest))
        in
        let path_vars =
          if TestGenConfig.is_random_size_splits () then
            Sym.Set.add marker_var path_vars
          else
            path_vars
        in
        let gt_rest = aux vars path_vars gt_rest in
        Annot (`SplitSizeElab (marker_var, syms, gt_rest), (path_vars, last_var), bt, loc)
    in
    aux [] Sym.Set.empty gt


  let transform_gd
        ({ filename; recursive; spec; name; iargs; oargs; body } : Stage3.Def.t)
    : Def.t
    =
    { filename; recursive; spec; name; iargs; oargs; body = body |> transform_gt }


  let transform (ctx : Stage3.Ctx.t) : Ctx.t = List.map_snd transform_gd ctx
end
