module BT = BaseTypes
module IT = IndexTerms
module StringMap = Map.Make (String)

let transform_gt (gt : Stage2.Term.t) : Term.t =
  let rec aux (gt : Stage2.Term.t) : Term.t =
    let (GT (gt_, bt, _loc)) = gt in
    match gt_ with
    | Arbitrary -> Arbitrary { bt }
    | Pick wgts ->
      let choices =
        let wgts =
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
            fun w -> w
          else
            fun w -> Z.max Z.one (Z.div w (Z.div (Z.add w_sum (Z.pred max_int)) max_int))
        in
        List.map (fun (w, gt) -> (f w, aux gt)) wgts
      in
      Pick { bt; choices }
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
      gt_lets (Call { fsym; iargs; oarg_bt = bt })
    | Asgn ((addr, sct), value, rest) -> Asgn { addr; sct; value; rest = aux rest }
    | LetStar ((x, gt1), gt2) ->
      LetStar { x; x_bt = Stage2.Term.bt gt1; value = aux gt1; rest = aux gt2 }
    | Return value -> Return { value }
    | Assert (prop, rest) -> Assert { prop; rest = aux rest }
    | ITE (cond, gt_then, gt_else) -> ITE { bt; cond; t = aux gt_then; f = aux gt_else }
    | Map ((i, i_bt, perm), inner) ->
      Map { i; bt = Map (i_bt, Stage2.Term.bt inner); perm; inner = aux inner }
  in
  aux gt


let transform_gd
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
  { filename; recursive; spec; name; iargs; oargs; body = transform_gt body }


let transform (ctx : Stage2.Ctx.t) : Ctx.t = List.map_snd transform_gd ctx
