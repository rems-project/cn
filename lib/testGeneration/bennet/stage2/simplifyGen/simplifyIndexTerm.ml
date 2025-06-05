module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

module Builtin = struct
  let transform_gt (prog5 : unit Mucore.file) (gt : Term.t) : Term.t =
    let globals =
      { Global.empty with
        logical_functions = Sym.Map.of_seq (List.to_seq prog5.logical_predicates)
      }
    in
    let simp_it (it : IT.t) : IT.t =
      Simplify.IndexTerms.simp ~inline_functions:true (Simplify.default globals) it
    in
    let simp_lc (lc : LC.t) : LC.t =
      Simplify.LogicalConstraints.simp
        ~inline_functions:true
        (Simplify.default globals)
        lc
    in
    let aux (gt : Term.t) : Term.t =
      let (GT (gt_, bt, loc)) = gt in
      match gt_ with
      | Call (fsym, iargs) -> Term.call_ (fsym, List.map_snd simp_it iargs) bt loc
      | Asgn ((it_addr, sct), it_val, gt') ->
        Term.asgn_ ((simp_it it_addr, sct), simp_it it_val, gt') loc
      | Return it -> Term.return_ (simp_it it) loc
      | Assert (lc, gt') -> Term.assert_ (simp_lc lc, gt') loc
      | ITE (it_if, gt_then, gt_else) -> Term.ite_ (simp_it it_if, gt_then, gt_else) loc
      | Map ((i, i_bt, it_perm), gt') -> Term.map_ ((i, i_bt, simp_it it_perm), gt') loc
      | _ -> gt
    in
    Term.map_gen_pre aux gt
end

module Fixes = struct
  let simplify_it (it : IT.t) : IT.t =
    let aux (it : IT.t) : IT.t =
      let (IT.IT (it_, bt, loc)) = it in
      match it_ with
      | Binop
          ( Add,
            IT (Binop (Add, it1, IT (Const (Bits (_, n1)), _, _)), _, _),
            IT (Const (Bits ((sgn, sz), n2)), _, _) )
      | Binop
          ( Add,
            IT (Const (Bits ((sgn, sz), n2)), _, _),
            IT (Binop (Add, it1, IT (Const (Bits (_, n1)), _, _)), _, _) )
        when BT.fits_range (sgn, sz) (Z.add n1 n2) ->
        IT.add_ (it1, IT.num_lit_ (Z.add n1 n2) (Bits (sgn, sz)) loc) loc
      | Binop
          ( Add,
            IT (Binop (Add, IT (Const (Bits (_, n1)), _, _), it1), _, _),
            IT (Const (Bits ((sgn, sz), n2)), _, _) )
      | Binop
          ( Add,
            IT (Const (Bits ((sgn, sz), n2)), _, _),
            IT (Binop (Add, IT (Const (Bits (_, n1)), _, _), it1), _, _) )
        when BT.fits_range (sgn, sz) (Z.add n1 n2) ->
        IT.add_ (it1, IT.num_lit_ (Z.add n1 n2) (Bits (sgn, sz)) loc) loc
      | Binop
          ( Add,
            IT (Binop (Sub, it1, IT (Const (Bits (_, n1)), _, _)), _, _),
            IT (Const (Bits ((sgn, sz), n2)), _, _) )
      | Binop
          ( Add,
            IT (Const (Bits ((sgn, sz), n2)), _, _),
            IT (Binop (Sub, it1, IT (Const (Bits (_, n1)), _, _)), _, _) )
        when BT.fits_range (sgn, sz) (Z.sub n1 n2) ->
        IT.add_ (it1, IT.num_lit_ (Z.sub n1 n2) (Bits (sgn, sz)) loc) loc
      | Binop
          ( EQ,
            IT
              ( Binop (Mul, IT (Binop (Div, IT (Sym x, x_bt, x_loc), it1), _, _), it2),
                _,
                _ ),
            IT (Sym x', _, _) )
      | Binop
          ( EQ,
            IT
              ( Binop (MulNoSMT, IT (Binop (Div, IT (Sym x, x_bt, x_loc), it1), _, _), it2),
                _,
                _ ),
            IT (Sym x', _, _) )
      | Binop
          ( EQ,
            IT
              ( Binop (Mul, IT (Binop (DivNoSMT, IT (Sym x, x_bt, x_loc), it1), _, _), it2),
                _,
                _ ),
            IT (Sym x', _, _) )
      | Binop
          ( EQ,
            IT
              ( Binop
                  ( MulNoSMT,
                    IT (Binop (DivNoSMT, IT (Sym x, x_bt, x_loc), it1), _, _),
                    it2 ),
                _,
                _ ),
            IT (Sym x', _, _) )
        when Sym.equal x x' && IT.equal it1 it2 ->
        IT.eq_
          (IT.mod_ (IT.sym_ (x, x_bt, x_loc), it1) loc, IT.num_lit_ Z.zero x_bt loc)
          loc
      | Binop
          ( EQ,
            IT (Sym x, x_bt, x_loc),
            IT (Binop (Mul, IT (Binop (Div, IT (Sym x', _, _), it1), _, _), it2), _, _) )
      | Binop
          ( EQ,
            IT (Sym x, x_bt, x_loc),
            IT
              (Binop (MulNoSMT, IT (Binop (Div, IT (Sym x', _, _), it1), _, _), it2), _, _)
          )
      | Binop
          ( EQ,
            IT (Sym x, x_bt, x_loc),
            IT
              (Binop (Mul, IT (Binop (DivNoSMT, IT (Sym x', _, _), it1), _, _), it2), _, _)
          )
      | Binop
          ( EQ,
            IT (Sym x, x_bt, x_loc),
            IT
              ( Binop (MulNoSMT, IT (Binop (DivNoSMT, IT (Sym x', _, _), it1), _, _), it2),
                _,
                _ ) )
        when Sym.equal x x' && IT.equal it1 it2 ->
        IT.eq_
          (IT.mod_ (IT.sym_ (x, x_bt, x_loc), it1) loc, IT.num_lit_ Z.zero x_bt loc)
          loc
      | (Binop (Min, it1, it2) | Binop (Max, it1, it2)) when IT.equal it1 it2 -> it1
      | Binop (Min, it1, it2) ->
        (match (IT.is_bits_const it1, IT.is_bits_const it2) with
         | Some (_, n1), Some (_, n2) -> IT.num_lit_ (Z.min n1 n2) bt loc
         | Some ((sgn, sz), n), _ when Z.equal n (fst (BT.bits_range (sgn, sz))) -> it1
         | _, Some ((sgn, sz), n) when Z.equal n (fst (BT.bits_range (sgn, sz))) -> it2
         | Some ((sgn, sz), n), _ when Z.equal n (snd (BT.bits_range (sgn, sz))) -> it2
         | _, Some ((sgn, sz), n) when Z.equal n (snd (BT.bits_range (sgn, sz))) -> it1
         | _ -> it)
      | Binop (Max, it1, it2) ->
        (match (IT.is_bits_const it1, IT.is_bits_const it2) with
         | Some (_, n1), Some (_, n2) -> IT.num_lit_ (Z.max n1 n2) bt loc
         | Some ((sgn, sz), n), _ when Z.equal n (snd (BT.bits_range (sgn, sz))) -> it1
         | _, Some ((sgn, sz), n) when Z.equal n (snd (BT.bits_range (sgn, sz))) -> it2
         | Some ((sgn, sz), n), _ when Z.equal n (fst (BT.bits_range (sgn, sz))) -> it2
         | _, Some ((sgn, sz), n) when Z.equal n (fst (BT.bits_range (sgn, sz))) -> it1
         | _ -> it)
      | _ -> it
    in
    IT.map_term_post aux it


  let simplify_lc (lc : LC.t) : LC.t =
    match lc with
    | T it -> T (simplify_it it)
    | Forall ((i, i_bt), IT (Binop (Implies, it_perm, it_body), _, loc_implies)) ->
      let it_perm = simplify_it it_perm in
      let it_body = simplify_it it_body in
      if IT.is_false it_perm || IT.is_true it_body then
        LC.T (IT.bool_ true loc_implies)
      else
        LC.Forall ((i, i_bt), IT.arith_binop Implies (it_perm, it_body) loc_implies)
    | _ -> failwith __LOC__


  let transform_gt (gt : Term.t) : Term.t =
    let aux (gt : Term.t) : Term.t =
      let (GT (gt_, bt, loc)) = gt in
      match gt_ with
      | Call (fsym, iargs) -> Term.call_ (fsym, List.map_snd simplify_it iargs) bt loc
      | Asgn ((it_addr, sct), it_val, gt') ->
        Term.asgn_ ((simplify_it it_addr, sct), simplify_it it_val, gt') loc
      | Return it -> Term.return_ (simplify_it it) loc
      | Assert (lc, gt') -> Term.assert_ (simplify_lc lc, gt') loc
      | ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (simplify_it it_if, gt_then, gt_else) loc
      | Map ((i, i_bt, it_perm), gt') ->
        Term.map_ ((i, i_bt, simplify_it it_perm), gt') loc
      | _ -> gt
    in
    Term.map_gen_post aux gt
end

let transform_gt (prog5 : unit Mucore.file) (gt : Term.t) : Term.t =
  gt |> Builtin.transform_gt prog5 |> Fixes.transform_gt


let transform_gd (prog5 : unit Mucore.file) (gd : Def.t) : Def.t =
  { gd with body = transform_gt prog5 gd.body }


let transform (prog5 : unit Mucore.file) (ctx : Ctx.t) : Ctx.t =
  List.map_snd (transform_gd prog5) ctx
