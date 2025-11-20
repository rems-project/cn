module IT = IndexTerms
module LC = LogicalConstraints

module Make (GT : GenTerms.T) = struct
  module Ctx = GenContext.Make (GT)
  module Def = GenDefinitions.Make (GT)
  module Term = GenTerms.Make (GT)

  type kind =
    | Struct of (Id.t * Sym.t) list
    | Record of (Id.t * Sym.t) list
    | Tuple of Sym.t list

  let rec replace_memberof_it (k : kind) (sym : Sym.t) (it : IT.t) : IT.t =
    let repl = replace_memberof_it k sym in
    let (IT (it_, bt, loc)) = it in
    let it_ =
      match it_ with
      | Const _ | Sym _ | SizeOf _ | OffsetOf _ | Nil _ -> it_
      | Unop (op, it') -> IT.Unop (op, repl it')
      | Binop (op, it1, it2) -> IT.Binop (op, repl it1, repl it2)
      | ITE (it1, it2, it3) -> IT.ITE (repl it1, repl it2, repl it3)
      | EachI ((min, (i_sym, i_bt), max), it') ->
        IT.EachI ((min, (i_sym, i_bt), max), repl it')
      | Tuple its -> IT.Tuple (List.map repl its)
      | NthTuple (n, it') ->
        (match (k, IT.is_sym it') with
         | Tuple syms, Some (y, _y_bt) when Sym.equal y sym -> IT.Sym (List.nth syms n)
         | _ -> IT.NthTuple (n, repl it'))
      | Struct (tag, xits) -> IT.Struct (tag, List.map_snd repl xits)
      | StructMember (it', x) ->
        (match (k, IT.is_sym it') with
         | Struct dict, Some (y, _y_bt) when Sym.equal y sym ->
           IT.Sym (List.assoc Id.equal x dict)
         | _ -> IT.StructMember (repl it', x))
      | StructUpdate ((it_struct, x), it_val) ->
        IT.StructUpdate ((repl it_struct, x), repl it_val)
      | Record xits -> IT.Record (List.map_snd repl xits)
      | RecordMember (it', x) ->
        (match (k, IT.is_sym it') with
         | Record dict, Some (y, _y_bt) when Sym.equal y sym ->
           IT.Sym (List.assoc Id.equal x dict)
         | _ -> IT.RecordMember (repl it', x))
      | RecordUpdate ((it_record, x), it_val) ->
        IT.RecordUpdate ((repl it_record, x), repl it_val)
      | Constructor (tag, xits) -> IT.Constructor (tag, List.map_snd repl xits)
      | MemberShift (it', tag, member) -> IT.MemberShift (it', tag, member)
      | ArrayShift { base; ct; index } ->
        IT.ArrayShift { base = repl base; ct; index = repl index }
      | CopyAllocId { addr; loc } -> IT.CopyAllocId { addr = repl addr; loc = repl loc }
      | HasAllocId it' -> IT.HasAllocId (repl it')
      | Cons (it1, it2) -> IT.Cons (repl it1, repl it2)
      | Head it' -> IT.Head (repl it')
      | Tail it' -> IT.Tail (repl it')
      | Representable (sct, it') -> IT.Representable (sct, repl it')
      | Good (sct, it') -> IT.Good (sct, repl it')
      | Aligned { t; align } -> IT.Aligned { t = repl t; align = repl align }
      | WrapI (sct, it') -> IT.WrapI (sct, repl it')
      | MapConst (bt, it') -> IT.MapConst (bt, repl it')
      | MapSet (it1, it2, it3) -> IT.MapSet (repl it1, repl it2, repl it3)
      | MapGet (it1, it2) -> IT.MapGet (repl it1, repl it2)
      | MapDef ((x, bt), it') -> IT.MapDef ((x, bt), repl it')
      | Apply (fsym, its) -> IT.Apply (fsym, List.map repl its)
      | Let ((x, it1), it2) -> IT.Let ((x, repl it1), it2)
      | Match (it', pits) -> IT.Match (repl it', List.map_snd repl pits)
      | Cast (bt, it') -> IT.Cast (bt, repl it')
      | CN_None bt -> IT.CN_None bt
      | CN_Some it' -> IT.CN_Some (repl it')
      | IsSome it' -> IT.IsSome (repl it')
      | GetOpt it' -> IT.GetOpt (repl it')
    in
    IT (it_, bt, loc)


  let replace_memberof_gt (k : kind) (sym : Sym.t) (gt : Term.t) : Term.t =
    let repl = replace_memberof_it k sym in
    let aux (gt : Term.t) : Term.t =
      let (Annot (gt_, tag, bt, loc)) = gt in
      match gt_ with
      | `Call (fsym, iargs) -> Term.call_ (fsym, List.map repl iargs) tag bt loc
      | `Asgn ((it_addr, sct), it_val, gt') ->
        Term.asgn_ ((repl it_addr, sct), repl it_val, gt') tag loc
      | `Return it -> Term.return_ (repl it) tag loc
      | `Assert (T it, gt') -> Term.assert_ (LC.T (repl it), gt') tag loc
      | `Assert (Forall ((i_sym, i_bt), it), gt') ->
        Term.assert_ (LC.Forall ((i_sym, i_bt), repl it), gt') tag loc
      | `ITE (it_if, gt_then, gt_else) -> Term.ite_ (repl it_if, gt_then, gt_else) tag loc
      | `Map ((i_sym, i_bt, it_perm), gt_inner) ->
        Term.map_ ((i_sym, i_bt, repl it_perm), gt_inner) tag loc
      | _ -> gt
    in
    Term.map_gen_pre aux gt


  let transform_gt (gt : Term.t) : Term.t =
    Cerb_debug.print_debug 2 [] (fun () -> "member_indirection");
    let aux (gt : Term.t) : Term.t =
      let (Annot (gt_, tag, _, loc)) = gt in
      match gt_ with
      | `LetStar
          ((x, Annot (`Return (IT (Struct (_, xits), bt, loc_it)), _, _, loc_ret)), gt')
      | `LetStar ((x, Annot (`Return (IT (Record xits, bt, loc_it)), _, _, loc_ret)), gt')
        ->
        let members_to_indirect, members_to_leave =
          xits |> List.partition (fun (_, it) -> Option.is_none (IT.is_sym it))
        in
        let indirect_map =
          List.map_snd (fun _ -> Sym.fresh_anon ()) members_to_indirect
          @ List.map
              (fun (y, it) -> (y, fst (Option.get (IT.is_sym it))))
              members_to_leave
        in
        let k =
          match bt with
          | Struct _ -> Struct indirect_map
          | Record _ -> Record indirect_map
          | _ -> failwith __LOC__
        in
        let gt_main =
          Term.let_star_
            ( ( x,
                Term.return_
                  (let members =
                     indirect_map
                     |> List.map (fun (y, z) ->
                       let it = List.assoc Id.equal y xits in
                       (y, IT.sym_ (z, IT.get_bt it, IT.get_loc it)))
                   in
                   match bt with
                   | Struct tag -> IT.struct_ (tag, members) loc_it
                   | Record _ -> IT.record_ members loc_it
                   | _ -> failwith __LOC__)
                  tag
                  loc_ret ),
              replace_memberof_gt k x gt' )
            tag
            loc
        in
        let here = Locations.other __LOC__ in
        members_to_indirect
        |> List.fold_left
             (fun gt'' (y, it) ->
                Term.let_star_
                  ((List.assoc Id.equal y indirect_map, Term.return_ it tag here), gt'')
                  tag
                  here)
             gt_main
      | `LetStar ((x, Annot (`Return (IT (Tuple items, _bt, loc_it)), _, _, loc_ret)), gt')
        ->
        let items_to_indirect, items_to_leave =
          items
          |> List.mapi (fun i it -> (i, it))
          |> List.partition (fun (_, it) -> Option.is_none (IT.is_sym it))
        in
        let indirect_map =
          List.map_snd (fun _ -> Sym.fresh_anon ()) items_to_indirect
          @ List.map (fun (i, it) -> (i, fst (Option.get (IT.is_sym it)))) items_to_leave
        in
        let k = Tuple (List.map snd indirect_map) in
        let gt_main =
          Term.let_star_
            ( ( x,
                Term.return_
                  (let tuple_items =
                     indirect_map
                     |> List.map (fun (i, sym) ->
                       let it = List.nth items i in
                       IT.sym_ (sym, IT.get_bt it, IT.get_loc it))
                   in
                   IT.tuple_ tuple_items loc_it)
                  tag
                  loc_ret ),
              replace_memberof_gt k x gt' )
            tag
            loc
        in
        let here = Locations.other __LOC__ in
        items_to_indirect
        |> List.fold_left
             (fun gt'' (i, it) ->
                Term.let_star_
                  ((List.assoc Int.equal i indirect_map, Term.return_ it tag here), gt'')
                  tag
                  here)
             gt_main
      | _ -> gt
    in
    Term.map_gen_post aux gt


  let transform_gd (gd : Def.t) : Def.t = { gd with body = transform_gt gd.body }

  let transform (ctx : Ctx.t) = List.map_snd transform_gd ctx
end
