module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

module Make (AD : Domain.T) = struct
  module Term = Term.Make (AD)
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module IdSet = Set.Make (Id)

  type prune_request =
    | Struct of Id.t list
    | Record of Id.t list
    | Tuple of int list
    | Entire
    | Nothing

  (* Analysis result for how a symbol is used in a term *)
  type usage_analysis =
    { used_directly : bool; (* Symbol used without member access *)
      struct_members : IdSet.t; (* Struct members accessed *)
      record_members : IdSet.t; (* Record members accessed *)
      tuple_indices : int list (* Tuple indices accessed *)
    }

  let empty_analysis =
    { used_directly = false;
      struct_members = IdSet.empty;
      record_members = IdSet.empty;
      tuple_indices = []
    }


  (* Analyze how a symbol is used within an IT expression *)
  let rec analyze_it_usage (sym : Sym.t) (it : IT.t) : usage_analysis =
    match IT.get_term it with
    | Sym s when Sym.equal s sym ->
      (* Direct use of the symbol itself *)
      { empty_analysis with used_directly = true }
    | StructMember (t, member) ->
      (* Check if the base is our symbol *)
      (match IT.get_term t with
       | Sym s when Sym.equal s sym ->
         { empty_analysis with struct_members = IdSet.singleton member }
       | _ ->
         (* The base is not our symbol, recurse normally *)
         analyze_it_usage sym t)
    | RecordMember (t, member) ->
      (match IT.get_term t with
       | Sym s when Sym.equal s sym ->
         { empty_analysis with record_members = IdSet.singleton member }
       | _ ->
         (* The base is not our symbol, recurse normally *)
         analyze_it_usage sym t)
    | NthTuple (n, t) ->
      (match IT.get_term t with
       | Sym s when Sym.equal s sym -> { empty_analysis with tuple_indices = [ n ] }
       | _ ->
         (* The base is not our symbol, recurse normally *)
         analyze_it_usage sym t)
    (* Recursively analyze all other IT constructors *)
    | Const _ -> empty_analysis
    | Sym _ -> empty_analysis
    | Unop (_, t) -> analyze_it_usage sym t
    | Binop (_, t1, t2) ->
      merge_analysis (analyze_it_usage sym t1) (analyze_it_usage sym t2)
    | ITE (t1, t2, t3) ->
      merge_analysis_list
        [ analyze_it_usage sym t1; analyze_it_usage sym t2; analyze_it_usage sym t3 ]
    | Tuple ts -> analyze_it_usage_list sym ts
    | Struct (_, members) -> analyze_it_usage_list sym (List.map snd members)
    | StructUpdate ((t1, _), t2) ->
      merge_analysis (analyze_it_usage sym t1) (analyze_it_usage sym t2)
    | Record members -> analyze_it_usage_list sym (List.map snd members)
    | RecordUpdate ((t1, _), t2) ->
      merge_analysis (analyze_it_usage sym t1) (analyze_it_usage sym t2)
    | Cast (_, t) -> analyze_it_usage sym t
    | MemberShift (t, _, _) -> analyze_it_usage sym t
    | ArrayShift { base; index; _ } ->
      merge_analysis (analyze_it_usage sym base) (analyze_it_usage sym index)
    | CopyAllocId { addr; loc } ->
      merge_analysis (analyze_it_usage sym addr) (analyze_it_usage sym loc)
    | HasAllocId t -> analyze_it_usage sym t
    | SizeOf _ | OffsetOf _ -> empty_analysis
    | Nil _ -> empty_analysis
    | Cons (t1, t2) -> merge_analysis (analyze_it_usage sym t1) (analyze_it_usage sym t2)
    | Head t -> analyze_it_usage sym t
    | Tail t -> analyze_it_usage sym t
    | Representable (_, t) -> analyze_it_usage sym t
    | Good (_, t) -> analyze_it_usage sym t
    | WrapI (_, t) -> analyze_it_usage sym t
    | Aligned { t; align } ->
      merge_analysis (analyze_it_usage sym t) (analyze_it_usage sym align)
    | MapConst (_, t) -> analyze_it_usage sym t
    | MapSet (t1, t2, t3) ->
      merge_analysis_list
        [ analyze_it_usage sym t1; analyze_it_usage sym t2; analyze_it_usage sym t3 ]
    | MapGet (t1, t2) ->
      merge_analysis (analyze_it_usage sym t1) (analyze_it_usage sym t2)
    | MapDef (_, t) -> analyze_it_usage sym t
    | Apply (_, ts) -> analyze_it_usage_list sym ts
    | Let ((_, t1), t2) ->
      merge_analysis (analyze_it_usage sym t1) (analyze_it_usage sym t2)
    | Match (e, cases) ->
      let e_analysis = analyze_it_usage sym e in
      let case_analyses = List.map (fun (_, body) -> analyze_it_usage sym body) cases in
      merge_analysis_list (e_analysis :: case_analyses)
    | Constructor (_, args) -> analyze_it_usage_list sym (List.map snd args)
    | CN_None _ -> empty_analysis
    | CN_Some t -> analyze_it_usage sym t
    | IsSome t -> analyze_it_usage sym t
    | GetOpt t -> analyze_it_usage sym t
    | EachI (_, t) -> analyze_it_usage sym t


  and analyze_it_usage_list (sym : Sym.t) (its : IT.t list) : usage_analysis =
    merge_analysis_list (List.map (analyze_it_usage sym) its)


  and merge_analysis (a1 : usage_analysis) (a2 : usage_analysis) : usage_analysis =
    { used_directly = a1.used_directly || a2.used_directly;
      struct_members = IdSet.union a1.struct_members a2.struct_members;
      record_members = IdSet.union a1.record_members a2.record_members;
      tuple_indices = a1.tuple_indices @ a2.tuple_indices
    }


  and merge_analysis_list (analyses : usage_analysis list) : usage_analysis =
    List.fold_left merge_analysis empty_analysis analyses


  (* Analyze how a symbol is used within a Term *)
  let rec analyze_term_usage (sym : Sym.t) (term : Term.t) : usage_analysis =
    let (GenTerms.Annot (term_, _, _, _)) = term in
    match term_ with
    | `Arbitrary | `Symbolic -> empty_analysis
    | `Call (_, iargs) -> analyze_it_usage_list sym iargs
    | `Asgn ((it_addr, _), it_val, gt') ->
      merge_analysis_list
        [ analyze_it_usage sym it_addr;
          analyze_it_usage sym it_val;
          analyze_term_usage sym gt'
        ]
    | `LetStar ((_, gt_inner), gt_rest) ->
      merge_analysis (analyze_term_usage sym gt_inner) (analyze_term_usage sym gt_rest)
    | `Return it -> analyze_it_usage sym it
    | `Assert (lc, gt') ->
      (* Also need to analyze logical constraints *)
      merge_analysis (analyze_lc_usage sym lc) (analyze_term_usage sym gt')
    | `ITE (it_if, gt_then, gt_else) ->
      merge_analysis_list
        [ analyze_it_usage sym it_if;
          analyze_term_usage sym gt_then;
          analyze_term_usage sym gt_else
        ]
    | `Map ((_, _, it_perm), gt') ->
      merge_analysis (analyze_it_usage sym it_perm) (analyze_term_usage sym gt')


  (* Analyze logical constraints for symbol usage *)
  and analyze_lc_usage (sym : Sym.t) (lc : LC.t) : usage_analysis =
    (* LogicalConstraints contain IT expressions, extract and analyze them *)
    match lc with
    | LC.T it -> analyze_it_usage sym it
    | LC.Forall (_, it) -> analyze_it_usage sym it


  let get_struct_request (sym : Sym.t) (members : Id.t list) (term : Term.t)
    : prune_request
    =
    let analysis = analyze_term_usage sym term in
    if analysis.used_directly then
      Nothing (* Symbol used directly, need entire struct *)
    else (
      (* Check which members are NOT used *)
      let all_members = IdSet.of_list members in
      let unused_members = IdSet.diff all_members analysis.struct_members in
      if IdSet.is_empty unused_members then
        Nothing
      else
        Struct (IdSet.elements unused_members))


  let get_record_request (sym : Sym.t) (members : Id.t list) (term : Term.t)
    : prune_request
    =
    let analysis = analyze_term_usage sym term in
    if analysis.used_directly then
      Nothing
    else (
      let all_members = IdSet.of_list members in
      let unused_members = IdSet.diff all_members analysis.record_members in
      if IdSet.is_empty unused_members then
        Nothing
      else
        Record (IdSet.elements unused_members))


  let get_tuple_request (sym : Sym.t) (length : int) (term : Term.t) : prune_request =
    let analysis = analyze_term_usage sym term in
    if analysis.used_directly then
      Nothing
    else (
      let all_indices = List.init length (fun i -> i) in
      let used_indices = List.sort_uniq compare analysis.tuple_indices in
      let unused_indices =
        List.filter (fun i -> not (List.mem Int.equal i used_indices)) all_indices
      in
      if List.is_empty unused_indices then
        Nothing
      else
        Tuple unused_indices)


  let get_request (prog5 : unit Mucore.file) (sym : Sym.t) (bt : BT.t) (term : Term.t)
    : prune_request
    =
    (* Check if sym appears at all in the term *)
    if not (Sym.Set.mem sym (Term.free_vars term)) then
      Entire (* Symbol not used, can prune entire return value *)
    else (
      (* Symbol is used, check the base type and dispatch to appropriate helper *)
        match bt with
        | BT.Struct tag ->
          (match Pmap.find tag prog5.tagDefs with
           | StructDef layout ->
             let member_ids = Memory.members layout in
             get_struct_request sym member_ids term
           | _ -> Nothing)
        | BT.Record members ->
          let member_ids = List.map fst members in
          get_record_request sym member_ids term
        | BT.Tuple items ->
          let length = List.length items in
          get_tuple_request sym length term
        | _ ->
          (* Not a composite type that can be pruned *)
          Nothing)


  (* Traverse a term and find all generator calls, analyzing their return value usage *)
  let rec get_requests (prog5 : unit Mucore.file) (term : Term.t)
    : prune_request list Sym.Map.t
    =
    let (GenTerms.Annot (term_, _, _, _)) = term in
    let merge_maps = Sym.Map.union (fun _ l1 l2 -> Some (l1 @ l2)) in
    match term_ with
    | `LetStar ((bound_sym, inner_term), gt_rest) ->
      (* Check if inner_term is a Call *)
      let (GenTerms.Annot (inner_term_, _, _, _)) = inner_term in
      let map_from_rest = get_requests prog5 gt_rest in
      (match inner_term_ with
       | `Call (gen_sym, _) ->
         (* This is a call to a generator, analyze how the return value is used *)
         let return_type = Term.basetype inner_term in
         let request = get_request prog5 bound_sym return_type gt_rest in
         (* Add this request to the map *)
         let single_map = Sym.Map.singleton gen_sym [ request ] in
         merge_maps single_map map_from_rest
       | _ ->
         (* Not a call, just merge results from inner and rest *)
         let map_from_inner = get_requests prog5 inner_term in
         merge_maps map_from_inner map_from_rest)
    | `Asgn (_, _, gt') -> get_requests prog5 gt'
    | `Assert (_, gt') -> get_requests prog5 gt'
    | `ITE (_, gt_then, gt_else) ->
      merge_maps (get_requests prog5 gt_then) (get_requests prog5 gt_else)
    | `Map (_, gt') -> get_requests prog5 gt'
    | `Call (fsym, _) -> Sym.Map.singleton fsym [ Nothing ]
    | `Arbitrary | `Symbolic | `Return _ -> Sym.Map.empty


  (* Compare prune requests for deduplication *)
  let compare_prune_request (r1 : prune_request) (r2 : prune_request) : int =
    match (r1, r2) with
    | Entire, Entire -> 0
    | Entire, _ -> -1
    | _, Entire -> 1
    | Nothing, Nothing -> 0
    | Nothing, _ -> -1
    | _, Nothing -> 1
    | Struct ids1, Struct ids2 -> List.compare Id.compare ids1 ids2
    | Struct _, _ -> -1
    | _, Struct _ -> 1
    | Record ids1, Record ids2 -> List.compare Id.compare ids1 ids2
    | Record _, _ -> -1
    | _, Record _ -> 1
    | Tuple is1, Tuple is2 -> List.compare Int.compare is1 is2


  (* Collect prune requests from all generators in the context *)
  let collect_all_requests (prog5 : unit Mucore.file) (ctx : Ctx.t)
    : prune_request list Sym.Map.t
    =
    let merge_maps = Sym.Map.union (fun _ l1 l2 -> Some (l1 @ l2)) in
    let all_maps = List.map (fun (_, (gd : Def.t)) -> get_requests prog5 gd.body) ctx in
    List.fold_left merge_maps Sym.Map.empty all_maps


  (* De-duplicate prune requests for each generator *)
  let deduplicate_requests (map : prune_request list Sym.Map.t)
    : prune_request list Sym.Map.t
    =
    Sym.Map.map (fun reqs -> List.sort_uniq compare_prune_request reqs) map


  (* Modify return type based on prune request *)
  let modify_return_type (bt : BT.t) (request : prune_request) : BT.t =
    match request with
    | Entire -> BT.Unit
    | Nothing -> bt (* No change *)
    | Struct _ -> bt (* Keep same struct type, will use defaults for pruned members *)
    | Record ids_to_remove ->
      (match bt with
       | BT.Record members ->
         let kept_members =
           List.filter (fun (id, _) -> not (List.mem Id.equal id ids_to_remove)) members
         in
         BT.Record kept_members
       | _ -> bt)
    | Tuple indices_to_remove ->
      (match bt with
       | BT.Tuple items ->
         let kept_items =
           List.filteri (fun i _ -> not (List.mem Int.equal i indices_to_remove)) items
         in
         BT.Tuple kept_items
       | _ -> bt)


  (* Transform a term body to prune return values *)
  let transform_body
        (prog5 : unit Mucore.file)
        (bt : BT.t)
        (request : prune_request)
        (term : Term.t)
    : Term.t
    =
    let rec aux (gt : Term.t) : Term.t =
      let (GenTerms.Annot (gt_, (), _bt, loc)) = gt in
      match gt_ with
      | `Arbitrary | `Symbolic -> gt
      | `Return it ->
        (match request with
         | Entire -> Term.return_ (IT.unit_ loc) () loc
         | Nothing -> gt
         | Struct ids_to_remove ->
           (match bt with
            | BT.Struct tag ->
              (match Pmap.find tag prog5.tagDefs with
               | StructDef layout ->
                 let member_types_list = Memory.member_types layout in
                 (* Build struct with defaults for pruned members *)
                 (match IT.get_term it with
                  | Struct (tag, members) ->
                    let new_members =
                      List.map
                        (fun (id, sct) ->
                           if List.mem Id.equal id ids_to_remove then
                             (id, IT.default_ (Memory.bt_of_sct sct) loc)
                           else (
                             (* Find existing value *)
                               match List.assoc_opt Id.equal id members with
                               | Some v -> (id, v)
                               | None -> (id, IT.default_ (Memory.bt_of_sct sct) loc)))
                        member_types_list
                    in
                    Term.return_ (IT.struct_ (tag, new_members) loc) () loc
                  | _ -> gt)
               | _ -> gt)
            | _ -> gt)
         | Record ids_to_remove ->
           (match IT.get_term it with
            | Record members ->
              let kept_members =
                List.filter
                  (fun (id, _) -> not (List.mem Id.equal id ids_to_remove))
                  members
              in
              Term.return_ (IT.record_ kept_members loc) () loc
            | _ -> gt)
         | Tuple indices_to_remove ->
           (match IT.get_term it with
            | Tuple items ->
              let kept_items =
                List.filteri
                  (fun i _ -> not (List.mem Int.equal i indices_to_remove))
                  items
              in
              Term.return_ (IT.tuple_ kept_items loc) () loc
            | _ -> gt))
      | `Call _ -> gt
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        Term.asgn_ ((it_addr, sct), it_val, aux gt_rest) () loc
      | `LetStar ((x, gt_inner), gt_rest) ->
        Term.let_star_ ((x, gt_inner), aux gt_rest) () loc
      | `Assert (lc, gt_rest) -> Term.assert_ (lc, aux gt_rest) () loc
      | `ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (it_if, aux gt_then, aux gt_else) () loc
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        Term.map_ ((i, i_bt, it_perm), gt_inner) () loc
    in
    aux term


  (* Create a variant of a generator with pruned return *)
  let create_variant (prog5 : unit Mucore.file) (gd : Def.t) (request : prune_request)
    : Def.t
    =
    let new_name =
      Sym.fresh_make_uniq Pp.(plain (Sym.pp gd.name ^^ underscore ^^ !^"pruned"))
    in
    let new_oarg = modify_return_type gd.oarg request in
    let new_body = transform_body prog5 gd.oarg request gd.body in
    { gd with name = new_name; oarg = new_oarg; body = new_body }


  (* Adapt all uses of a symbol to use the new basetype *)
  let adapt_symbol_uses
        (_prog5 : unit Mucore.file)
        (sym : Sym.t)
        (_old_bt : BT.t)
        (new_bt : BT.t)
        (request : prune_request)
        (term : Term.t)
    : Term.t
    =
    (* For Struct pruning, no transformation needed since type stays same *)
    match request with
    | Struct _ | Nothing -> term
    | Entire | Record _ | Tuple _ ->
      let adapt_it (it : IT.t) : IT.t =
        let (IT (it_, bt, loc)) = it in
        match it_ with
        (* Direct symbol reference - update its type *)
        | Sym s when Sym.equal s sym -> IT.sym_ (s, new_bt, loc)
        (* Struct member access *)
        | StructMember (base_it, member) ->
          (match IT.get_term base_it with
           | Sym s when Sym.equal s sym ->
             let new_base = IT.sym_ (s, new_bt, loc) in
             IT (StructMember (new_base, member), bt, loc)
           | _ -> it)
        (* Record member access *)
        | RecordMember (base_it, member) ->
          (match IT.get_term base_it with
           | Sym s when Sym.equal s sym ->
             let new_base = IT.sym_ (s, new_bt, loc) in
             IT (RecordMember (new_base, member), bt, loc)
           | _ -> it)
        (* Tuple index access - remap indices for Tuple pruning *)
        | NthTuple (n, base_it) ->
          (match IT.get_term base_it with
           | Sym s when Sym.equal s sym ->
             (match request with
              | Tuple indices_to_remove ->
                (* Calculate new index by counting removed indices before n *)
                let removed_before =
                  List.filter (fun i -> i < n) indices_to_remove |> List.length
                in
                let new_n = n - removed_before in
                let new_base = IT.sym_ (s, new_bt, loc) in
                IT (NthTuple (new_n, new_base), bt, loc)
              | _ ->
                let new_base = IT.sym_ (s, new_bt, loc) in
                IT (NthTuple (n, new_base), bt, loc))
           | _ -> it)
        (* Let map_term_pre handle recursion for everything else *)
        | _ -> it
      in
      let adapt_lc (lc : LC.t) : LC.t =
        match lc with
        | LC.T it -> LC.T (IT.map_term_pre adapt_it it)
        | LC.Forall ((s, forall_bt), body) ->
          LC.Forall ((s, forall_bt), IT.map_term_pre adapt_it body)
      in
      let rec adapt_term (gt : Term.t) : Term.t =
        let (GenTerms.Annot (gt_, tag, term_bt, loc)) = gt in
        match gt_ with
        | `Arbitrary | `Symbolic -> gt
        | `Call (fsym, iargs) ->
          Term.call_ (fsym, List.map (IT.map_term_pre adapt_it) iargs) tag term_bt loc
        | `Asgn ((it_addr, sct), it_val, gt') ->
          Term.asgn_
            ( (IT.map_term_pre adapt_it it_addr, sct),
              IT.map_term_pre adapt_it it_val,
              adapt_term gt' )
            tag
            loc
        | `Return it -> Term.return_ (IT.map_term_pre adapt_it it) tag loc
        | `Assert (lc, gt') -> Term.assert_ (adapt_lc lc, adapt_term gt') tag loc
        | `ITE (it_if, gt_then, gt_else) ->
          Term.ite_
            (IT.map_term_pre adapt_it it_if, adapt_term gt_then, adapt_term gt_else)
            tag
            loc
        | `Map ((i, i_bt, it_perm), gt_inner) ->
          Term.map_
            ((i, i_bt, IT.map_term_pre adapt_it it_perm), adapt_term gt_inner)
            tag
            loc
        | `LetStar ((x, gt_inner), gt_rest) ->
          Term.let_star_ ((x, adapt_term gt_inner), adapt_term gt_rest) tag loc
      in
      adapt_term term


  (* Update call sites in a term to use appropriate variants *)
  let update_call_sites
        (prog5 : unit Mucore.file)
        (variants_map : (prune_request * Def.t) list Sym.Map.t)
        (term : Term.t)
    : Term.t
    =
    let rec aux (gt : Term.t) : Term.t =
      let (GenTerms.Annot (gt_, (), _bt, loc)) = gt in
      match gt_ with
      | `LetStar ((bound_sym, inner_term), gt_rest) ->
        let updated_rest = aux gt_rest in
        let (GenTerms.Annot (inner_term_, (), inner_bt, inner_loc)) = inner_term in
        (match inner_term_ with
         | `Call (gen_sym, args) ->
           (match Sym.Map.find_opt gen_sym variants_map with
            | Some request_variant_list ->
              (* Compute local prune request *)
              let local_request = get_request prog5 bound_sym inner_bt updated_rest in
              (* Find matching variant *)
              (match
                 List.find_opt
                   (fun (req, _) -> compare_prune_request req local_request = 0)
                   request_variant_list
               with
               | Some (_, variant) ->
                 (* Use the matching variant *)
                 let new_call =
                   Term.call_ (variant.name, args) () variant.oarg inner_loc
                 in
                 (* Adapt all uses of bound_sym to use the new type *)
                 let adapted_rest =
                   adapt_symbol_uses
                     prog5
                     bound_sym
                     inner_bt
                     variant.oarg
                     local_request
                     updated_rest
                 in
                 Term.let_star_ ((bound_sym, new_call), adapted_rest) () loc
               | None ->
                 (* No exact match, keep original *)
                 let orig_call = Term.call_ (gen_sym, args) () inner_bt inner_loc in
                 Term.let_star_ ((bound_sym, orig_call), updated_rest) () loc)
            | None ->
              (* No variants, keep original *)
              let orig_call = Term.call_ (gen_sym, args) () inner_bt inner_loc in
              Term.let_star_ ((bound_sym, orig_call), updated_rest) () loc)
         | _ -> Term.let_star_ ((bound_sym, aux inner_term), updated_rest) () loc)
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        Term.asgn_ ((it_addr, sct), it_val, aux gt_rest) () loc
      | `Assert (lc, gt_rest) -> Term.assert_ (lc, aux gt_rest) () loc
      | `ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (it_if, aux gt_then, aux gt_else) () loc
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        Term.map_ ((i, i_bt, it_perm), aux gt_inner) () loc
      | _ -> gt
    in
    aux term


  let transform (prog5 : unit Mucore.file) (ctx : Ctx.t) : Ctx.t =
    (* Step 1 & 2: Collect and de-duplicate all prune requests *)
    let all_requests = collect_all_requests prog5 ctx |> deduplicate_requests in
    (* Step 3: Create variants for each generator *)
    let gen_with_variants : (Sym.t * Def.t * (prune_request * Def.t) list) list =
      ctx
      |> List.map (fun (sym, (gd : Def.t)) ->
        match Sym.Map.find_opt sym all_requests with
        | None ->
          (* No requests found, keep original only *)
          (sym, gd, [ (Nothing, gd) ])
        | Some requests ->
          let has_nothing =
            List.exists (fun r -> compare_prune_request r Nothing = 0) requests
          in
          let non_nothing_requests =
            List.filter (fun r -> compare_prune_request r Nothing <> 0) requests
          in
          let variants =
            List.map (fun req -> (req, create_variant prog5 gd req)) non_nothing_requests
          in
          (sym, gd, if has_nothing then (Nothing, gd) :: variants else variants))
    in
    (* Step 4: Build variants map for call site updating *)
    let variants_map : (prune_request * Def.t) list Sym.Map.t =
      List.fold_left
        (fun map (orig_sym, _, variants) -> Sym.Map.add orig_sym variants map)
        Sym.Map.empty
        gen_with_variants
    in
    (* Step 5: Update all generator bodies and flatten to new context *)
    gen_with_variants
    |> List.concat_map (fun (_, _, request_variant_list) ->
      List.map
        (fun (_, (gd : Def.t)) ->
           let updated_body = update_call_sites prog5 variants_map gd.body in
           let updated_gd = { gd with body = updated_body } in
           (updated_gd.name, updated_gd))
        request_variant_list)
end
