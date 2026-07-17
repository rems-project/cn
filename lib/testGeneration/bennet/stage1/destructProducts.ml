module BT = BaseTypes
module T = Terms.Normal
module MT = MakeTerm

module Make (AD : Domain.T) = struct
  module MemberIndirection = MemberIndirection.Make (Term.Make (AD))
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let member_map = ref []

  let get_member_new_name (parent : Sym.t) (member : Id.t) =
    let k = (parent, member) in
    match
      List.assoc_opt
        (fun (x1, y1) (x2, y2) -> Sym.equal x1 x2 && Id.equal y1 y2)
        k
        !member_map
    with
    | Some name -> name
    | None ->
      (* Deterministic name (no uniquifying counter) so that codegen consumers which
         do not share this memo -- notably [SpecTests.convert_from], which reconstructs
         the whole-struct argument for the function-under-test call -- can compute the
         same record field name from just the parent symbol and member. *)
      let name = Sym.fresh Pp.(plain (Sym.pp parent ^^ underscore ^^ Id.pp member)) in
      member_map := (k, name) :: !member_map;
      name


  let item_map = ref []

  let get_item_new_name (parent : Sym.t) (i : int) =
    let k = (parent, i) in
    match
      List.assoc_opt
        (fun (x1, y1) (x2, y2) -> Sym.equal x1 x2 && Int.equal y1 y2)
        k
        !item_map
    with
    | Some name -> name
    | None ->
      (* Deterministic name (no uniquifying counter); see [get_member_new_name]. *)
      let name = Sym.fresh Pp.(plain (Sym.pp parent ^^ underscore ^^ int i)) in
      item_map := (k, name) :: !item_map;
      name


  type new_args =
    | Struct of Sym.t * (Id.t * (Sym.t * new_args)) list
    | Record of (Id.t * (Sym.t * new_args)) list
    | Tuple of (Sym.t * new_args) list
    | Leaf of (Sym.t * BT.t)

  let rec args_list_of (na : new_args) : (Sym.t * BT.t) list =
    match na with
    | Struct (_, members) ->
      members |> List.map snd |> List.map snd |> List.map args_list_of |> List.flatten
    | Record members ->
      members |> List.map snd |> List.map snd |> List.map args_list_of |> List.flatten
    | Tuple members -> members |> List.map snd |> List.map args_list_of |> List.flatten
    | Leaf arg -> [ arg ]


  let replace_kind_of (na : new_args) : MemberIndirection.kind option =
    match na with
    | Struct (_, members) -> Some (Struct (List.map_snd fst members))
    | Record members -> Some (Record (List.map_snd fst members))
    | Tuple members -> Some (Tuple (List.map fst members))
    | Leaf _ -> None


  (* Rebuild the original (whole-struct/record/tuple) argument value from the
     destructured leaf symbols, so any reference to the parent symbol that is not
     a member access (e.g. [valid_state(s)], [Good(struct T, s)]) stays in scope. *)
  let rec it_of_new_args (na : new_args) (loc : Locations.t) : T.t =
    match na with
    | Struct (tag, members) ->
      MT.struct_
        ( tag,
          List.map (fun (member, (_, na')) -> (member, it_of_new_args na' loc)) members )
        loc
    | Record members ->
      MT.record_
        (List.map (fun (member, (_, na')) -> (member, it_of_new_args na' loc)) members)
        loc
    | Tuple items ->
      MT.tuple_ (List.map (fun (_, na') -> it_of_new_args na' loc) items) loc
    | Leaf (sym, bt) -> MT.sym_ (sym, bt, loc)


  (* Collect every aggregate node in the tree -- the top-level parent AND every nested
     struct/record/tuple -- paired with its symbol. Each needs rebinding, because a
     whole-value reference at any depth (e.g. [valid_inner(s.inner)]) is rewritten by
     [MemberIndirection] to that node's symbol, which is otherwise neither a leaf iarg
     nor reconstructed. *)
  let rec aggregate_rebinds ((x, na) : Sym.t * new_args) : (Sym.t * new_args) list =
    match na with
    | Leaf _ -> []
    | Struct (_, members) | Record members ->
      (x, na) :: (members |> List.map snd |> List.map aggregate_rebinds |> List.flatten)
    | Tuple items -> (x, na) :: (items |> List.map aggregate_rebinds |> List.flatten)


  let transform_iarg (prog5 : unit Mucore.file) ((arg_sym, arg_bt) : Sym.t * BT.t)
    : new_args
    =
    let rec aux (arg_sym : Sym.t) (arg_bt : BT.t) =
      match arg_bt with
      | BT.Struct tag ->
        (match Pmap.find tag prog5.tagDefs with
         | StructDef pieces ->
           let members =
             pieces
             |> List.filter_map (fun ({ member_or_padding; _ } : Memory.struct_piece) ->
               member_or_padding)
             |> List.map (fun (member, sct) ->
               (member, (get_member_new_name arg_sym member, Memory.bt_of_sct sct)))
             |> List.map_snd (fun (sym, bt) -> (sym, aux sym bt))
           in
           Struct (tag, members)
         | _ -> failwith ("no struct " ^ Sym.pp_string tag ^ " found"))
      | BT.Record members ->
        let members =
          members
          |> List.map (fun (member, bt) ->
            (member, (get_member_new_name arg_sym member, bt)))
          |> List.map_snd (fun (sym, bt) -> (sym, aux sym bt))
        in
        Record members
      | BT.Tuple items ->
        let items =
          items
          |> List.mapi (fun i bt -> (get_item_new_name arg_sym i, bt))
          |> List.map (fun (sym, bt) -> (sym, aux sym bt))
        in
        Tuple items
      | _ -> Leaf (arg_sym, arg_bt)
    in
    aux arg_sym arg_bt


  let transform_iargs (prog5 : unit Mucore.file) (args : (Sym.t * BT.t) list)
    : (Sym.t * new_args) list
    =
    List.map (fun (sym, bt) -> (sym, transform_iarg prog5 (sym, bt))) args


  (* Destructure a spec parameter's C type in lockstep with its [new_args] tree, so
     [c_types] keeps an entry for each destructured leaf argument. The leaf symbols
     come from [na] (i.e. the actual new iargs); the C types come from the struct
     definition. C parameters are never CN records/tuples. *)
  let rec c_types_of_new_args (prog5 : unit Mucore.file) (na : new_args) ct
    : (Sym.t * Cerb_frontend.Ctype.ctype) list
    =
    match na with
    | Leaf (sym, _bt) -> [ (sym, ct) ]
    | Struct (tag, members) ->
      let member_scts =
        match Pmap.find tag prog5.tagDefs with
        | StructDef pieces ->
          pieces
          |> List.filter_map (fun ({ member_or_padding; _ } : Memory.struct_piece) ->
            member_or_padding)
        | _ -> failwith ("no struct " ^ Sym.pp_string tag ^ " found")
      in
      List.map2
        (fun (_, (_, na')) (_, member_sct) ->
           c_types_of_new_args prog5 na' (Sctypes.to_ctype member_sct))
        members
        member_scts
      |> List.flatten
    | Record _ | Tuple _ ->
      failwith "destructProducts: unexpected record/tuple C-typed parameter"


  let transform_it (prog5 : unit Mucore.file) ((it, bt) : T.t * BT.t) : T.t list =
    let rec aux ((it, bt) : T.t * BT.t) : T.t list =
      match bt with
      | BT.Struct tag ->
        (match Pmap.find tag prog5.tagDefs with
         | StructDef pieces ->
           pieces
           |> List.filter_map (fun ({ member_or_padding; _ } : Memory.struct_piece) ->
             member_or_padding)
           |> List.map (fun (member, sct) ->
             let loc = Locations.other __LOC__ in
             let member_bt = Memory.bt_of_sct sct in
             (MT.member_ ~member_bt (it, member) loc, member_bt))
           |> List.map aux
           |> List.flatten
         | _ -> failwith ("no struct " ^ Sym.pp_string tag ^ " found"))
      | BT.Record members ->
        members
        |> List.map (fun (member, member_bt) ->
          let loc = Locations.other __LOC__ in
          (MT.recordMember_ ~member_bt (it, member) loc, member_bt))
        |> List.map aux
        |> List.flatten
      | Tuple members ->
        members
        |> List.mapi (fun i item_bt ->
          let loc = Locations.other __LOC__ in
          (MT.nthTuple_ ~item_bt (i, it) loc, item_bt))
        |> List.map aux
        |> List.flatten
      | _ -> [ it ]
    in
    aux (it, bt)


  let transform_its (prog5 : unit Mucore.file) (its : (T.t * BT.t) list) : T.t list =
    its |> List.map (transform_it prog5) |> List.flatten


  let transform_gt (prog5 : unit Mucore.file) (ctx : Ctx.t) (gt : Term.t) : Term.t =
    let aux (gt : Term.t) : Term.t =
      let (Annot (gt_, (), bt, loc)) = gt in
      match gt_ with
      | `Call (fsym, iargs) ->
        let gd = Ctx.find fsym ctx in
        let iargs' =
          gd.iargs |> List.map snd |> List.combine iargs |> transform_its prog5
        in
        Term.call_ (fsym, iargs') () bt loc
      | _ -> gt
    in
    Term.map_gen_pre aux gt


  let transform_gd (prog5 : unit Mucore.file) (ctx : Ctx.t) (gd : Def.t) : Def.t =
    let xds = transform_iargs prog5 gd.iargs in
    let iargs = xds |> List.map snd |> List.map args_list_of |> List.flatten in
    let rec replace_member_of (tm_rest : Term.t) ((x, na) : Sym.t * new_args) : Term.t =
      match na with
      | Struct (_, members) | Record members ->
        let k = Option.get (replace_kind_of na) in
        List.fold_left
          (fun tm_rest' (_, xna') -> replace_member_of tm_rest' xna')
          (MemberIndirection.replace_memberof_gt k x tm_rest)
          members
      | Tuple members ->
        let k = Option.get (replace_kind_of na) in
        List.fold_left
          (fun tm_rest' xna' -> replace_member_of tm_rest' xna')
          (MemberIndirection.replace_memberof_gt k x tm_rest)
          members
      | Leaf _ -> tm_rest
    in
    let body = xds |> List.fold_left replace_member_of (transform_gt prog5 ctx gd.body) in
    (* Rebind each destructured aggregate argument -- at every nesting depth -- to a value
       reconstructed from its leaf symbols, so whole-value references (e.g. [valid_state(s)]
       or [valid_inner(s.inner)]) remain well-scoped. Unused rebindings are dropped by
       stage2 [RemoveUnused]. *)
    let loc = Locations.other __LOC__ in
    let body =
      xds
      |> List.map aggregate_rebinds
      |> List.flatten
      |> List.fold_left
           (fun body (x, na) ->
              Term.let_star_
                ((x, Term.return_ (it_of_new_args na loc) () loc), body)
                ()
                loc)
           body
    in
    let c_types =
      gd.c_types
      |> Option.map (fun cts ->
        cts
        |> List.map (fun (param_sym, ct) ->
          match
            List.find_opt
              (fun (x, _) -> String.equal (Sym.pp_string x) (Sym.pp_string param_sym))
              xds
          with
          | Some (_, na) -> c_types_of_new_args prog5 na ct
          | None -> [ (param_sym, ct) ])
        |> List.flatten)
    in
    { gd with iargs; c_types; body }


  let transform (prog5 : unit Mucore.file) (ctx : Ctx.t) : Ctx.t =
    List.map_snd (transform_gd prog5 ctx) ctx
end
