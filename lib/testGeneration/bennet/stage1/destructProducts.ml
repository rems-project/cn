module BT = BaseTypes
module IT = IndexTerms

module Make (AD : GenTerms.Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  type destruct_tree =
    | Struct of Sym.t * (Id.t * destruct_tree) list
    | Record of (Id.t * destruct_tree) list
    | Tuple of destruct_tree list
    | Leaf of Sym.t * BT.t (** New Name *)

  let rec flatten_tree (t : destruct_tree) : (Sym.t * BT.t) list =
    match t with
    | Struct (_, xts) | Record xts ->
      xts |> List.map snd |> List.map flatten_tree |> List.flatten
    | Tuple ts -> ts |> List.map flatten_tree |> List.flatten
    | Leaf (x, bt) -> [ (x, bt) ]


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
      let name = Sym.fresh Pp.(plain (Sym.pp parent ^^ underscore ^^ int i)) in
      item_map := (k, name) :: !item_map;
      name


  let transform_iarg (prog5 : unit Mucore.file) (arg : Sym.t * BT.t) : destruct_tree =
    let rec aux ((arg_sym, arg_bt) : Sym.t * BT.t) : destruct_tree =
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
             |> List.map_snd aux
           in
           Struct (tag, members)
         | _ -> failwith ("no struct " ^ Sym.pp_string tag ^ " found"))
      | BT.Record members ->
        let members =
          members
          |> List.map (fun (member, bt) ->
            (member, (get_member_new_name arg_sym member, bt)))
          |> List.map_snd aux
        in
        Record members
      | BT.Tuple items ->
        let items =
          items
          |> List.mapi (fun i bt -> (get_item_new_name arg_sym i, bt))
          |> List.map aux
        in
        Tuple items
      | _ -> Leaf (arg_sym, arg_bt)
    in
    aux arg


  let transform_iargs (prog5 : unit Mucore.file) (args : (Sym.t * BT.t) list)
    : (Sym.t * destruct_tree) list
    =
    (* TODO: Ensure uniqueness of names *)
    (* let assert_uniqueness args' =
    let uniq = args' |> List.map fst |> Sym.Set.of_list in
    assert (Sym.Set.cardinal uniq = List.length args');
    args'
  in *)
    args |> List.map (fun (sym, bt) -> (sym, transform_iarg prog5 (sym, bt)))


  let transform_it (prog5 : unit Mucore.file) ((it, bt) : IT.t * BT.t) : IT.t list =
    let rec aux ((it, bt) : IT.t * BT.t) : IT.t list =
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
             (IT.member_ ~member_bt (it, member) loc, member_bt))
           |> List.map aux
           |> List.flatten
         | _ -> failwith ("no struct " ^ Sym.pp_string tag ^ " found"))
      | BT.Record members ->
        members
        |> List.map (fun (member, member_bt) ->
          let loc = Locations.other __LOC__ in
          (IT.recordMember_ ~member_bt (it, member) loc, member_bt))
        |> List.map aux
        |> List.flatten
      | Tuple members ->
        members
        |> List.mapi (fun i item_bt ->
          let loc = Locations.other __LOC__ in
          (IT.nthTuple_ ~item_bt (i, it) loc, item_bt))
        |> List.map aux
        |> List.flatten
      | _ -> [ it ]
    in
    aux (it, bt)


  let transform_its (prog5 : unit Mucore.file) (its : (IT.t * BT.t) list) : IT.t list =
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
    let t = transform_iargs prog5 gd.iargs in
    let iargs = t |> List.map snd |> List.map flatten_tree |> List.flatten in
    let rec assemble_product (t : destruct_tree) : IT.t =
      match t with
      | Struct (tag, members) ->
        IT.struct_ (tag, List.map_snd assemble_product members) (Locations.other __LOC__)
      | Record members ->
        IT.record_ (List.map_snd assemble_product members) (Locations.other __LOC__)
      | Tuple items ->
        IT.tuple_ (List.map assemble_product items) (Locations.other __LOC__)
      | Leaf (x, bt) -> IT.sym_ (x, bt, Locations.other __LOC__)
    in
    let body =
      List.fold_left
        (fun gt_rest (sym, t) ->
           Term.let_star_
             ( (sym, Term.return_ (assemble_product t) () (Locations.other __LOC__)),
               gt_rest )
             ()
             (Locations.other __LOC__))
        (transform_gt prog5 ctx gd.body)
        t
    in
    { gd with iargs; body }


  let transform (prog5 : unit Mucore.file) (ctx : Ctx.t) : Ctx.t =
    List.map_snd (transform_gd prog5 ctx) ctx
end
