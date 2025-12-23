module BT = BaseTypes
module IT = IndexTerms

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
      let name =
        Sym.fresh_make_uniq Pp.(plain (Sym.pp parent ^^ underscore ^^ Id.pp member))
      in
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
      let name = Sym.fresh_make_uniq Pp.(plain (Sym.pp parent ^^ underscore ^^ int i)) in
      item_map := (k, name) :: !item_map;
      name


  type new_args =
    | Struct of (Id.t * (Sym.t * new_args)) list
    | Record of (Id.t * (Sym.t * new_args)) list
    | Tuple of (Sym.t * new_args) list
    | Leaf of (Sym.t * BT.t)

  let rec args_list_of (na : new_args) : (Sym.t * BT.t) list =
    match na with
    | Struct members ->
      members |> List.map snd |> List.map snd |> List.map args_list_of |> List.flatten
    | Record members ->
      members |> List.map snd |> List.map snd |> List.map args_list_of |> List.flatten
    | Tuple members -> members |> List.map snd |> List.map args_list_of |> List.flatten
    | Leaf arg -> [ arg ]


  let replace_kind_of (na : new_args) : MemberIndirection.kind option =
    match na with
    | Struct members -> Some (Struct (List.map_snd fst members))
    | Record members -> Some (Record (List.map_snd fst members))
    | Tuple members -> Some (Tuple (List.map fst members))
    | Leaf _ -> None


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
           Struct members
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
    let xds = transform_iargs prog5 gd.iargs in
    let iargs = xds |> List.map snd |> List.map args_list_of |> List.flatten in
    let rec replace_member_of (tm_rest : Term.t) ((x, na) : Sym.t * new_args) : Term.t =
      match na with
      | Struct members | Record members ->
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
    { gd with iargs; body }


  let transform (prog5 : unit Mucore.file) (ctx : Ctx.t) : Ctx.t =
    List.map_snd (transform_gd prog5 ctx) ctx
end
