module BT = BaseTypes
module IT = IndexTerms

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let rec arbitrary_of_sctype (sct : Sctypes.t) loc : Term.t =
    match sct with
    | Sctypes.Array (ct', Some len) ->
      let sym = Sym.fresh_anon () in
      let bt = BT.Bits (Unsigned, 64) in
      Term.map_
        ( ( sym,
            bt,
            IT.and2_
              ( IT.le_ (IT.num_lit_ Z.zero bt loc, IT.sym_ (sym, bt, loc)) loc,
                IT.lt_ (IT.sym_ (sym, bt, loc), IT.num_lit_ (Z.of_int len) bt loc) loc )
              loc ),
          arbitrary_of_sctype ct' loc )
        ()
        loc
    | Array (_, None) ->
      failwith
        Pp.(plain (Sctypes.pp sct ^^^ at ^^^ Locations.pp loc ^^^ at ^^^ !^__LOC__))
    | _ -> Term.arbitrary_ AD.top () (Memory.bt_of_sct sct) loc


  let transform_gt (prog5 : unit Mucore.file) (gt : Term.t) : Term.t =
    let aux (gt : Term.t) : Term.t =
      match gt with
      (* This case is for when nested in a `map` due to needing an arbitrary array*)
      | Annot (`Arbitrary _, (), Struct tag, loc_arb) ->
        (* Generate fresh vars for each member *)
        let members =
          match Pmap.find tag prog5.tagDefs with
          | StructDef pieces ->
            pieces
            |> List.filter_map (fun ({ member_or_padding; _ } : Memory.struct_piece) ->
              member_or_padding)
            |> List.map (fun (member, ct) -> (Sym.fresh_anon (), (member, ct)))
          | _ -> failwith ("no struct " ^ Sym.pp_string tag ^ " found")
        in
        (* Assemble final struct *)
        let gt_struct =
          Term.return_
            (IT.struct_
               ( tag,
                 List.map
                   (fun (y, (member, ct)) ->
                      (member, IT.sym_ (y, Memory.bt_of_sct ct, loc_arb)))
                   members )
               loc_arb)
            ()
            loc_arb
        in
        (* Generate appropriate generators for the members *)
        List.fold_left
          (fun gt'' (y, (_, sct)) ->
             let gt_arb = arbitrary_of_sctype sct loc_arb in
             (* NOTE: By construction, this should only be inside maps, so it'll never get backtracked to *)
             Term.let_star_ ((y, gt_arb), gt'') () loc_arb)
          gt_struct
          members
      | Annot
          ( `LetStar
              ( (x, Annot (`Arbitrary _, (), Struct tag, loc_arb)),
                Annot
                  (`Asgn ((it_addr, Struct tag'), IT (Sym x', _, _), gt_rest), (), _, _)
              ),
            (),
            _,
            _ )
        when TestGenConfig.is_experimental_struct_asgn_destruction () ->
        assert (Sym.equal tag tag');
        assert (Sym.equal x x');
        (* Generate fresh vars for each member *)
        let members =
          match Pmap.find tag prog5.tagDefs with
          | StructDef pieces ->
            pieces
            |> List.filter_map (fun ({ member_or_padding; _ } : Memory.struct_piece) ->
              member_or_padding)
            |> List.map (fun (member, ct) -> (Sym.fresh_anon (), (member, ct)))
          | _ -> failwith ("no struct " ^ Sym.pp_string tag ^ " found")
        in
        (* Assemble final struct *)
        let gt_struct =
          Term.let_star_
            ( ( x,
                Term.return_
                  (IT.struct_
                     ( tag,
                       List.map
                         (fun (y, (member, ct)) ->
                            (member, IT.sym_ (y, Memory.bt_of_sct ct, loc_arb)))
                         members )
                     loc_arb)
                  ()
                  loc_arb ),
              gt_rest )
            ()
            loc_arb
        in
        let loc = Locations.other __LOC__ in
        List.fold_left
          (fun gt_rest' (y, (member, sct)) ->
             Term.let_star_
               ( (y, arbitrary_of_sctype sct loc_arb),
                 Term.asgn_
                   ( (IT.memberShift_ (it_addr, tag, member) (IT.get_loc it_addr), sct),
                     IT.sym_ (y, Memory.bt_of_sct sct, loc),
                     gt_rest' )
                   ()
                   loc )
               ()
               loc_arb)
          gt_struct
          members
      | _ -> gt
    in
    Term.map_gen_pre aux gt


  let transform_gd
        (prog5 : unit Mucore.file)
        ({ filename; recursive; spec; name; iargs; oargs; body } : Def.t)
    : Def.t
    =
    Def.{ filename; recursive; spec; name; iargs; oargs; body = transform_gt prog5 body }


  let transform (prog5 : unit Mucore.file) (ctx : Ctx.t) =
    List.map_snd (transform_gd prog5) ctx
end
