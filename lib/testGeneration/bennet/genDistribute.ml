module BT = BaseTypes
module IT = IndexTerms
module GT = GenTerms
module GD = GenDefinitions.Make (GenTerms)

let generated_size (bt : BT.t) : int =
  match bt with
  | Datatype _ -> failwith Pp.(plain (BT.pp bt ^^^ at ^^^ !^__LOC__))
  | _ -> 0


let allocations (gt : GT.t) : GT.t =
  let aux (gt : GT.t) : GT.t =
    let (GT (gt_, bt, loc)) = gt in
    let gt_ =
      match gt_ with
      | Arbitrary ->
        (match bt with
         | Loc () -> GT.Alloc (IT.num_lit_ Z.zero Memory.size_bt loc)
         | _ -> gt_)
      | _ -> gt_
    in
    GT (gt_, bt, loc)
  in
  GT.map_gen_pre aux gt


let default_weights (gt : GT.t) : GT.t =
  let aux (gt : GT.t) : GT.t =
    let (GT (gt_, bt, loc)) = gt in
    let gt_ =
      match gt_ with
      | Arbitrary ->
        (match bt with
         | Map _ | Loc () -> failwith Pp.(plain (BT.pp bt ^^^ at ^^^ !^__LOC__))
         | _ -> GT.Uniform (generated_size bt))
      | _ -> gt_
    in
    GT (gt_, bt, loc)
  in
  GT.map_gen_pre aux gt


let confirm_distribution (gt : GT.t) : GT.t =
  let rec aux (gt : GT.t) : Locations.t list =
    let (GT (gt_, _, loc)) = gt in
    match gt_ with
    | Arbitrary -> [ loc ]
    | Uniform _ | Alloc _ | Call _ | Return _ -> []
    | Pick wgts -> wgts |> List.map snd |> List.map aux |> List.flatten
    | Asgn (_, _, gt') | Assert (_, gt') | Map ((_, _, _), gt') -> aux gt'
    | Let (_, (_, gt1), gt2) | ITE (_, gt1, gt2) ->
      [ gt1; gt2 ] |> List.map aux |> List.flatten
  in
  let failures = aux gt in
  if List.is_empty failures then
    gt
  else
    failwith
      Pp.(
        plain
          (!^"Distribute failure: `arbitrary` still remaining at following locations"
           ^^^ brackets (separate_map (comma ^^ break 1) Locations.pp failures)))


let distribute_gen (gt : GT.t) : GT.t =
  gt |> allocations |> default_weights |> confirm_distribution


let distribute_gen_def ({ filename; recursive; spec; name; iargs; oargs; body } : GD.t)
  : GD.t
  =
  { filename; recursive; spec; name; iargs; oargs; body = Option.map distribute_gen body }


let distribute (ctx : GD.context) : GD.context = List.map_snd distribute_gen_def ctx
