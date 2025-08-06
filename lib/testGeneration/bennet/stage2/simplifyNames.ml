module IT = IndexTerms
module StringMap = Map.Make (String)

module Make (AD : GenTerms.Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let transform_gt (inputs : Sym.Set.t) (gt : Term.t) : Term.t =
    let basename (sym : Sym.t) : string =
      let open Sym in
      match description sym with
      | SD_Id name | SD_CN_Id name | SD_ObjectAddress name | SD_FunArgValue name -> name
      | SD_None -> "fresh"
      | _ -> failwith Pp.(plain (Sym.pp_debug sym ^^^ at ^^^ !^__LOC__))
    in
    let rec aux (vars : int StringMap.t) (gt : Term.t) : int StringMap.t * Term.t =
      let (Annot (gt_, (), bt, loc)) = gt in
      match gt_ with
      | `Arbitrary _ | `Call _ | `Return _ -> (vars, gt)
      | `Pick gts ->
        let vars, gts =
          List.fold_right
            (fun gr' (vars', choices') ->
               let vars'', gr'' = aux vars' gr' in
               (vars'', gr'' :: choices'))
            gts
            (vars, [])
        in
        (vars, Term.pick_ gts () bt loc)
      | `Asgn ((it_addr, sct), it_val, gt') ->
        let vars', gt' = aux vars gt' in
        (vars', Term.asgn_ ((it_addr, sct), it_val, gt') () loc)
      | `LetStar ((x, gt_inner), gt') ->
        let vars, gt_inner = aux vars gt_inner in
        let name = basename x in
        let vars, x, gt' =
          match StringMap.find_opt name vars with
          | Some n ->
            let name' = name ^ "_" ^ string_of_int n in
            let y = Sym.fresh name' in
            ( StringMap.add name (n + 1) vars,
              y,
              Term.subst
                (IT.make_subst
                   [ (x, IT.sym_ (y, Term.basetype gt_inner, Term.loc gt_inner)) ])
                gt' )
          | None -> (StringMap.add name 1 vars, x, gt')
        in
        let vars, gt' = aux vars gt' in
        (vars, Term.let_star_ ((x, gt_inner), gt') () loc)
      | `Assert (lc, gt') ->
        let vars, gt' = aux vars gt' in
        (vars, Term.assert_ (lc, gt') () loc)
      | `ITE (it_if, gt_then, gt_else) ->
        let vars, gt_then = aux vars gt_then in
        let vars, gt_else = aux vars gt_else in
        (vars, Term.ite_ (it_if, gt_then, gt_else) () loc)
      | `Map ((i_sym, i_bt, it_perm), gt_inner) ->
        let vars, gt_inner = aux vars gt_inner in
        let name = basename i_sym in
        let vars, i_sym, it_perm, gt_inner =
          match StringMap.find_opt name vars with
          | Some n ->
            let name' = name ^ "_" ^ string_of_int n in
            let j = Sym.fresh name' in
            let su = IT.make_subst [ (i_sym, IT.sym_ (j, i_bt, loc)) ] in
            ( StringMap.add name (n + 1) vars,
              j,
              IT.subst su it_perm,
              Term.subst su gt_inner )
          | None -> (StringMap.add name 1 vars, i_sym, it_perm, gt_inner)
        in
        (vars, Term.map_ ((i_sym, i_bt, it_perm), gt_inner) () loc)
    in
    snd
      (aux
         (inputs
          |> Sym.Set.to_seq
          |> Seq.map (fun x -> (basename x, 1))
          |> StringMap.of_seq)
         gt)


  let transform_gd (gd : Def.t) : Def.t =
    let inputs = gd.iargs |> List.map fst |> Sym.Set.of_list in
    { gd with body = transform_gt inputs gd.body }


  let transform (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "simplify_names");
    List.map_snd transform_gd ctx
end
