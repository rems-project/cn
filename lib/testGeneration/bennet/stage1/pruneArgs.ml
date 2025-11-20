module IT = IndexTerms

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  (** Indices to delete *)
  type change = Sym.t * int list

  let get_change (def : Def.t) : change =
    ( def.name,
      def.iargs
      |> List.map fst
      |> List.mapi (fun i x -> (i, x))
      |> List.filter_map (fun (i, x) ->
        if Sym.Set.mem x (Term.free_vars def.body) then None else Some i) )


  let get_changes (ctx : Ctx.t) =
    ctx |> List.map snd |> List.map get_change |> List.to_seq |> Sym.Map.of_seq


  module ApplyChanges = struct
    let rec apply_changes_tm (changes : int list Sym.Map.t) (tm : Term.t) : Term.t =
      let (Annot (tm_, (), bt, loc)) = tm in
      match tm_ with
      | `Arbitrary | `Symbolic | `Return _ -> tm
      | `Call (fsym, iargs) ->
        (match Sym.Map.find_opt fsym changes with
         | None -> tm
         | Some indices_to_remove ->
           let new_iargs =
             List.filteri (fun i _ -> not (List.mem Int.equal i indices_to_remove)) iargs
           in
           Term.call_ (fsym, new_iargs) () bt loc)
      | `Asgn ((it_addr, sct), it_val, tm_rest) ->
        Term.asgn_ ((it_addr, sct), it_val, apply_changes_tm changes tm_rest) () loc
      | `LetStar ((x, tm_inner), tm_rest) ->
        Term.let_star_
          ((x, apply_changes_tm changes tm_inner), apply_changes_tm changes tm_rest)
          ()
          loc
      | `Assert (lc, tm_rest) ->
        Term.assert_ (lc, apply_changes_tm changes tm_rest) () loc
      | `ITE (it_if, tm_then, tm_else) ->
        Term.ite_
          (it_if, apply_changes_tm changes tm_then, apply_changes_tm changes tm_else)
          ()
          loc
      | `Map ((i, i_bt, it_perm), tm_inner) ->
        Term.map_ ((i, i_bt, it_perm), apply_changes_tm changes tm_inner) () loc


    let apply_changes_def (changes : int list Sym.Map.t) (def : Def.t) : Def.t =
      let new_iargs =
        match Sym.Map.find_opt def.name changes with
        | None -> def.iargs
        | Some indices_to_remove ->
          List.filteri (fun i _ -> not (List.mem Int.equal i indices_to_remove)) def.iargs
      in
      let new_body = apply_changes_tm changes def.body in
      { def with iargs = new_iargs; body = new_body }


    let apply_changes (changes : int list Sym.Map.t) (ctx : Ctx.t) : Ctx.t =
      List.map (fun (sym, def) -> (sym, apply_changes_def changes def)) ctx
  end

  let transform (ctx : Ctx.t) : Ctx.t =
    let rec aux ctx =
      let changes = get_changes ctx in
      let ctx' = ApplyChanges.apply_changes changes ctx in
      if Ctx.equal ctx' ctx then ctx else aux ctx'
    in
    aux ctx
end
