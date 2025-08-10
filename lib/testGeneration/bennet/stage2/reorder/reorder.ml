module IT = IndexTerms
module LC = LogicalConstraints

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)
  module Stmt = Stmt.Make (AD)

  let add_edge_no_cycle (g : Sym.Digraph.t) ((x, y) : Sym.t * Sym.t) : Sym.Digraph.t =
    let g' =
      let module Oper = Graph.Oper.P (Sym.Digraph) in
      Sym.Digraph.add_edge g x y |> Oper.transitive_closure
    in
    if Sym.Digraph.fold_edges (fun x y acc -> Sym.equal x y || acc) g' false then
      g
    else
      g'


  let get_variable_ordering
        (_rec_fsyms : Sym.Set.t)
        (iargs : Sym.Set.t)
        (stmts : Stmt.t list)
    : Sym.t list
    =
    (* Describes data dependencies where [x <- y] means that [x] depends on [y] *)
    let rec collect_dependencies (stmts : Stmt.t list) : Sym.Digraph.t =
      match stmts with
      | Stmt (LetStar (x, gt), _) :: stmts' ->
        let g = Sym.Digraph.add_vertex (collect_dependencies stmts') x in
        Sym.Set.fold
          (fun y g' ->
             if Sym.Set.mem y iargs then
               g'
             else
               Sym.Digraph.add_edge g' y x)
          (Term.free_vars gt)
          g
      | _ :: stmts' -> collect_dependencies stmts'
      | [] -> Sym.Digraph.empty
    in
    (* Insert edges x <- y_1, ..., y_n when x = f(y_1, ..., y_n) *)
    let rec consider_equalities (stmts : Stmt.t list) (g : Sym.Digraph.t) : Sym.Digraph.t =
      match stmts with
      | Stmt (LetStar (x, _), _) :: stmts' ->
        consider_equalities stmts' (Sym.Digraph.add_vertex g x)
      | Stmt (Assert (T (IT (Binop (EQ, IT (Sym x, _, _), it), _, _))), _) :: stmts'
      | Stmt
          ( Assert (T (IT (Binop (EQ, IT (Cast (_, IT (Sym x, _, _)), _, _), it), _, _))),
            _ )
        :: stmts'
        when not (Sym.Set.mem x (IT.free_vars it)) ->
        let g' =
          List.fold_left
            (fun g' y ->
               if Sym.Set.mem y iargs || Sym.equal x y then
                 g'
               else
                 add_edge_no_cycle g' (y, x))
            g
            (it |> IT.free_vars |> Sym.Set.to_seq |> List.of_seq)
        in
        consider_equalities stmts' g'
      | Stmt (Assert (T (IT (Binop (EQ, it, IT (Sym x, _, _)), _, _))), _) :: stmts'
      | Stmt
          ( Assert (T (IT (Binop (EQ, it, IT (Cast (_, IT (Sym x, _, _)), _, _)), _, _))),
            _ )
        :: stmts'
        when not (Sym.Set.mem x (IT.free_vars it)) ->
        let g' =
          Seq.fold_left
            (fun g' y ->
               if Sym.Set.mem y iargs || Sym.equal x y then
                 g'
               else
                 add_edge_no_cycle g' (y, x))
            g
            (it |> IT.free_vars |> Sym.Set.to_seq)
        in
        consider_equalities stmts' g'
      | _ :: stmts' -> consider_equalities stmts' g
      | [] -> g
    in
    let rec consider_learnable_constraints (stmts : Stmt.t list) (g : Sym.Digraph.t)
      : Sym.Digraph.t
      =
      match stmts with
      | Stmt (LetStar (x, _), _) :: stmts' ->
        consider_learnable_constraints stmts' (Sym.Digraph.add_vertex g x)
      | Stmt (Assert (T (IT (Binop (LE, IT (Sym x, _, _), it), _, _))), _) :: stmts'
      | Stmt (Assert (T (IT (Binop (LEPointer, IT (Sym x, _, _), it), _, _))), _)
        :: stmts'
      | Stmt (Assert (T (IT (Binop (LT, IT (Sym x, _, _), it), _, _))), _) :: stmts'
      | Stmt (Assert (T (IT (Binop (LTPointer, IT (Sym x, _, _), it), _, _))), _)
        :: stmts'
      | Stmt
          ( Assert (T (IT (Binop (LE, IT (Cast (_, IT (Sym x, _, _)), _, _), it), _, _))),
            _ )
        :: stmts'
      | Stmt
          ( Assert
              (T (IT (Binop (LEPointer, IT (Cast (_, IT (Sym x, _, _)), _, _), it), _, _))),
            _ )
        :: stmts'
      | Stmt
          ( Assert (T (IT (Binop (LT, IT (Cast (_, IT (Sym x, _, _)), _, _), it), _, _))),
            _ )
        :: stmts'
      | Stmt
          ( Assert
              (T (IT (Binop (LTPointer, IT (Cast (_, IT (Sym x, _, _)), _, _), it), _, _))),
            _ )
        :: stmts'
        when (not (Sym.Set.mem x (IT.free_vars it)))
             && Option.is_none (IT.is_sym it)
             && it
                |> IT.is_cast
                |> Option.map (fun (_, it') -> IT.is_sym it')
                |> Option.join
                |> Option.is_none ->
        let g' =
          List.fold_left
            (fun g' y ->
               if Sym.equal x y then
                 g'
               else
                 add_edge_no_cycle g' (y, x))
            g
            (it |> IT.free_vars |> Sym.Set.to_seq |> List.of_seq)
        in
        consider_learnable_constraints stmts' g'
      | Stmt (Assert (T (IT (Binop (LE, it, IT (Sym x, _, _)), _, _))), _) :: stmts'
      | Stmt (Assert (T (IT (Binop (LEPointer, it, IT (Sym x, _, _)), _, _))), _)
        :: stmts'
      | Stmt (Assert (T (IT (Binop (LT, it, IT (Sym x, _, _)), _, _))), _) :: stmts'
      | Stmt (Assert (T (IT (Binop (LTPointer, it, IT (Sym x, _, _)), _, _))), _)
        :: stmts'
      | Stmt
          ( Assert (T (IT (Binop (LE, it, IT (Cast (_, IT (Sym x, _, _)), _, _)), _, _))),
            _ )
        :: stmts'
      | Stmt
          ( Assert
              (T (IT (Binop (LEPointer, it, IT (Cast (_, IT (Sym x, _, _)), _, _)), _, _))),
            _ )
        :: stmts'
      | Stmt
          ( Assert (T (IT (Binop (LT, it, IT (Cast (_, IT (Sym x, _, _)), _, _)), _, _))),
            _ )
        :: stmts'
      | Stmt
          ( Assert
              (T (IT (Binop (LTPointer, it, IT (Cast (_, IT (Sym x, _, _)), _, _)), _, _))),
            _ )
        :: stmts'
        when (not (Sym.Set.mem x (IT.free_vars it)))
             && Option.is_none (IT.is_sym it)
             && it
                |> IT.is_cast
                |> Option.map (fun (_, it') -> IT.is_sym it')
                |> Option.join
                |> Option.is_none ->
        let g' =
          List.fold_left
            (fun g' y ->
               if Sym.equal x y then
                 g'
               else
                 add_edge_no_cycle g' (y, x))
            g
            (it |> IT.free_vars |> Sym.Set.to_seq |> List.of_seq)
        in
        consider_learnable_constraints stmts' g'
      | _ :: stmts' -> consider_learnable_constraints stmts' g
      | [] -> g
    in
    (* Put calls before local variables they constrain *)
    let consider_constrained_calls (stmts : Stmt.t list) (g : Sym.Digraph.t)
      : Sym.Digraph.t
      =
      let rec aux (from_calls : Sym.Set.t) (stmts : Stmt.t list) (g : Sym.Digraph.t)
        : Sym.Digraph.t
        =
        match stmts with
        | Stmt (LetStar (x, gt), _) :: stmts' when Term.contains_call gt ->
          aux (Sym.Set.add x from_calls) stmts' g
        | Stmt (Asgn _, _) :: stmts' | Stmt (LetStar _, _) :: stmts' ->
          aux from_calls stmts' g
        | Stmt (Assert lc, _) :: stmts' ->
          let g = aux from_calls stmts' g in
          let free_vars = LC.free_vars lc in
          let call_vars = Sym.Set.inter free_vars from_calls in
          let non_call_vars = Sym.Set.diff free_vars from_calls in
          let add_from_call (x : Sym.t) (g : Sym.Digraph.t) : Sym.Digraph.t =
            Sym.Set.fold (fun y g' -> add_edge_no_cycle g' (y, x)) call_vars g
          in
          Sym.Set.fold add_from_call non_call_vars g
        | [] -> g
      in
      aux Sym.Set.empty stmts g
    in
    (* Get original ordering of variables *)
    let consider_original_ordering (stmts : Stmt.t list) (g : Sym.Digraph.t) =
      let rec aux (defined : Sym.Set.t) (stmts : Stmt.t list) (g : Sym.Digraph.t)
        : Sym.Digraph.t
        =
        match stmts with
        | Stmt (LetStar (x, _), _) :: stmts' ->
          let g' = Sym.Set.fold (fun y g' -> add_edge_no_cycle g' (y, x)) defined g in
          let defined' = Sym.Set.add x defined in
          aux defined' stmts' g'
        | Stmt (Asgn _, _) :: stmts' | Stmt (Assert _, _) :: stmts' ->
          aux defined stmts' g
        | [] -> g
      in
      aux Sym.Set.empty stmts g
    in
    let g =
      collect_dependencies stmts
      |> consider_equalities stmts
      |> consider_learnable_constraints stmts
      |> consider_constrained_calls stmts
      |> consider_original_ordering stmts
    in
    Sym.Digraph.fold_edges (fun x y () -> assert (not (Sym.equal x y))) g ();
    let module T = Graph.Topological.Make (Sym.Digraph) in
    List.rev (T.fold List.cons g [])


  let get_statement_ordering
        (rec_fsyms : Sym.Set.t)
        (iargs : Sym.Set.t)
        (stmts : Stmt.t list)
    : Stmt.t list
    =
    let rec loop (vars : Sym.Set.t) (syms : Sym.t list) (stmts : Stmt.t list)
      : Stmt.t list
      =
      let res, stmts' =
        List.partition
          (fun (stmt : Stmt.t) ->
             match stmt with
             | Stmt (Asgn ((it_addr, _sct), it_val), _) ->
               Sym.Set.subset (IT.free_vars_list [ it_addr; it_val ]) vars
             | Stmt (Assert lc, _) -> Sym.Set.subset (LC.free_vars lc) vars
             | _ -> false)
          stmts
      in
      match syms with
      | sym :: syms' ->
        let res', stmts'' =
          List.partition
            (fun (stmt : Stmt.t) ->
               match stmt with Stmt (LetStar (x, _), _) -> Sym.equal x sym | _ -> false)
            stmts'
        in
        res @ res' @ loop (Sym.Set.add sym vars) syms' stmts''
      | [] ->
        if List.non_empty stmts' then
          print_endline
            (match stmts' with
             | [ Stmt (Assert lc, _) ] ->
               Pp.(
                 LC.free_vars lc
                 |> Sym.Set.to_seq
                 |> List.of_seq
                 |> separate_map (comma ^^ space) Sym.pp
                 |> plain)
             | _ -> "ss");
        res
    in
    let syms = get_variable_ordering rec_fsyms iargs stmts in
    loop iargs syms stmts


  let reorder (rec_fsyms : Sym.Set.t) (iargs : Sym.Set.t) (gt : Term.t) : Term.t =
    let stmts, gt_last = Stmt.stmts_of_gt gt in
    let stmts = get_statement_ordering rec_fsyms iargs stmts in
    Stmt.gt_of_stmts stmts gt_last


  let transform_gd (gtx : Ctx.t) (gd : Def.t) : Def.t =
    let rec_fsyms =
      gtx
      |> List.map snd
      |> List.filter_map (fun (gd' : Def.t) ->
        if gd'.recursive then Some gd'.name else None)
      |> Sym.Set.of_list
    in
    let rec aux (iargs : Sym.Set.t) (gt : Term.t) : Term.t =
      let rec loop (iargs : Sym.Set.t) (gt : Term.t) : Term.t =
        let (Annot (gt_, (), bt, loc)) = gt in
        match gt_ with
        | `Arbitrary | `Call _ | `Return _ -> gt
        | `Pick gts -> Term.pick_ (List.map (aux iargs) gts) () bt loc
        | `Asgn ((it_addr, sct), it_val, gt_rest) ->
          Term.asgn_ ((it_addr, sct), it_val, loop iargs gt_rest) () loc
        | `LetStar ((x, gt'), gt_rest) ->
          let iargs = Sym.Set.add x iargs in
          Term.let_star_ ((x, (aux iargs) gt'), loop iargs gt_rest) () loc
        | `Assert (lc, gt_rest) -> Term.assert_ (lc, loop iargs gt_rest) () loc
        | `ITE (it_if, gt_then, gt_else) ->
          Term.ite_ (it_if, aux iargs gt_then, aux iargs gt_else) () loc
        | `Map ((i_sym, i_bt, it_perm), gt_inner) ->
          Term.map_
            ((i_sym, i_bt, it_perm), aux (Sym.Set.add i_sym iargs) gt_inner)
            ()
            loc
      in
      gt |> reorder rec_fsyms iargs |> loop iargs
    in
    let iargs = gd.iargs |> List.map fst |> Sym.Set.of_list in
    { gd with body = aux iargs gd.body }


  let transform (ctx : Ctx.t) : Ctx.t = List.map_snd (transform_gd ctx) ctx
end
