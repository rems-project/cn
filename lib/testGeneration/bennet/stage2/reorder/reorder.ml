module IT = IndexTerms
module LC = LogicalConstraints
module SymGraph = Graph.Persistent.Digraph.Concrete (Sym)

let get_variable_ordering
      (_rec_fsyms : Sym.Set.t)
      (iargs : Sym.Set.t)
      (stmts : Stmt.t list)
  : Sym.t list
  =
  let module Oper = Graph.Oper.P (SymGraph) in
  (* Insert edges x <- y_1, ..., y_n when x = f(y_1, ..., y_n) *)
  let rec consider_equalities (stmts : Stmt.t list) : SymGraph.t =
    match stmts with
    | Stmt (LetStar (x, _), _) :: stmts' ->
      SymGraph.add_vertex (consider_equalities stmts') x
    | Stmt (Assert (T (IT (Binop (EQ, IT (Sym x, _, _), it), _, _))), _) :: stmts' ->
      let g = consider_equalities stmts' in
      let g' =
        List.fold_left
          (fun g' y ->
             if Sym.Set.mem y iargs || Sym.equal x y then
               g'
             else
               SymGraph.add_edge_e g' (y, x))
          g
          (it |> IT.free_vars |> Sym.Set.to_seq |> List.of_seq)
      in
      g'
    | Stmt (Assert (T (IT (Binop (EQ, it, IT (Sym x, _, _)), _, _))), _) :: stmts' ->
      let g = consider_equalities stmts' in
      Seq.fold_left
        (fun g' y ->
           if Sym.Set.mem y iargs || Sym.equal x y then
             g'
           else
             SymGraph.add_edge_e g' (y, x))
        g
        (it |> IT.free_vars |> Sym.Set.to_seq)
    | _ :: stmts' -> consider_equalities stmts'
    | [] -> SymGraph.empty
  in
  (* Put calls before local variables they constrain *)
  let rec consider_constrained_calls
            (from_calls : Sym.Set.t)
            (g : SymGraph.t)
            (stmts : Stmt.t list)
    : SymGraph.t
    =
    match stmts with
    | Stmt (LetStar (x, gt), _) :: stmts' when Term.contains_call gt ->
      consider_constrained_calls (Sym.Set.add x from_calls) g stmts'
    | Stmt (Asgn _, _) :: stmts' | Stmt (LetStar _, _) :: stmts' ->
      consider_constrained_calls from_calls g stmts'
    | Stmt (Assert lc, _) :: stmts' ->
      let g = consider_constrained_calls from_calls g stmts' in
      let free_vars = LC.free_vars lc in
      let call_vars = Sym.Set.inter free_vars from_calls in
      let non_call_vars = Sym.Set.diff free_vars from_calls in
      let add_from_call (x : Sym.t) (g : SymGraph.t) : SymGraph.t =
        Sym.Set.fold (fun y g' -> SymGraph.add_edge g' y x) call_vars g
      in
      Sym.Set.fold add_from_call non_call_vars g
    | [] -> g
  in
  (* Describes logical dependencies where [x <- y] means that [x] depends on [y] *)
  let collect_constraints (stmts : Stmt.t list) : SymGraph.t =
    let g = consider_equalities stmts in
    let g' = Oper.transitive_closure g in
    let g'' = consider_constrained_calls Sym.Set.empty g' stmts in
    let g''' = Oper.transitive_closure g'' in
    assert (not (SymGraph.fold_edges (fun x y acc -> Sym.equal x y || acc) g''' false));
    g'''
  in
  (* Describes data dependencies where [x <- y] means that [x] depends on [y] *)
  let collect_dependencies (stmts : Stmt.t list) : SymGraph.t =
    let rec aux (stmts : Stmt.t list) : SymGraph.t =
      match stmts with
      | Stmt (LetStar (x, gt), _) :: stmts' ->
        let g = SymGraph.add_vertex (aux stmts') x in
        Sym.Set.fold
          (fun y g' ->
             if Sym.Set.mem y iargs then
               g'
             else
               SymGraph.add_edge g' y x)
          (Term.free_vars gt)
          g
      | _ :: stmts' -> aux stmts'
      | [] -> SymGraph.empty
    in
    stmts |> aux |> Oper.transitive_closure
  in
  let g_c = collect_constraints stmts in
  let g_d = collect_dependencies stmts in
  let orig_order =
    List.filter_map (function Stmt.Stmt (LetStar (x, _), _) -> Some x | _ -> None) stmts
  in
  (* Get variables that should be generated before [x], in the order they appear *)
  let get_needs (x : Sym.t) (ys : Sym.t list) : Sym.t list =
    let syms_c =
      SymGraph.fold_pred
        (fun y syms -> if List.mem Sym.equal y ys then syms else Sym.Set.add y syms)
        g_c
        x
        Sym.Set.empty
    in
    let syms =
      Sym.Set.fold
        (fun z acc ->
           Sym.Set.union
             acc
             (SymGraph.fold_pred
                (fun y syms' ->
                   if List.mem Sym.equal y ys then syms' else Sym.Set.add y syms')
                g_d
                z
                Sym.Set.empty))
        syms_c
        syms_c
    in
    orig_order |> List.filter (fun x -> Sym.Set.mem x syms)
  in
  let new_order : Sym.t list -> Sym.t list =
    List.fold_left
      (fun acc y ->
         if List.mem Sym.equal y acc then
           acc
         else (
           let zs = get_needs y acc in
           if List.is_empty zs then
             acc @ [ y ]
           else
             acc @ zs @ [ y ]))
      []
  in
  let rec loop (ys : Sym.t list) : Sym.t list =
    let old_ys = ys in
    let new_ys = new_order ys in
    if List.equal Sym.equal old_ys new_ys then new_ys else loop new_ys
  in
  loop orig_order


let get_statement_ordering
      (rec_fsyms : Sym.Set.t)
      (iargs : Sym.Set.t)
      (stmts : Stmt.t list)
  : Stmt.t list
  =
  let rec loop (vars : Sym.Set.t) (syms : Sym.t list) (stmts : Stmt.t list) : Stmt.t list =
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
      let (GT (gt_, _bt, loc)) = gt in
      match gt_ with
      | Uniform | Alloc | Call _ | Return _ -> gt
      | Pick wgts -> Term.pick_ (List.map_snd (aux iargs) wgts) loc
      | Asgn ((it_addr, sct), it_val, gt_rest) ->
        Term.asgn_ ((it_addr, sct), it_val, loop iargs gt_rest) loc
      | LetStar ((x, gt'), gt_rest) ->
        let iargs = Sym.Set.add x iargs in
        Term.let_star_ ((x, (aux iargs) gt'), loop iargs gt_rest) loc
      | Assert (lc, gt_rest) -> Term.assert_ (lc, loop iargs gt_rest) loc
      | ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (it_if, aux iargs gt_then, aux iargs gt_else) loc
      | Map ((i_sym, i_bt, it_perm), gt_inner) ->
        Term.map_ ((i_sym, i_bt, it_perm), aux (Sym.Set.add i_sym iargs) gt_inner) loc
    in
    gt |> reorder rec_fsyms iargs |> loop iargs
  in
  let iargs = gd.iargs |> List.map fst |> Sym.Set.of_list in
  { gd with body = aux iargs gd.body }


let transform (ctx : Ctx.t) : Ctx.t = List.map_snd (transform_gd ctx) ctx
