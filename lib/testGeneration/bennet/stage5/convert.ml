module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module SymGraph = Graph.Persistent.Digraph.Concrete (Sym)
module StringMap = Map.Make (String)

let bennet = Sym.fresh "bennet"

let transform_gt (gt : Stage4.Term.t) : Term.t =
  let rec aux (vars : Sym.t list) (path_vars : Sym.Set.t) (gt : Stage4.Term.t) : Term.t =
    let last_var = match vars with v :: _ -> v | [] -> bennet in
    match gt with
    | Uniform { bt } -> Uniform { bt }
    | Pick { bt; choices } ->
      let choice_var = Sym.fresh_anon () in
      Pick
        { bt;
          choice_var;
          choices =
            (let choices =
               let gcd =
                 List.fold_left
                   (fun x y -> Z.gcd x y)
                   (fst (List.hd choices))
                   (List.map fst (List.tl choices))
               in
               List.map_fst (fun x -> Z.div x gcd) choices
             in
             let w_sum = List.fold_left Z.add Z.zero (List.map fst choices) in
             let max_int = Z.of_int Int.max_int in
             let f =
               if Z.leq w_sum max_int then
                 fun w -> Z.to_int w
               else
                 fun w ->
               Z.to_int
                 (Z.max Z.one (Z.div w (Z.div (Z.add w_sum (Z.pred max_int)) max_int)))
             in
             List.map
               (fun (w, gt) ->
                  (f w, aux (choice_var :: vars) (Sym.Set.add choice_var path_vars) gt))
               choices);
          last_var
        }
    | Alloc -> Alloc
    | Call { fsym; iargs; oarg_bt; sized } ->
      Call { fsym; iargs; oarg_bt; path_vars; last_var; sized }
    | Asgn { addr; sct; value; rest } ->
      let rec pointer_of (it : IT.t) : Sym.t * BT.t =
        match it with
        | IT (CopyAllocId { loc = ptr; _ }, _, _)
        | IT (ArrayShift { base = ptr; _ }, _, _)
        | IT (MemberShift (ptr, _, _), _, _) ->
          pointer_of ptr
        | IT (Sym x, bt, _) | IT (Cast (_, IT (Sym x, bt, _)), _, _) -> (x, bt)
        | _ ->
          let pointers =
            addr
            |> IT.free_vars_bts
            |> Sym.Map.filter (fun _ bt -> BT.equal bt (BT.Loc ()))
          in
          if not (Sym.Map.cardinal pointers == 1) then
            Cerb_debug.print_debug 2 [] (fun () ->
              Pp.(
                plain
                  (braces
                     (separate_map
                        (comma ^^ space)
                        (fun (x, bt) -> Sym.pp x ^^ colon ^^^ BT.pp bt)
                        (List.of_seq (Sym.Map.to_seq pointers)))
                   ^^^ !^" in "
                   ^^ IT.pp addr)));
          if Sym.Map.is_empty pointers then (
            print_endline (Pp.plain (IT.pp it));
            failwith __LOC__);
          Sym.Map.choose pointers
      in
      let backtrack_var = Sym.fresh_anon () in
      Asgn
        { backtrack_var;
          pointer = pointer_of addr;
          addr;
          sct;
          value;
          last_var;
          rest = aux vars path_vars rest
        }
    | LetStar { x; x_bt; value; rest } ->
      LetStar
        { x;
          x_bt;
          value = aux vars path_vars value;
          last_var;
          rest = aux (x :: vars) path_vars rest
        }
    | Return { value } -> Return { value }
    | Assert { prop; rest } -> Assert { prop; last_var; rest = aux vars path_vars rest }
    | ITE { bt; cond; t; f } ->
      let path_vars = Sym.Set.union path_vars (IT.free_vars cond) in
      ITE { bt; cond; t = aux vars path_vars t; f = aux vars path_vars f }
    | Map { i; bt; perm; inner } ->
      let i_bt, _ = BT.map_bt bt in
      let min, max = IndexTerms.Bounds.get_bounds (i, i_bt) perm in
      Map { i; bt; min; max; perm; inner = aux (i :: vars) path_vars inner; last_var }
    | SplitSize { syms; rest } ->
      let marker_var = Sym.fresh_anon () in
      let path_vars =
        if TestGenConfig.is_random_size_splits () then
          Sym.Set.add marker_var path_vars
        else
          path_vars
      in
      SplitSize { marker_var; syms; rest = aux vars path_vars rest; last_var; path_vars }
  in
  aux [] Sym.Set.empty gt


let transform_gd ({ filename; recursive; spec; name; iargs; oargs; body } : Stage4.Def.t)
  : Def.t
  =
  { filename; recursive; spec; name; iargs; oargs; body = body |> transform_gt }


let transform (ctx : Stage4.Ctx.t) : Ctx.t = List.map_snd transform_gd ctx
