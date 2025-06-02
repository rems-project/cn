module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints
module GT = GenTerms
module GD = GenDefinitions.Make (GenTerms)
module GC = GenContext.Make (GenTerms)
module GET = GenElaboratedTerms
module GED = GenDefinitions.Make (GenElaboratedTerms)
module GA = GenAnalysis
module SymGraph = Graph.Persistent.Digraph.Concrete (Sym)
module StringMap = Map.Make (String)

let bennet = Sym.fresh "bennet"

let nice_names (inputs : Sym.Set.t) (gt : GT.t) : GT.t =
  let basename (sym : Sym.t) : string =
    let open Sym in
    match description sym with
    | SD_Id name | SD_CN_Id name | SD_ObjectAddress name | SD_FunArgValue name -> name
    | SD_None -> "fresh"
    | _ -> failwith Pp.(plain (Sym.pp_debug sym ^^^ at ^^^ !^__LOC__))
  in
  let rec aux (vars : int StringMap.t) (gt : GT.t) : int StringMap.t * GT.t =
    let (GT (gt_, _, loc)) = gt in
    match gt_ with
    | Arbitrary | Uniform | Alloc | Call _ | Return _ -> (vars, gt)
    | Pick wgts ->
      let vars, wgts =
        List.fold_right
          (fun (w, gr') (vars', choices') ->
             let vars'', gr'' = aux vars' gr' in
             (vars'', (w, gr'') :: choices'))
          wgts
          (vars, [])
      in
      (vars, GT.pick_ wgts loc)
    | Asgn ((it_addr, sct), it_val, gt') ->
      let vars', gt' = aux vars gt' in
      (vars', GT.asgn_ ((it_addr, sct), it_val, gt') loc)
    | Let (backtracks, (x, gt_inner), gt') ->
      let vars, gt_inner = aux vars gt_inner in
      let name = basename x in
      let vars, x, gt' =
        match StringMap.find_opt name vars with
        | Some n ->
          let name' = name ^ "_" ^ string_of_int n in
          let y = Sym.fresh name' in
          ( StringMap.add name (n + 1) vars,
            y,
            GT.subst
              (IT.make_subst [ (x, IT.sym_ (y, GT.bt gt_inner, GT.loc gt_inner)) ])
              gt' )
        | None -> (StringMap.add name 1 vars, x, gt')
      in
      let vars, gt' = aux vars gt' in
      (vars, GT.let_ (backtracks, (x, gt_inner), gt') loc)
    | Assert (lc, gt') ->
      let vars, gt' = aux vars gt' in
      (vars, GT.assert_ (lc, gt') loc)
    | ITE (it_if, gt_then, gt_else) ->
      let vars, gt_then = aux vars gt_then in
      let vars, gt_else = aux vars gt_else in
      (vars, GT.ite_ (it_if, gt_then, gt_else) loc)
    | Map ((i_sym, i_bt, it_perm), gt_inner) ->
      let vars, gt_inner = aux vars gt_inner in
      let name = basename i_sym in
      let vars, i_sym, it_perm, gt_inner =
        match StringMap.find_opt name vars with
        | Some n ->
          let name' = name ^ "_" ^ string_of_int n in
          let j = Sym.fresh name' in
          let su = IT.make_subst [ (i_sym, IT.sym_ (j, i_bt, loc)) ] in
          (StringMap.add name (n + 1) vars, j, IT.subst su it_perm, GT.subst su gt_inner)
        | None -> (StringMap.add name 1 vars, i_sym, it_perm, gt_inner)
      in
      (vars, GT.map_ ((i_sym, i_bt, it_perm), gt_inner) loc)
  in
  snd
    (aux
       (inputs |> Sym.Set.to_seq |> Seq.map (fun x -> (basename x, 1)) |> StringMap.of_seq)
       gt)


let elaborate_gt (inputs : Sym.Set.t) (gt : GT.t) : GET.t =
  let rec aux (vars : Sym.t list) (path_vars : Sym.Set.t) (gt : GT.t) : GET.t =
    let last_var = match vars with v :: _ -> v | [] -> bennet in
    let (GT (gt_, bt, loc)) = gt in
    match gt_ with
    | Arbitrary ->
      failwith
        Pp.(plain (!^"Value from " ^^ Locations.pp loc ^^ !^" is still `arbitrary`"))
    | Uniform -> Uniform { bt }
    | Pick wgts ->
      let choice_var = Sym.fresh_anon () in
      Pick
        { bt;
          choice_var;
          choices =
            (let wgts =
               let gcd =
                 List.fold_left
                   (fun x y -> Z.gcd x y)
                   (fst (List.hd wgts))
                   (List.map fst (List.tl wgts))
               in
               List.map_fst (fun x -> Z.div x gcd) wgts
             in
             let w_sum = List.fold_left Z.add Z.zero (List.map fst wgts) in
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
               wgts);
          last_var
        }
    | Alloc -> Alloc
    | Call (fsym, xits) ->
      let (iargs : (Sym.t * Sym.t) list), (gt_lets : Sym.t -> GET.t -> GET.t) =
        List.fold_right
          (fun (y, it) (yzs, f) ->
             let (IT.IT (it_, z_bt, _here)) = it in
             match it_ with
             | Sym z -> ((y, z) :: yzs, f)
             | _ ->
               let z = Sym.fresh_anon () in
               ( (y, z) :: yzs,
                 fun w gr ->
                   GET.Let
                     { backtracks = 0;
                       x = z;
                       x_bt = z_bt;
                       value = Return { value = it };
                       last_var = w;
                       rest = f z gr
                     } ))
          xits
          ([], fun _ gr -> gr)
      in
      gt_lets last_var (Call { fsym; iargs; oarg_bt = bt; path_vars; sized = None })
    | Asgn ((addr, sct), value, rest) ->
      let rec pointer_of (it : IT.t) =
        match it with
        | IT (ArrayShift { base; _ }, _, _) -> pointer_of base
        | IT (Sym x, _, _) | IT (Cast (_, IT (Sym x, _, _)), _, _) -> x
        | _ ->
          let pointers =
            addr
            |> IT.free_vars_bts
            |> Sym.Map.filter (fun _ bt -> BT.equal bt (BT.Loc ()))
            |> Sym.Map.bindings
            |> List.map fst
            |> Sym.Set.of_list
          in
          if not (Sym.Set.cardinal pointers == 1) then
            Cerb_debug.print_debug 2 [] (fun () ->
              Pp.(
                plain
                  (braces
                     (separate_map
                        (comma ^^ space)
                        Sym.pp
                        (List.of_seq (Sym.Set.to_seq pointers)))
                   ^^^ !^" in "
                   ^^ IT.pp addr)));
          if Sym.Set.is_empty pointers then (
            print_endline (Pp.plain (IT.pp it));
            failwith __LOC__);
          Sym.Set.choose pointers
      in
      Asgn
        { pointer = pointer_of addr;
          addr;
          sct;
          value;
          last_var;
          rest = aux vars path_vars rest
        }
    | Let (backtracks, (x, gt1), gt2) ->
      Let
        { backtracks;
          x;
          x_bt = GT.bt gt1;
          value = aux vars path_vars gt1;
          last_var;
          rest = aux (x :: vars) path_vars gt2
        }
    | Return value -> Return { value }
    | Assert (prop, rest) -> Assert { prop; last_var; rest = aux vars path_vars rest }
    | ITE (cond, gt_then, gt_else) ->
      let path_vars = Sym.Set.union path_vars (IT.free_vars cond) in
      ITE { bt; cond; t = aux vars path_vars gt_then; f = aux vars path_vars gt_else }
    | Map ((i, i_bt, perm), inner) ->
      let min, max = IndexTerms.Bounds.get_bounds (i, i_bt) perm in
      Map
        { i;
          bt = Map (i_bt, GT.bt inner);
          min;
          max;
          perm;
          inner = aux (i :: vars) path_vars inner;
          last_var
        }
  in
  aux [] Sym.Set.empty (nice_names inputs gt)


let elaborate_gd ({ filename; recursive; spec; name; iargs; oargs; body } : GD.t) : GED.t =
  { filename;
    recursive;
    spec;
    name;
    iargs;
    oargs;
    body =
      body
      |> GenNormalize.MemberIndirection.transform
      |> elaborate_gt (Sym.Set.of_list (List.map fst iargs))
  }


type context = (A.ail_identifier * GED.t) list

let pp (ctx : context) : Pp.document =
  let open Pp in
  ctx
  |> List.map snd
  |> surround_separate_map 2 1 empty lbracket (semi ^^ twice hardline) rbracket GED.pp


module Sizing = struct
  let count_recursive_calls (syms : Sym.Set.t) (gr : GET.t) : int =
    let rec aux (gr : GET.t) : int =
      match gr with
      | Uniform _ | Alloc | Return _ -> 0
      | Pick { choices; _ } ->
        choices |> List.map snd |> List.map aux |> List.fold_left max 0
      | Call { fsym; _ } -> if Sym.Set.mem fsym syms then 1 else 0
      | Asgn { rest; _ } -> aux rest
      | Let { value; rest; _ } -> aux value + aux rest
      | Assert { rest; _ } -> aux rest
      | ITE { t; f; _ } -> max (aux t) (aux f)
      | Map { inner; _ } -> aux inner
      | SplitSize _ -> failwith ("unreachable @ " ^ __LOC__)
    in
    aux gr


  let size_recursive_calls
        (marker_var : Sym.t)
        (syms : Sym.Set.t)
        (size : int)
        (gr : GET.t)
    : GET.t * Sym.Set.t
    =
    let rec aux (gr : GET.t) : GET.t * Sym.Set.t =
      match gr with
      | Call ({ fsym; path_vars; _ } as gr) when Sym.Set.mem fsym syms ->
        let sym = Sym.fresh_anon () in
        let gr' =
          if size > 1 && TestGenConfig.is_random_size_splits () then
            GET.Call
              { gr with
                sized = Some (size, sym);
                path_vars = Sym.Set.add marker_var path_vars
              }
          else
            GET.Call { gr with sized = Some (size, sym) }
        in
        (gr', Sym.Set.singleton sym)
      | Uniform _ | Call _ | Return _ -> (gr, Sym.Set.empty)
      | Alloc -> (Alloc, Sym.Set.empty)
      | Pick ({ choices; _ } as gr) ->
        let choices, syms =
          choices
          |> List.map (fun (w, gr) ->
            let gr, syms = aux gr in
            ((w, gr), syms))
          |> List.split
        in
        (Pick { gr with choices }, List.fold_left Sym.Set.union Sym.Set.empty syms)
      | Asgn ({ rest; _ } as gr) ->
        let rest, syms = aux rest in
        (Asgn { gr with rest }, syms)
      | Let ({ value; rest; _ } as gr) ->
        let value, syms = aux value in
        let rest, syms' = aux rest in
        (Let { gr with value; rest }, Sym.Set.union syms syms')
      | Assert ({ rest; _ } as gr) ->
        let rest, syms = aux rest in
        (Assert { gr with rest }, syms)
      | ITE ({ t; f; _ } as gr) ->
        let t, syms = aux t in
        let f, syms' = aux f in
        (ITE { gr with t; f }, Sym.Set.union syms syms')
      | Map ({ inner; _ } as gr) ->
        let inner, syms = aux inner in
        (Map { gr with inner }, syms)
      | SplitSize _ -> failwith ("unreachable @ " ^ __LOC__)
    in
    aux gr


  let transform_gr (syms : Sym.Set.t) (gr : GET.t) : GET.t =
    let rec aux (path_vars : Sym.Set.t) (gr : GET.t) : GET.t =
      match gr with
      | ITE { bt; cond; t; f } ->
        let path_vars = Sym.Set.union path_vars (IT.free_vars cond) in
        ITE { bt; cond; t = aux path_vars t; f = aux path_vars f }
      | Pick { bt; choice_var; choices; last_var } ->
        Pick
          { bt;
            choice_var;
            choices = List.map_snd (aux (Sym.Set.add choice_var path_vars)) choices;
            last_var
          }
      | _ ->
        let count = count_recursive_calls syms gr in
        let marker_var = Sym.fresh_anon () in
        let gr, syms = size_recursive_calls marker_var syms count gr in
        if count > 1 then
          SplitSize
            { marker_var; syms; last_var = Sym.fresh "bennet"; path_vars; rest = gr }
        else
          gr
    in
    aux Sym.Set.empty gr


  let transform_def
        (cg : SymGraph.t)
        ({ filename : string;
           recursive : bool;
           spec;
           name : Sym.Set.elt;
           iargs : (Sym.Set.elt * BT.t) list;
           oargs : (Sym.Set.elt * BT.t) list;
           body : GET.t
         } :
          GED.t)
    : GED.t
    =
    { filename;
      recursive;
      spec;
      name;
      iargs;
      oargs;
      body = transform_gr (SymGraph.fold_pred Sym.Set.add cg name Sym.Set.empty) body
    }


  let transform (cg : SymGraph.t) (ctx : context) : context =
    List.map_snd
      (fun (GED.{ recursive; _ } as def) ->
         if recursive then transform_def cg def else def)
      ctx
end

let elaborate (gtx : GC.t) : context =
  let cg = GA.get_call_graph gtx in
  gtx |> List.map_snd elaborate_gd |> Sizing.transform cg
