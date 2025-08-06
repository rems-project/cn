module IT = IndexTerms
module BT = BaseTypes
module AT = ArgumentTypes
module LC = LogicalConstraints
module LAT = LogicalArgumentTypes
module Config = TestGenConfig

module Make (AD : GenTerms.Domain.T) = struct
  module OptCtx = GenContext.MakeOptional (Term.Make (AD))
  module OptDef = GenDefinitions.MakeOptional (Term.Make (AD))
  module Tm = Term.Make (AD)

  module State : sig
    type s = OptCtx.t

    type 'a m

    val modify : (s -> s) -> unit m

    val ( let@ ) : 'a m -> ('a -> 'b m) -> 'b m

    val return : 'a -> 'a m

    val runState : 'a m -> s -> 'a * s

    val execState : 'a m -> s -> s
  end = struct
    type s = OptCtx.t

    type 'a t = s -> 'a * s

    type 'a m = 'a t

    let modify f : unit m = fun s -> ((), f s)

    let ( let@ ) (g : 'a m) (f : 'a -> 'b m) : 'b m =
      fun s ->
      let x, s' = g s in
      f x s'


    let return (x : 'a) : 'a m = fun s -> (x, s)

    let runState f s = f s

    let execState f s = snd (f s)
  end

  open State

  let cn_return = Sym.fresh "cn_return"

  let transform_oargs (ret_bt : BT.t) (iargs : (Sym.t * BT.t) list) : (Sym.t * BT.t) list =
    match ret_bt with Unit -> [] | _ -> (cn_return, ret_bt) :: iargs


  let add_request
        (recursive : Sym.Set.t)
        (preds : (Sym.Map.key * Definition.Predicate.t) list)
        (fsym : Sym.t)
    : unit m
    =
    let pred = List.assoc Sym.equal fsym preds in
    let gd : OptDef.t =
      { filename = Option.get (Cerb_location.get_filename pred.loc);
        recursive = Sym.Set.mem fsym recursive;
        spec = false;
        name = fsym;
        iargs = (pred.pointer, BT.Loc ()) :: pred.iargs;
        oargs = transform_oargs (snd pred.oarg) [];
        body = None
      }
    in
    modify (OptCtx.add gd)


  let transform_vars
        (generated : Sym.Set.t)
        (oargs : (Sym.t * BT.t) list)
        (lat : IT.t LAT.t)
    : Sym.Set.t * (Tm.t -> Tm.t)
    =
    let rec aux (xbts : (Sym.t * BT.t) list) : Tm.t -> Tm.t =
      match xbts with
      | (x, bt) :: xbts' ->
        let here = Locations.other __FUNCTION__ in
        let gt_gen = Tm.arbitrary_ AD.top () bt here in
        fun (gt : Tm.t) ->
          let gt' = aux xbts' gt in
          Tm.let_star_ ((x, gt_gen), gt') () here
      | [] -> fun gt -> gt
    in
    let xs, xbts =
      match lat with
      | Define ((x, it), _info, _) -> (Sym.Set.singleton x, IT.free_vars_bts it)
      | Resource ((x, ((P { name = Owned _; _ } as ret), bt)), _, _) ->
        (Sym.Set.singleton x, Sym.Map.add x bt (Request.free_vars_bts ret))
      | Resource ((x, (ret, _)), _, _) -> (Sym.Set.singleton x, Request.free_vars_bts ret)
      | Constraint (lc, _, _) -> (Sym.Set.empty, LC.free_vars_bts lc)
      | I it ->
        ( Sym.Set.empty,
          Sym.Map.union
            (fun _ bt1 bt2 ->
               assert (BT.equal bt1 bt2);
               Some bt1)
            (IT.free_vars_bts it)
            (oargs
             |> List.filter (fun (x, _) -> not (Sym.equal x cn_return))
             |> List.to_seq
             |> Sym.Map.of_seq) )
    in
    let xbts =
      xbts
      |> Sym.Map.filter (fun x _ -> not (Sym.Set.mem x generated))
      |> Sym.Map.bindings
    in
    let generated =
      xbts
      |> List.map fst
      |> Sym.Set.of_list
      |> Sym.Set.union generated
      |> Sym.Set.union xs
    in
    (generated, aux xbts)


  let rec transform_it_lat
            (filename : string)
            (recursive : Sym.Set.t)
            (preds : (Sym.t * Definition.Predicate.t) list)
            (name : Sym.t)
            (generated : Sym.Set.t)
            (oargs : (Sym.t * BT.t) list)
            (lat : IT.t LAT.t)
    : Tm.t m
    =
    (* Generate any free variables needed *)
    let generated, f_gt_init = transform_vars generated oargs lat in
    (* Compile *)
    let@ gt =
      match lat with
      | Define ((x, it), (loc, _), lat') ->
        let@ gt' = transform_it_lat filename recursive preds name generated oargs lat' in
        return (Tm.let_star_ ((x, Tm.return_ it () (IT.get_loc it)), gt') () loc)
      | Resource
          ((x, (P { name = Owned (ct, _); pointer; iargs = _ }, bt)), (loc, _), lat') ->
        let@ gt' = transform_it_lat filename recursive preds name generated oargs lat' in
        let gt_asgn = Tm.asgn_ ((pointer, ct), IT.sym_ (x, bt, loc), gt') () loc in
        let gt_val =
          if Sym.Set.mem x generated then
            gt_asgn
          else
            Tm.let_star_ ((x, Tm.arbitrary_ AD.top () bt loc), gt_asgn) () loc
        in
        return gt_val
      | Resource
          ((x, (P { name = PName fsym; pointer; iargs = args_its' }, bt)), (loc, _), lat')
        ->
        let here = Locations.other __LOC__ in
        let ret_bt =
          BT.Record
            (transform_oargs bt []
             |> List.map_fst (fun x -> Id.make here (Sym.pp_string x)))
        in
        (* Recurse *)
        let@ gt' =
          transform_it_lat
            filename
            recursive
            preds
            name
            generated
            oargs
            (LAT.subst
               IT.subst
               (IT.make_subst
                  [ ( x,
                      IT.recordMember_
                        ~member_bt:bt
                        (IT.sym_ (x, ret_bt, here), Identifier (here, "cn_return"))
                        here )
                  ])
               lat')
        in
        (* Add request *)
        let@ () = add_request recursive preds fsym in
        (* Get arguments *)
        let iargs = pointer :: args_its' in
        (* Build [Tm.t] *)
        let gt_call = Tm.call_ (fsym, iargs) () ret_bt loc in
        let gt_let = Tm.let_star_ ((x, gt_call), gt') () loc in
        return gt_let
      | Resource
          ( ( x,
              ( Q
                  { name = Owned (ct, _);
                    pointer;
                    q = q_sym, _;
                    q_loc;
                    step;
                    permission;
                    iargs = _
                  },
                bt ) ),
            (loc, _),
            lat' ) ->
        let@ gt' = transform_it_lat filename recursive preds name generated oargs lat' in
        let k_bt, v_bt = BT.map_bt bt in
        let gt_body =
          let sym_val = Sym.fresh_anon () in
          let it_q = IT.sym_ (q_sym, k_bt, q_loc) in
          let it_p = IT.arrayShift_ ~base:pointer ~index:it_q step loc in
          let gt_asgn =
            Tm.asgn_
              ( (it_p, ct),
                IT.sym_ (sym_val, v_bt, loc),
                Tm.return_ (IT.sym_ (sym_val, v_bt, loc)) () loc )
              ()
              loc
          in
          Tm.let_star_ ((sym_val, Tm.arbitrary_ AD.top () v_bt loc), gt_asgn) () loc
        in
        let gt_map = Tm.map_ ((q_sym, k_bt, permission), gt_body) () loc in
        let gt_let = Tm.let_star_ ((x, gt_map), gt') () loc in
        return gt_let
      | Resource
          ( ( x,
              ( Q
                  { name = PName fsym;
                    pointer;
                    q = q_sym, q_bt;
                    q_loc;
                    step;
                    permission;
                    iargs
                  },
                bt ) ),
            (loc, _),
            lat' ) ->
        (* Recurse *)
        let@ gt' = transform_it_lat filename recursive preds name generated oargs lat' in
        (* Add request *)
        let@ () = add_request recursive preds fsym in
        (* Get arguments *)
        let it_q = IT.sym_ (q_sym, q_bt, q_loc) in
        let it_p = IT.arrayShift_ ~base:pointer ~index:it_q step loc in
        let iargs = it_p :: iargs in
        (* Build [Tm.t] *)
        let _, v_bt = BT.map_bt bt in
        let gt_body =
          let here = Locations.other __LOC__ in
          let ret_bt =
            BT.Record
              (transform_oargs v_bt []
               |> List.map_fst (fun x -> Id.make here (Sym.pp_string x)))
          in
          let y = Sym.fresh_anon () in
          if BT.equal (BT.Record []) ret_bt then
            Tm.let_star_
              ( (y, Tm.call_ (fsym, iargs) () ret_bt loc),
                Tm.return_ (IT.sym_ (y, ret_bt, loc)) () loc )
              ()
              loc
          else (
            let it_ret =
              IT.recordMember_
                ~member_bt:v_bt
                (IT.sym_ (y, ret_bt, loc), Id.make here "cn_return")
                loc
            in
            Tm.let_star_
              ((y, Tm.call_ (fsym, iargs) () ret_bt loc), Tm.return_ it_ret () loc)
              ()
              loc)
        in
        let gt_map = Tm.map_ ((q_sym, q_bt, permission), gt_body) () loc in
        let gt_let = Tm.let_star_ ((x, gt_map), gt') () loc in
        return gt_let
      | Constraint (lc, (loc, _), lat') ->
        let@ gt' = transform_it_lat filename recursive preds name generated oargs lat' in
        return (Tm.assert_ (lc, gt') () loc)
      | I it ->
        let here = Locations.other __LOC__ in
        let conv_fn = List.map (fun (x, bt) -> (x, IT.sym_ (x, bt, here))) in
        let it_oargs =
          match oargs with
          | (sym, _) :: oargs' when Sym.equal sym cn_return ->
            (cn_return, it) :: conv_fn oargs'
          | _ -> conv_fn oargs
        in
        let it_ret =
          IT.record_
            (List.map_fst (fun sym -> Id.make here (Sym.pp_string sym)) it_oargs)
            here
        in
        return (Tm.return_ it_ret () (IT.get_loc it))
    in
    return (f_gt_init gt)


  let rec transform_clauses
            (filename : string)
            (recursive : Sym.Set.t)
            (preds : (Sym.t * Definition.Predicate.t) list)
            (name : Sym.t)
            (iargs : Sym.Set.t)
            (oargs : (Sym.t * BT.t) list)
            (cls : Definition.Clause.t list)
    : Tm.t m
    =
    match cls with
    | [ cl ] ->
      assert (IT.is_true cl.guard);
      transform_it_lat filename recursive preds name iargs oargs cl.packing_ft
    | cl :: cls' ->
      let it_if = cl.guard in
      let@ gt_then =
        transform_it_lat filename recursive preds name iargs oargs cl.packing_ft
      in
      let@ gt_else = transform_clauses filename recursive preds name iargs oargs cls' in
      return (Tm.ite_ (it_if, gt_then, gt_else) () cl.loc)
    | [] -> failwith ("unreachable @ " ^ __LOC__)


  let transform_pred
        (recursive_preds : Sym.Set.t)
        (preds : (Sym.t * Definition.Predicate.t) list)
        ({ filename; recursive; spec; name; iargs; oargs; body } : OptDef.t)
    : unit m
    =
    assert (Option.is_none body);
    let pred = List.assoc Sym.equal name preds in
    let@ gt =
      transform_clauses
        filename
        recursive_preds
        preds
        name
        (Sym.Set.of_list (List.map fst iargs))
        oargs
        (Option.get pred.clauses)
    in
    let gd : OptDef.t =
      { filename; recursive; spec; name; iargs; oargs; body = Some gt }
    in
    modify (OptCtx.add gd)


  let transform_spec
        (filename : string)
        (recursive : Sym.Set.t)
        (preds : (Sym.t * Definition.Predicate.t) list)
        (name : Sym.t)
        (at : 'a AT.t)
    : unit m
    =
    (* Necessary to avoid triggering special-cased logic in [CtA] w.r.t globals *)
    let rename x =
      match Sym.description x with SD_ObjectAddress x' -> Sym.fresh x' | _ -> x
    in
    let lat =
      let lat = AT.get_lat at in
      let subst =
        let loc = Locations.other __LOC__ in
        lat
        |> LAT.free_vars_bts (fun _ -> Sym.Map.empty)
        |> Sym.Map.bindings
        |> List.map (fun (x, bt) -> (x, IT.sym_ (rename x, bt, loc)))
        |> IT.make_subst
        |> LAT.subst (fun _ x -> x)
      in
      subst lat
    in
    let here = Locations.other __FUNCTION__ in
    let oargs =
      let oargs' =
        lat |> LAT.free_vars_bts (fun _ -> Sym.Map.empty) |> Sym.Map.bindings
      in
      oargs'
      @ (at
         |> AT.get_computational
         |> List.map_fst rename
         |> List.filter (fun (x, _) ->
           not
             (List.mem_assoc
                (fun x y -> String.equal (Sym.pp_string x) (Sym.pp_string y))
                x
                oargs')))
    in
    let@ gt =
      transform_it_lat
        filename
        recursive
        preds
        name
        Sym.Set.empty
        oargs
        (LAT.map (fun _ -> IT.unit_ here) lat)
    in
    let gd : OptDef.t =
      { filename;
        recursive = false;
        spec = true;
        name;
        iargs = [];
        oargs;
        body = Some gt
      }
    in
    modify (OptCtx.add gd)


  let get_recursive_preds (preds : (Sym.t * Definition.Predicate.t) list) : Sym.Set.t =
    let get_calls (pred : Definition.Predicate.t) : Sym.Set.t =
      pred.clauses
      |> Option.get
      |> List.map (fun (cl : Definition.Clause.t) -> cl.packing_ft)
      |> List.map LAT.r_resource_requests
      |> List.flatten
      |> List.map snd
      |> List.map fst
      |> List.map Request.get_name
      |> List.filter_map (fun (n : Request.name) ->
        match n with PName name -> Some name | Owned _ -> None)
      |> Sym.Set.of_list
    in
    let module G = Graph.Persistent.Digraph.Concrete (Sym) in
    let g =
      List.fold_left
        (fun g (fsym, pred) ->
           Sym.Set.fold (fun gsym g' -> G.add_edge g' fsym gsym) (get_calls pred) g)
        G.empty
        preds
    in
    let module Oper = Graph.Oper.P (G) in
    let closure = Oper.transitive_closure g in
    preds
    |> List.map fst
    |> List.filter (fun fsym -> G.mem_edge closure fsym fsym)
    |> Sym.Set.of_list


  let transform
        filename
        (preds : (Sym.t * Definition.Predicate.t) list)
        (tests : Test.t list)
    : GenContext.Make(Term.Make(AD)).t
    =
    let recursive_preds = get_recursive_preds preds in
    let context_specs =
      tests
      |> List.map (fun (test : Test.t) ->
        transform_spec
          (Option.get (Cerb_location.get_filename test.fn_loc))
          recursive_preds
          preds
          (if test.is_static then
             Sym.fresh
               (Fulminate.Utils.static_prefix filename ^ "_" ^ Sym.pp_string test.fn)
           else
             test.fn)
          test.internal)
      |> List.fold_left (fun ctx f -> execState f ctx) OptCtx.empty
    in
    let context_preds (ctx : OptCtx.t) : OptCtx.t =
      List.fold_left
        (fun ctx' ((_, gd) : _ * OptDef.t) ->
           if Option.is_some gd.body then
             ctx'
           else
             execState (transform_pred recursive_preds preds gd) ctx')
        ctx
        ctx
    in
    let rec loop (ctx : OptCtx.t) : OptCtx.t =
      let old_ctx = ctx in
      let new_ctx = context_preds ctx in
      if OptCtx.equal old_ctx new_ctx then ctx else loop new_ctx
    in
    OptCtx.drop_nones (loop context_specs)
end
