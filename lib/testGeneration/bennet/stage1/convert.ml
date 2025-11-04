module IT = IndexTerms
module BT = BaseTypes
module AT = ArgumentTypes
module LC = LogicalConstraints
module LAT = LogicalArgumentTypes
module Config = TestGenConfig
module CF = Cerb_frontend
module A = CF.AilSyntax

module Make (AD : Domain.T) = struct
  module OptCtx = GenContext.MakeOptional (Term.Make (AD))
  module OptDef = GenDefinitions.MakeOptional (Term.Make (AD))
  module Tm = Term.Make (AD)

  module State : sig
    type s = OptCtx.t

    type 'a m

    val modify : (s -> s) -> unit m

    val ( let@ ) : 'a m -> ('a -> 'b m) -> 'b m

    val return : 'a -> 'a m

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

    let execState f s = snd (f s)
  end

  open State

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
        oarg = snd pred.oarg;
        c_types = None;
        body = None
      }
    in
    modify (OptCtx.add gd)


  let transform_vars (generated : Sym.Set.t) (_oarg : BT.t) (lat : IT.t LAT.t)
    : Sym.Set.t * (Tm.t -> Tm.t)
    =
    let rec aux (xbts : (Sym.t * BT.t) list) : Tm.t -> Tm.t =
      match xbts with
      | (x, bt) :: xbts' ->
        let here = Locations.other __FUNCTION__ in
        let gt_gen = Tm.arbitrary_ () bt here in
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
      | I it -> (Sym.Set.empty, IT.free_vars_bts it)
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
            (oarg : BT.t)
            (lat : IT.t LAT.t)
    : Tm.t m
    =
    (* Generate any free variables needed *)
    let generated, f_gt_init = transform_vars generated oarg lat in
    (* Compile *)
    let@ gt =
      match lat with
      | Define ((x, it), (loc, _), lat') ->
        let@ gt' = transform_it_lat filename recursive preds name generated oarg lat' in
        return (Tm.let_star_ ((x, Tm.return_ it () (IT.get_loc it)), gt') () loc)
      | Resource
          ((x, (P { name = Owned (ct, _); pointer; iargs = _ }, bt)), (loc, _), lat') ->
        let@ gt' = transform_it_lat filename recursive preds name generated oarg lat' in
        let gt_asgn = Tm.asgn_ ((pointer, ct), IT.sym_ (x, bt, loc), gt') () loc in
        let gt_val =
          if Sym.Set.mem x generated then
            gt_asgn
          else
            Tm.let_star_ ((x, Tm.arbitrary_ () bt loc), gt_asgn) () loc
        in
        return gt_val
      | Resource
          ((x, (P { name = PName fsym; pointer; iargs = args_its' }, bt)), (loc, _), lat')
        ->
        (* Recurse *)
        let@ gt' = transform_it_lat filename recursive preds name generated oarg lat' in
        (* Add request *)
        let@ () = add_request recursive preds fsym in
        (* Get arguments *)
        let iargs = pointer :: args_its' in
        (* Build [Tm.t] *)
        let gt_call = Tm.call_ (fsym, iargs) () bt loc in
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
        let@ gt' = transform_it_lat filename recursive preds name generated oarg lat' in
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
          Tm.let_star_ ((sym_val, Tm.arbitrary_ () v_bt loc), gt_asgn) () loc
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
        let@ gt' = transform_it_lat filename recursive preds name generated oarg lat' in
        (* Add request *)
        let@ () = add_request recursive preds fsym in
        (* Get arguments *)
        let it_q = IT.sym_ (q_sym, q_bt, q_loc) in
        let it_p = IT.arrayShift_ ~base:pointer ~index:it_q step loc in
        let iargs = it_p :: iargs in
        (* Build [Tm.t] *)
        let _, v_bt = BT.map_bt bt in
        let gt_body =
          let y = Sym.fresh_anon () in
          Tm.let_star_
            ( (y, Tm.call_ (fsym, iargs) () v_bt loc),
              Tm.return_ (IT.sym_ (y, v_bt, loc)) () loc )
            ()
            loc
        in
        let gt_map = Tm.map_ ((q_sym, q_bt, permission), gt_body) () loc in
        let gt_let = Tm.let_star_ ((x, gt_map), gt') () loc in
        return gt_let
      | Constraint (lc, (loc, _), lat') ->
        let@ gt' = transform_it_lat filename recursive preds name generated oarg lat' in
        return (Tm.assert_ (lc, gt') () loc)
      | I it -> return (Tm.return_ it () (IT.get_loc it))
    in
    return (f_gt_init gt)


  let rec transform_clauses
            (filename : string)
            (recursive : Sym.Set.t)
            (preds : (Sym.t * Definition.Predicate.t) list)
            (name : Sym.t)
            (iargs : Sym.Set.t)
            (oarg : BT.t)
            (cls : Definition.Clause.t list)
    : Tm.t m
    =
    match cls with
    | [ cl ] ->
      assert (IT.is_true cl.guard);
      transform_it_lat filename recursive preds name iargs oarg cl.packing_ft
    | cl :: cls' ->
      let it_if = cl.guard in
      let@ gt_then =
        transform_it_lat filename recursive preds name iargs oarg cl.packing_ft
      in
      let@ gt_else = transform_clauses filename recursive preds name iargs oarg cls' in
      return (Tm.ite_ (it_if, gt_then, gt_else) () cl.loc)
    | [] -> failwith ("unreachable @ " ^ __LOC__)


  let transform_pred
        (recursive_preds : Sym.Set.t)
        (preds : (Sym.t * Definition.Predicate.t) list)
        ({ filename; recursive; spec; name; iargs; oarg; c_types; body } : OptDef.t)
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
        oarg
        (Option.get pred.clauses)
    in
    let gd : OptDef.t =
      { filename; recursive; spec; name; iargs; oarg; c_types; body = Some gt }
    in
    modify (OptCtx.add gd)


  let transform_spec
        (filename : string)
        (recursive : Sym.Set.t)
        (preds : (Sym.t * Definition.Predicate.t) list)
        (name : Sym.t)
        (c_types : (Sym.t * CF.Ctype.ctype) list)
        (at : 'a AT.t)
    : unit m
    =
    let lat = AT.get_lat at in
    let here = Locations.other __FUNCTION__ in
    let vars =
      let vars' = lat |> LAT.free_vars_bts (fun _ -> Sym.Map.empty) |> Sym.Map.bindings in
      vars'
      @ (at
         |> AT.get_computational
         |> List.filter (fun (x, _) ->
           not
             (List.mem_assoc
                (fun x y -> String.equal (Sym.pp_string x) (Sym.pp_string y))
                x
                vars')))
    in
    let iargs, oarg =
      if Config.is_symbolic_enabled () then
        (* Symbolic mode: take arguments as inputs, return unit *)
        (vars, BT.Unit)
      else (* Concrete mode: no inputs, return arguments as outputs *)
        (
        let oarg_bt =
          match vars with
          | [] -> BT.Unit
          | _ ->
            (* Always wrap arguments in a record, even for single argument *)
            BT.Record
              (vars
               |> List.map (fun (x, bt) -> (Id.make here (Sym.pp_string x), bt))
               |> List.sort (fun (id, _) (id', _) -> Id.compare id id'))
        in
        ([], oarg_bt))
    in
    let ret_it =
      match oarg with
      | BT.Unit -> IT.unit_ here
      | BT.Record fields ->
        (* Construct a record with all the vars *)
        IT.record_
          (List.map
             (fun (id, bt) ->
                (* Find the var corresponding to this field *)
                let field_name = Id.get_string id in
                let x, _ =
                  List.find (fun (x, _) -> String.equal (Sym.pp_string x) field_name) vars
                in
                (id, IT.sym_ (x, bt, here)))
             fields)
          here
      | _ -> failwith "Unexpected oarg type in transform_spec"
    in
    let@ gt =
      transform_it_lat
        filename
        recursive
        preds
        name
        (Sym.Set.of_list (List.map fst iargs))
        oarg
        (LAT.map (fun _ -> ret_it) lat)
    in
    let gd : OptDef.t =
      { filename;
        recursive = false;
        spec = true;
        name;
        iargs;
        oarg;
        c_types = Some c_types;
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
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        filename
        (globals : Sym.t list)
        (preds : (Sym.t * Definition.Predicate.t) list)
        (tests : Test.t list)
    : GenContext.Make(Term.Make(AD)).t
    =
    (* Necessary to avoid triggering special-cased logic in [CtA] w.r.t globals *)
    let globals_subst =
      globals
      |> List.fold_left
           (fun acc x ->
              match Sym.description x with
              | SD_ObjectAddress x' -> (x, `Rename (Sym.fresh x')) :: acc
              | _ -> acc)
           []
      |> Subst.make IT.free_vars_with_rename
    in
    let preds =
      List.map_snd
        (fun ({ clauses; _ } as pred : Definition.Predicate.t) ->
           { pred with
             clauses =
               (let open Option in
                let@ clauses = clauses in
                return (List.map (Definition.Clause.subst globals_subst) clauses))
           })
        preds
    in
    let recursive_preds = get_recursive_preds preds in
    let context_specs =
      tests
      |> List.map (fun (test : Test.t) ->
        (* Look up C types using the original function name *)
        let c_types =
          match
            ( List.assoc_opt Sym.equal test.fn sigma.declarations,
              List.assoc_opt Sym.equal test.fn sigma.function_definitions )
          with
          | ( Some (_, _, A.Decl_function (_, _, arg_ctypes, _, _, _)),
              Some (_, _, _, param_names, _) ) ->
            List.combine param_names arg_ctypes
            |> List.map (fun (param_name, (_, ctype, _)) -> (param_name, ctype))
          | _ ->
            failwith
              (Printf.sprintf
                 "Spec function %s not found in sigma.declarations or \
                  sigma.function_definitions"
                 (Sym.pp_string test.fn))
        in
        let name =
          if test.is_static then
            Sym.fresh
              (Fulminate.Utils.static_prefix filename ^ "_" ^ Sym.pp_string test.fn)
          else
            test.fn
        in
        test.internal
        |> AT.subst (fun _ x -> x) globals_subst
        |> transform_spec
             (Option.get (Cerb_location.get_filename test.fn_loc))
             recursive_preds
             preds
             name
             c_types)
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
