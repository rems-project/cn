module IT = IndexTerms
module LC = LogicalConstraints
module Req = Request
open Typing

let debug_constraint_failure_diagnostics
      lvl
      (model_with_q : Solver.model_with_q)
      simp_ctxt
      c
  =
  let model = fst model_with_q in
  if !Pp.print_level == 0 then
    ()
  else (
    let pp_f = IT.pp_with_eval (Solver.eval model) in
    let diag msg c =
      match (c, model_with_q) with
      | LC.T tm, _ ->
        Pp.debug lvl (lazy (Pp.item msg (IT.pp tm)));
        Pp.debug lvl (lazy (pp_f tm))
      | LC.Forall ((sym, _bt), tm), (_, [ (sym', _bt') ]) ->
        let tm' = IT.subst (IT.make_rename ~from:sym ~to_:sym') tm in
        Pp.debug lvl (lazy (Pp.item ("quantified " ^ msg) (IT.pp tm)));
        Pp.debug lvl (lazy (pp_f tm'))
      | _ ->
        Pp.warn
          (Locations.other __LOC__)
          (Pp.bold "unexpected quantifier count with model")
    in
    diag "counterexample, expanding" c;
    let c2 = Simplify.LogicalConstraints.simp simp_ctxt c in
    if LC.equal c c2 then
      ()
    else
      diag "simplified variant" c2)


module General = struct
  type one =
    { one_index : IT.t;
      value : IT.t
    }

  type many =
    { many_guard : IT.t;
      value : IT.t
    }

  type uiinfo = Error_common.situation * TypeErrors.RequestChain.t

  type case =
    | One of one
    | Many of many

  type cases = C of case list

  let add_case case (C cases) = C (cases @ [ case ])

  let cases_to_map loc (situation, requests) a_bt item_bt (C cases) =
    let here = Locations.other __LOC__ in
    let update_with_ones base_array ones =
      List.fold_left
        (fun m { one_index; value } -> IT.map_set_ m (one_index, value) here)
        base_array
        ones
    in
    let ones, manys =
      List.partition_map (function One c -> Left c | Many c -> Right c) cases
    in
    let@ base_value =
      let default = IT.default_ (BaseTypes.Map (a_bt, item_bt)) here in
      match (manys, item_bt) with
      | [ { many_guard = _; value } ], _ -> return value
      | [], _ | _, BaseTypes.Unit -> return default
      | _many, _ ->
        let@ provable = provable loc in
        (match provable (LC.T (IT.bool_ false here)) with
         | `False ->
           let@ model = model () in
           let msg ctxt =
             let orequest =
               Option.map
                 (fun r -> r.TypeErrors.RequestChain.resource)
                 (List.nth_opt (List.rev requests) 0)
             in
             let report =
               Explain.trace ctxt model Explain.{ no_ex with request = orequest }
             in
             TypeErrors.Merging_multiple_arrays { requests; situation; report }
           in
           fail (fun ctxt -> { loc; msg = msg ctxt })
         | `True -> return default)
    in
    return (update_with_ones base_value ones)


  (* this version is parametric in resource_request (defined below) to ensure the
     return-type (also parametric) is as general as possible *)
  let parametric_ftyp_args_request_step
        resource_request
        rt_subst
        loc
        (uiinfo : uiinfo)
        _original_resources
        ftyp
    =
    (* take one step of the "spine" judgement, reducing a function-type by claiming an
       argument resource or otherwise reducing towards an instantiated return-type *)
    let@ simp_ctxt = simp_ctxt () in
    let module LAT = LogicalArgumentTypes in
    match ftyp with
    | LAT.Resource ((s, (resource, _bt)), info, ftyp) ->
      let resource = Simplify.Request.simp simp_ctxt resource in
      let situation, request_chain = uiinfo in
      let step =
        TypeErrors.RequestChain.
          { resource; loc = Some (fst info); reason = Some ("arg " ^ Sym.pp_string s) }
      in
      let request_chain = step :: request_chain in
      let uiinfo = (situation, request_chain) in
      let@ o_re_oarg = resource_request loc uiinfo resource in
      (match o_re_oarg with
       | None ->
         let here = Locations.other __LOC__ in
         let@ provable = provable loc in
         (match provable (LC.T (IT.bool_ false here)) with
          | `False ->
            let@ model = model () in
            fail (fun ctxt ->
              let orequest =
                Option.map
                  TypeErrors.RequestChain.(fun (r : elem) -> r.resource)
                  (List.nth_opt (List.rev request_chain) 0)
              in
              let report =
                Explain.trace ctxt model Explain.{ no_ex with request = orequest }
              in
              let msg =
                TypeErrors.Missing_resource
                  { requests = request_chain; situation; report }
              in
              { loc; msg })
          | `True -> assert false)
       | Some ((re, Resource.O oargs), l) ->
         assert (Request.equal re resource);
         let oargs = Simplify.IndexTerms.simp simp_ctxt oargs in
         return (LAT.subst rt_subst (IT.make_subst [ (s, oargs) ]) ftyp, l))
    | Define ((s, it), _info, ftyp) ->
      let it = Simplify.IndexTerms.simp simp_ctxt it in
      return (LAT.subst rt_subst (IT.make_subst [ (s, it) ]) ftyp, [])
    | Constraint (c, info, ftyp) ->
      let@ provable = provable loc in
      Pp.(debug 9 (lazy (item "checking constraint" (LC.pp c))));
      let res = provable ~purpose:"constraint" c in
      (match res with
       | `True -> return (ftyp, [])
       | `False ->
         let@ model = model () in
         debug_constraint_failure_diagnostics 6 model simp_ctxt c;
         let@ () = Diagnostics.investigate model c in
         fail (fun ctxt ->
           let report =
             Explain.trace ctxt model Explain.{ no_ex with unproven_constraint = Some c }
           in
           { loc;
             msg =
               TypeErrors.Unproven_constraint
                 { constr = c; info; requests = snd uiinfo; report }
           }))
    | I _rt -> return (ftyp, [])


  (* TODO: check that oargs are in the same order? *)
  let rec predicate_request loc (uiinfo : uiinfo) (requested : Req.Predicate.t)
    : (Resource.predicate * Prooflog.log) option m
    =
    Pp.(debug 7 (lazy (item __LOC__ (Req.pp (P requested)))));
    let@ oarg_bt = WellTyped.oarg_bt_of_pred loc requested.name in
    let@ provable = provable loc in
    let provable = provable ~purpose:"predicate_request" in
    let@ global = get_global () in
    let@ simp_ctxt = simp_ctxt () in
    let resource_scan re ((needed : bool), oargs) =
      let continue = (Unchanged, (needed, oargs)) in
      if not needed then
        continue
      else (
        match re with
        | Req.P p', p'_oarg when Req.subsumed requested.name p'.name ->
          let here = Locations.other __LOC__ in
          let addr_eq =
            IT.(eq_ ((addr_ requested.pointer) here, addr_ p'.pointer here) here)
          in
          let iargs_eq =
            List.map2 (fun x y -> IT.eq__ x y here) requested.iargs p'.iargs
          in
          let alloc_id_eq =
            IT.(eq_ (allocId_ requested.pointer here, allocId_ p'.pointer here) here)
          in
          let eqs = addr_eq :: alloc_id_eq :: iargs_eq in
          let debug_failure model msg term =
            Pp.debug 9 (lazy (Pp.item msg (Req.pp (fst re))));
            debug_constraint_failure_diagnostics 9 model simp_ctxt (LC.T term)
          in
          (match provable (LC.T (IT.and_ eqs here)) with
           | `True ->
             Pp.debug 9 (lazy (Pp.item "used resource" (Req.pp (fst re))));
             (Deleted, (false, p'_oarg))
           | `False ->
             let model = Solver.model () in
             debug_failure
               model
               "couldn't use resource (pointer+iargs did not match)"
               (IT.and_ eqs here);
             continue)
        | _re -> continue)
    in
    let needed = true in
    let here = Locations.other __LOC__ in
    let@ needed, oarg =
      map_and_fold_resources loc resource_scan (needed, O (IT.default_ oarg_bt here))
    in
    let not_str = lazy Pp.(if needed then !^" not " else !^" ") in
    Pp.(debug 9 (Lazy.map (fun x -> !^"resource was" ^^ x ^^ !^"found") not_str));
    let@ res =
      match needed with
      | false -> return (Some ((requested, oarg), []))
      | true ->
        (match
           Pack.packing_ft ~permit_recursive:true here global provable (P requested)
         with
         | Some packing_ft ->
           let ft_pp =
             lazy (LogicalArgumentTypes.pp (fun _ -> Pp.string "resource") packing_ft)
           in
           Pp.debug 9 (Lazy.map (Pp.item "attempting to pack compound resource") ft_pp);
           let@ o, log = ftyp_args_request_for_pack loc uiinfo packing_ft in
           return (Some ((requested, Resource.O o), log))
         | None ->
           let req_pp = lazy (Req.pp (P requested)) in
           Pp.debug 9 (Lazy.map (Pp.item "no pack rule for resource, failing") req_pp);
           return None)
    in
    return res


  and qpredicate_request_aux loc uiinfo (requested : Req.QPredicate.t) =
    Pp.(debug 7 (lazy (item __LOC__ (Req.pp (Q requested)))));
    let@ provable = provable loc in
    let provable = provable ~purpose:"qpredicate_request_aux" in
    let@ simp_ctxt = simp_ctxt () in
    let needed = requested.permission in
    let step = requested.step in
    let@ needed, oarg =
      map_and_fold_resources
        loc
        (fun re (needed, oarg) ->
           let continue = (Unchanged, (needed, oarg)) in
           if IT.is_false needed then
             continue
           else (
             match re with
             | Q p', O p'_oarg
               when Req.subsumed requested.name p'.name
                    && Sctypes.equal step p'.step
                    && BaseTypes.equal (snd requested.q) (snd p'.q) ->
               let p' = Req.QPredicate.alpha_rename_ (fst requested.q) p' in
               let here = Locations.other __LOC__ in
               let pmatch =
                 (* Work-around for https://github.com/Z3Prover/z3/issues/7352 *)
                 Simplify.IndexTerms.simp simp_ctxt
                 @@ IT.eq_ (requested.pointer, p'.pointer) here
               in
               let iarg_match =
                 let eq_here x y = IT.eq_ (x, y) here in
                 IT.and_ (List.map2 eq_here requested.iargs p'.iargs) here
               in
               let took =
                 IT.and_ [ iarg_match; requested.permission; p'.permission ] here
               in
               (match provable (LC.Forall (requested.q, IT.not_ took here)) with
                | `True -> continue
                | `False ->
                  (match provable (LC.T pmatch) with
                   | `True ->
                     Pp.debug 9 (lazy (Pp.item "used resource" (Req.pp (fst re))));
                     let open IT in
                     let needed' =
                       [ needed; not_ (and_ [ iarg_match; p'.permission ] here) here ]
                     in
                     let permission' =
                       [ p'.permission; not_ (and_ [ iarg_match; needed ] here) here ]
                     in
                     let oarg =
                       add_case (Many { many_guard = took; value = p'_oarg }) oarg
                     in
                     ( Changed
                         (Q { p' with permission = and_ permission' here }, O p'_oarg),
                       (Simplify.IndexTerms.simp simp_ctxt (and_ needed' here), oarg) )
                   | `False ->
                     let model = Solver.model () in
                     Pp.debug
                       9
                       (lazy (Pp.item "couldn't use q-resource" (Req.pp (fst re))));
                     debug_constraint_failure_diagnostics 9 model simp_ctxt (LC.T pmatch);
                     continue))
             | _re -> continue))
        (needed, C [])
    in
    let here = Locations.other __LOC__ in
    let@ needed, oarg, l =
      let@ movable_indices = get_movable_indices () in
      let module Eff = Effectful.Make (Typing) in
      Eff.ListM.fold_rightM
        (fun (predicate_name, index) (needed, oarg, l) ->
           let continue = return (needed, oarg, l) in
           if
             (not (IT.is_false needed))
             && Req.subsumed requested.name predicate_name
             && BaseTypes.equal (snd requested.q) (IT.get_bt index)
           then (
             let su = IT.make_subst [ (fst requested.q, index) ] in
             let needed_at_index = IT.subst su needed in
             match provable (LC.T needed_at_index) with
             | `False -> continue
             | `True ->
               let@ c = get_typing_context () in
               let pointer =
                 IT.(arrayShift_ ~base:requested.pointer ~index requested.step here)
               in
               let sub_req : Req.Predicate.t =
                 { name = requested.name;
                   pointer;
                   iargs = List.map (IT.subst su) requested.iargs
                 }
               in
               let@ o_re_index = predicate_request loc uiinfo sub_req in
               (match o_re_index with
                | None -> continue
                | Some (((_p', O p'_oarg) as rr), l') ->
                  let oarg = add_case (One { one_index = index; value = p'_oarg }) oarg in
                  let sym, bt' = requested.q in
                  let needed' =
                    IT.(and_ [ needed; ne__ (sym_ (sym, bt', here)) index here ] here)
                  in
                  let@ c' = get_typing_context () in
                  let hints =
                    if Prooflog.is_enabled () then
                      Prooflog.PredicateRequest (c, fst uiinfo, sub_req, rr, l', c') :: l
                    else
                      []
                  in
                  return (needed', oarg, hints)))
           else
             continue)
        movable_indices
        (needed, oarg, [])
    in
    let nothing_more_needed = LC.forall_ requested.q (IT.not_ needed here) in
    Pp.debug 9 (lazy (Pp.item "checking resource remainder" (LC.pp nothing_more_needed)));
    let holds = provable nothing_more_needed in
    match holds with
    | `True -> return (Some (oarg, l))
    | `False ->
      let@ model = model () in
      debug_constraint_failure_diagnostics 9 model simp_ctxt nothing_more_needed;
      return None


  and qpredicate_request loc uiinfo (requested : Req.QPredicate.t) =
    let@ o_oarg = qpredicate_request_aux loc uiinfo requested in
    let@ oarg_item_bt = WellTyped.oarg_bt_of_pred loc requested.name in
    match o_oarg with
    | None -> return None
    | Some (oarg, l) ->
      let@ oarg = cases_to_map loc uiinfo (snd requested.q) oarg_item_bt oarg in
      let r =
        Req.QPredicate.
          { name = requested.name;
            pointer = requested.pointer;
            q = requested.q;
            q_loc = requested.q_loc;
            step = requested.step;
            permission = requested.permission;
            iargs = requested.iargs
          }
      in
      return (Some ((r, Resource.O oarg), l))


  and ftyp_args_request_for_pack loc uiinfo ftyp =
    (* record the resources now, so errors are raised with all the resources present,
       rather than those that remain after some arguments are claimed *)
    let@ original_resources = all_resources loc in
    let rec loop ftyp l =
      match ftyp with
      | LogicalArgumentTypes.I rt -> return (rt, l)
      | _ ->
        let@ ftyp, l' =
          parametric_ftyp_args_request_step
            (resource_request ~simplify_prooflog:true)
            IT.subst
            loc
            uiinfo
            original_resources
            ftyp
        in
        loop ftyp (l @ l')
    in
    loop ftyp []


  and resource_request ?(simplify_prooflog = false) loc uiinfo (request : Req.t)
    : (Resource.t * Prooflog.log) option m
    =
    match request with
    | P request ->
      let@ c = get_typing_context () in
      let@ result = predicate_request loc uiinfo request in
      let@ c' = get_typing_context () in
      let@ simp_ctxt = simp_ctxt () in
      return
        (Option.map
           (fun ((p, Resource.O o), l) ->
              let hints =
                if Prooflog.is_enabled () then (
                  let p, o =
                    if not simplify_prooflog then
                      (p, o)
                    else (
                      let p = Simplify.Request.Predicate.simp simp_ctxt p in
                      let o = Simplify.IndexTerms.simp simp_ctxt o in
                      (p, o))
                  in
                  [ Prooflog.PredicateRequest
                      (c, fst uiinfo, request, (p, Resource.O o), l, c')
                  ])
                else
                  []
              in
              ((Req.P p, Resource.O o), hints))
           result)
    | Q request ->
      let@ result = qpredicate_request loc uiinfo request in
      return (Option.map (fun ((q, o), l) -> ((Req.Q q, o), l)) result)


  (*
     This is called directly from check.ml. Maybe it should be in Special, as
     predicate_request?
  *)
  let ftyp_args_request_step rt_subst loc situation original_resources ftyp =
    let@ rt, l =
      parametric_ftyp_args_request_step
        (resource_request ~simplify_prooflog:false)
        rt_subst
        loc
        situation
        original_resources
        ftyp
    in
    (* We started with top-level call of ftyp_args_request_step, so we need to
       record the resource inference steps for the inner calls. They not nested
       under anything, so we need to record them separately. *)
    if Prooflog.is_enabled () then
      List.iter Prooflog.record_resource_inference_step l
    else
      ();
    return rt


  let predicate_request loc uiinfo requested =
    let start_time = Pp.time_start () in
    let@ result = predicate_request loc uiinfo requested in
    Pp.time_end "predicate_request" start_time;
    return result


  let qpredicate_request loc uiinfo requested =
    let start_time = Pp.time_start () in
    let@ result = qpredicate_request loc uiinfo requested in
    Pp.time_end "qpredicate_request" start_time;
    return result
end

module Special = struct
  let fail_missing_resource loc (situation, requests) =
    let here = Locations.other __LOC__ in
    let@ provable = provable loc in
    match provable (LC.T (IT.bool_ false here)) with
    | `False ->
      let@ model = model () in
      fail (fun ctxt ->
        let orequest =
          Option.map
            TypeErrors.RequestChain.(fun (r : elem) -> r.resource)
            (List.nth_opt (List.rev requests) 0)
        in
        let report = Explain.trace ctxt model Explain.{ no_ex with request = orequest } in
        let msg = TypeErrors.Missing_resource { requests; situation; report } in
        { loc; msg })
    | `True -> assert false


  let predicate_request loc situation (request, oinfo) =
    let requests =
      [ TypeErrors.RequestChain.
          { resource = P request;
            loc = Option.map fst oinfo;
            reason = Option.map snd oinfo
          }
      ]
    in
    let uiinfo = (situation, requests) in
    let@ c = get_typing_context () in
    let@ result = General.predicate_request loc uiinfo request in
    match result with
    | Some (r, log) ->
      let@ c' = get_typing_context () in
      if Prooflog.is_enabled () then
        Prooflog.record_resource_inference_step
          (Prooflog.PredicateRequest (c, fst uiinfo, request, r, log, c'))
      else
        ();
      return r
    | None -> fail_missing_resource loc uiinfo


  let has_predicate loc situation (request, oinfo) =
    let@ result = sandbox @@ predicate_request loc situation (request, oinfo) in
    return (Result.is_ok result)


  (** This function checks whether [ptr1] belongs to a live allocation. It
      searches the context (without modification) for either an Alloc(p) or an
      Owned(p) such that (alloc_id) p == (alloc_id) ptr. *)
  let check_live_alloc reason loc ptr =
    let module Ans = struct
      type t =
        | Found
        | No_res
        | Model of (Solver.model_with_q * IT.t)
    end
    in
    let here = Locations.other __LOC__ in
    let alloc_id_matches found res_ptr =
      let@ found in
      match found with
      | Ans.Found -> return Ans.Found
      | No_res | Model _ ->
        let constr = IT.(eq_ (allocId_ ptr here, allocId_ res_ptr here) here) in
        let@ provable = provable loc in
        (match provable ~purpose:"check_live_alloc" (LC.T constr) with
         | `True -> return Ans.Found
         | `False ->
           let@ model = model () in
           return (Ans.Model (model, constr)))
    in
    let f res found =
      let found =
        match res with
        | Req.Q _, _ -> found
        | Req.P { name = Owned _; pointer; iargs = _ }, _ ->
          alloc_id_matches found pointer
        | Req.P { name = PName name; pointer; iargs = _ }, _ ->
          if Sym.equal name Alloc.Predicate.sym then
            alloc_id_matches found pointer
          else
            found
      in
      (Unchanged, found)
    in
    let@ found = map_and_fold_resources loc f (return Ans.No_res) in
    let@ found in
    match found with
    | Ans.Found -> return ()
    | No_res ->
      fail (fun _ctxt ->
        let msg =
          (* probably we want Report.report to work also if no model is
             available *)
          TypeErrors.Allocation_not_live { reason; ptr; maybe_report = None }
        in
        { loc; msg })
    | Model (model, constr) ->
      fail (fun ctxt ->
        let report =
          Explain.trace
            ctxt
            model
            Explain.{ no_ex with unproven_constraint = Some (LC.T constr) }
        in
        let msg =
          TypeErrors.Allocation_not_live { reason; ptr; maybe_report = Some report }
        in
        { loc; msg })


  let qpredicate_request loc situation (request, oinfo) =
    let requests =
      [ TypeErrors.RequestChain.
          { resource = Q request;
            loc = Option.map fst oinfo;
            reason = Option.map snd oinfo
          }
      ]
    in
    let uiinfo = (situation, requests) in
    let@ result = General.qpredicate_request loc uiinfo request in
    match result with
    | Some (r, log) ->
      (* We started with top-level call of qpredicate_request, so we need to
         record the resource inference steps for the inner calls. They not
         nested under anything, so we need to record them separately. *)
      if Prooflog.is_enabled () then
        List.iter Prooflog.record_resource_inference_step log
      else
        ();
      return r
    | None -> fail_missing_resource loc uiinfo
end
