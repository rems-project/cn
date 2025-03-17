module BT = BaseTypes
module IT = IndexTerms
module Req = Request
module Loc = Locations
module LAT = LogicalArgumentTypes
module LC = LogicalConstraints
module Res = Resource
module Def = Definition
module C = Context

module Result = struct
  type reason =
    | No
    | Unknown
    | Err
  [@@deriving eq]

  type ('a, 'b) t = ('a, reason * 'b) Result.t

  let return x = Ok x

  let no x = Error (No, x)

  let unknown x = Error (Unknown, x)

  let error x = Error (Err, x)

  let ( let@ ) = Result.bind

  let is_no r = Result.is_error r && equal_reason No (fst (Result.get_error r))

  (* Gives a single canonical result *)
  let combine_results (e : 'b) (results : ('a, 'b) t list) : ('a, 'b) t =
    match results with
    | [] -> Error (Err, e)
    | h :: t ->
      let combine (acc : ('a, 'b) t) (res : ('a, 'b) t) =
        match (acc, res) with
        | Ok l, _ -> Ok l
        | _, Ok l -> Ok l
        | Error (Err, s), _ -> Error (Err, s)
        | _, Error (Err, s) -> Error (Err, s)
        | Error (Unknown, s), _ -> Error (Unknown, s)
        | _, Error (Unknown, s) -> Error (Unknown, s)
        | Error (No, s), _ -> Error (No, s)
      in
      List.fold_left combine h t
end

(** Infrastructure for checking if a countermodel satisfies a predicate **)
(* The core function is `check_pred`, which, given a predicate and a term
   matching the return type of the predicate, checks if the term is in the
   range of that predicate. The term here is referred to as a "candidate".
   The algorithm works by "walking backward" over the predicate: it
   compares the candidate to the `return` expression of the predicate. If they
   are compatible (e.g., use the same constructor), then this implies candidate
   values for the free variables in the `return` expression.
   (`get_var_cands` identifies these candidates.) These new
   candidates are then recursively checked against the definitions of those
   variables. *)

(* let pp_check_result =
   pp_result_with_data (Pp.list (fun lc -> !^"\n" ^^^ LC.pp lc)) (fun d -> d) *)
(* Issue #900 *)

(* Type of nonterminal lines in a predicate clause.
   Corresponds to packing_ft *)
type def_line =
  | DefineL of (Sym.t * IT.t) * Loc.info
  | ResourceL of (Sym.t * (Req.t * BT.t)) * Loc.info

(* Takes in:
   - exps and exps', two lists
   - eq, an equality function for the type of the elements of the lists
   - f, which, given two elements from the same index in the two lists,
     uses them to construct a symbol map
     Applies f across the lists and combines the results into one map
     Safely fails if:
   - the lists have different lengths
   - different indexes produce incompatible maps
   - f safely fails on any pair of elements *)
let map_from_lists f eq exps exps' =
  let open Result in
  (* Take the union of two symbol maps,
     failing on any key that is in both maps with different values *)
  let merge eq m1 m2 =
    let comb k v acc =
      let@ macc = acc in
      match Sym.Map.find_opt k macc with
      | Some v' -> if eq v v' then acc else no (Pp.( !^ ) "Incompatible list elements")
      | None -> return (Sym.Map.add k v macc)
    in
    Sym.Map.fold comb m1 (return m2)
  in
  let merge_r_maps r_acc (exp1, exp1') =
    let@ acc = r_acc in
    let@ combined = f exp1 exp1' in
    merge eq acc combined
  in
  let zipped = List.combine exps exps' in
  List.fold_left merge_r_maps (return Sym.Map.empty) zipped


(* Match an expression with free variables against a candidate returned by the solver to
   get candidates for each of those free variables *)
let rec get_var_cands (exp : IT.t) (candidate : IT.t)
  : (IT.t Sym.Map.t, Pp.document) Result.t
  =
  let open Pp in
  let map_from_IT_lists = map_from_lists get_var_cands IT.equal in
  let sort_by_discard_fst compare l =
    List.map snd (List.sort (fun p1 p2 -> compare (fst p1) (fst p2)) l)
  in
  let sort_by_id = sort_by_discard_fst Id.compare in
  let sort_by_pattern = sort_by_discard_fst (Terms.compare_pattern BT.compare) in
  let map_with_guard_unknown g l1 l1' =
    if g then
      map_from_IT_lists l1 l1'
    else
      Result.unknown (Pp.bool g ^^^ !^" not satisfied")
  in
  let map_with_guard_no g l1 l1' =
    if g then map_from_IT_lists l1 l1' else Result.no (Pp.bool g ^^^ !^" not satisfied")
  in
  let default =
    Result.unknown
      (!^"Different CN constructors for " ^^^ IT.pp exp ^^^ !^" and " ^^^ IT.pp candidate)
  in
  match (IT.get_term exp, IT.get_term candidate) with
  | Const c, Const c' -> map_with_guard_no (IT.equal_const c c') [] []
  | Sym v, _' -> Result.return (Sym.Map.add v candidate Sym.Map.empty)
  | Unop (op, exp1), Unop (op', exp1') ->
    map_with_guard_unknown (IT.equal_unop op op') [ exp1 ] [ exp1' ]
  | Binop (op, exp1, exp2), Binop (op', exp1', exp2') ->
    map_with_guard_unknown (IT.equal_binop op op') [ exp1; exp2 ] [ exp1'; exp2' ]
  | ITE (exp1, exp2, exp3), ITE (exp1', exp2', exp3') ->
    map_from_IT_lists [ exp1; exp2; exp3 ] [ exp1'; exp2'; exp3' ]
  | EachI ((z1, (v, bty), z2), exp1), EachI ((z1', (v', bty'), z2'), exp1') ->
    map_with_guard_unknown
      (z1 = z1' && Sym.equal v v' && BT.equal bty bty' && z2 = z2')
      [ exp1 ]
      [ exp1' ]
  | Tuple exps, Tuple exps' -> map_from_IT_lists exps exps'
  | NthTuple (n, exp1), NthTuple (n', exp1') ->
    map_with_guard_unknown (n = n') [ exp1 ] [ exp1' ]
  | Struct (name, fields), Struct (name', fields') ->
    map_with_guard_no (Sym.equal name name') (sort_by_id fields) (sort_by_id fields')
  | StructMember (exp1, id), StructMember (exp1', id') ->
    map_with_guard_unknown (Id.equal id id') [ exp1 ] [ exp1' ]
  | StructUpdate ((exp1, id), exp2), StructUpdate ((exp1', id'), exp2') ->
    map_with_guard_unknown (Id.equal id id') [ exp1; exp2 ] [ exp1'; exp2' ]
  | Record fields, Record fields' ->
    map_from_IT_lists (sort_by_id fields) (sort_by_id fields')
  | RecordMember (exp1, id), RecordMember (exp1', id') ->
    map_with_guard_unknown (Id.equal id id') [ exp1 ] [ exp1' ]
  | RecordUpdate ((exp1, id), exp2), RecordUpdate ((exp1', id'), exp2') ->
    map_with_guard_unknown (Id.equal id id') [ exp1; exp2 ] [ exp1'; exp2' ]
  | Constructor (name, args), Constructor (name', args') ->
    map_with_guard_no (Sym.equal name name') (sort_by_id args) (sort_by_id args')
  | MemberShift (exp1, v, id), MemberShift (exp1', v', id') ->
    map_with_guard_unknown (Sym.equal v v' && Id.equal id id') [ exp1 ] [ exp1' ]
  | ArrayShift { base; ct; index }, ArrayShift { base = base'; ct = ct'; index = index' }
    ->
    map_with_guard_unknown (Sctypes.equal ct ct') [ base; index ] [ base'; index' ]
  | CopyAllocId { addr = exp1; loc = exp2 }, CopyAllocId { addr = exp1'; loc = exp2' } ->
    map_from_IT_lists [ exp1; exp2 ] [ exp1'; exp2' ]
  | HasAllocId exp1, HasAllocId exp1' -> get_var_cands exp1 exp1'
  | SizeOf cty, SizeOf cty' -> map_with_guard_unknown (Sctypes.equal cty cty') [] []
  | OffsetOf (v, id), OffsetOf (v', id') ->
    map_with_guard_unknown (Sym.equal v v' && Id.equal id id') [] []
  | Nil bty, Nil bty' -> map_with_guard_no (BT.equal bty bty') [] []
  | Cons (h, tl), Cons (h', tl') -> map_from_IT_lists [ h; tl ] [ h'; tl' ]
  | Head l, Head l' -> get_var_cands l l'
  | Tail l, Tail l' -> get_var_cands l l'
  | NthList (exp1, exp2, exp3), NthList (exp1', exp2', exp3') ->
    map_from_IT_lists [ exp1; exp2; exp3 ] [ exp1'; exp2'; exp3' ]
  | ArrayToList (exp1, exp2, exp3), ArrayToList (exp1', exp2', exp3') ->
    map_from_IT_lists [ exp1; exp2; exp3 ] [ exp1'; exp2'; exp3' ]
  | Representable (cty, exp1), Representable (cty', exp1') ->
    map_with_guard_unknown (Sctypes.equal cty cty') [ exp1 ] [ exp1' ]
  | Good (cty, exp1), Good (cty', exp1') ->
    map_with_guard_unknown (Sctypes.equal cty cty') [ exp1 ] [ exp1' ]
  | Aligned { t = exp1; align = exp2 }, Aligned { t = exp1'; align = exp2' } ->
    map_from_IT_lists [ exp1; exp2 ] [ exp1'; exp2' ]
  | WrapI (ity, exp1), WrapI (ity', exp1') ->
    map_with_guard_unknown
      (Cerb_frontend.IntegerType.integerTypeEqual ity ity')
      [ exp1 ]
      [ exp1' ]
  | MapConst (bty, exp1), MapConst (bty', exp1') ->
    map_with_guard_unknown (BT.equal bty bty') [ exp1 ] [ exp1' ]
  | MapSet (exp1, exp2, exp3), MapSet (exp1', exp2', exp3') ->
    map_from_IT_lists [ exp1; exp2; exp3 ] [ exp1'; exp2'; exp3' ]
  | MapGet (exp1, exp2), MapGet (exp1', exp2') ->
    map_from_IT_lists [ exp1; exp2 ] [ exp1'; exp2' ]
  | MapDef ((v, bty), exp1), MapDef ((v', bty'), exp1') ->
    map_with_guard_unknown (Sym.equal v v' && BT.equal bty bty') [ exp1 ] [ exp1' ]
  | Apply (v, exps), Apply (v', exps') ->
    map_with_guard_unknown (Sym.equal v v') exps exps'
  | Let ((v, exp1), exp2), Let ((v', exp1'), exp2') ->
    map_with_guard_unknown (Sym.equal v v') [ exp1; exp2 ] [ exp1'; exp2' ]
  | Match (exp1, pats), Match (exp1', pats') ->
    map_from_IT_lists (exp1 :: sort_by_pattern pats) (exp1' :: sort_by_pattern pats')
  | Cast (bt, exp1), Cast (bt', exp1') ->
    map_with_guard_unknown (BT.equal bt bt') [ exp1 ] [ exp1' ]
  (* included so the compiler will catch any missing new constructors *)
  | Const _, _ -> default
  | Unop _, _ -> default
  | Binop _, _ -> default
  | ITE _, _ -> default
  | EachI _, _ -> default
  | Tuple _, _ -> default
  | NthTuple _, _ -> default
  | Struct _, _ -> default
  | StructMember _, _ -> default
  | StructUpdate _, _ -> default
  | Record _, _ -> default
  | RecordMember _, _ -> default
  | RecordUpdate _, _ -> default
  | Constructor _, _ -> default
  | MemberShift _, _ -> default
  | ArrayShift _, _ -> default
  | CopyAllocId _, _ -> default
  | HasAllocId _, _ -> default
  | SizeOf _, _ -> default
  | OffsetOf _, _ -> default
  | Nil _, _ -> default
  | Cons _, _ -> default
  | Head _, _ -> default
  | Tail _, _ -> default
  | NthList _, _ -> default
  | ArrayToList _, _ -> default
  | Representable _, _ -> default
  | Good _, _ -> default
  | Aligned _, _ -> default
  | WrapI _, _ -> default
  | MapConst _, _ -> default
  | MapSet _, _ -> default
  | MapGet _, _ -> default
  | MapDef _, _ -> default
  | Apply _, _ -> default
  | Let _, _ -> default
  | Match _, _ -> default
  | Cast _, _ -> default


let rec organize_lines_aux
          (lines : LAT.packing_ft)
          (defs : def_line Sym.Map.t)
          (lcs : LC.t list)
  : IT.t * def_line Sym.Map.t * LC.t list
  =
  match lines with
  | Define ((v, it), i, next) ->
    let ln = DefineL ((v, it), i) in
    let new_defs = Sym.Map.add v ln defs in
    organize_lines_aux next new_defs lcs
  | Resource ((v, (rt, bt)), i, next) ->
    let ln = ResourceL ((v, (rt, bt)), i) in
    let new_defs = Sym.Map.add v ln defs in
    organize_lines_aux next new_defs lcs
  | Constraint (lc, _, next) -> organize_lines_aux next defs (lc :: lcs)
  | I it -> (it, defs, lcs)


(* Sort lines into the returned expression, a map of variables to their defining lines, and a list of constraints *)
let organize_lines (lines : LAT.packing_ft) : IT.t * def_line Sym.Map.t * LC.t list =
  organize_lines_aux lines Sym.Map.empty []


(* ask the solver if the given set of constraints is satisfiable *)
let ask_solver g lcs =
  let here = Locations.other __LOC__ in
  let simp_ctxt =
    Simplify.{ global = g; values = Sym.Map.empty; simp_hook = (fun _ -> None) }
  in
  let s = Solver.make g in
  List.fold_right (fun lc _ -> Solver.add_assumption s g lc) lcs ();
  let solver_res =
    Solver.provableWithUnknown
      ~loc:here
      ~solver:s
      ~assumptions:(LC.Set.of_list lcs)
      ~simp_ctxt
      (LC.T (IT.bool_ false here))
  in
  let res =
    match solver_res with
    | `True -> Result.no (Pp.( !^ ) "Solver returned No.")
    | `Unknown -> Result.unknown (Pp.( !^ ) "Solver returned Unknown.")
    | `False ->
      Result.unknown
        (Pp.( !^ ) "Solver returned No, but without some definitions available.")
  in
  res


let pair_to_lc (ps : IT.t * IT.t) : LogicalConstraints.t =
  let here = Locations.other __LOC__ in
  LC.T (IT.eq_ (fst ps, snd ps) here)


(* convert a list of variable assignments to equality constraints *)
let convert_symmap_to_lcs (m : IT.t Sym.Map.t) : LogicalConstraints.t list =
  let here = Locations.other __LOC__ in
  let kvs = Sym.Map.bindings m in
  List.map (fun (k, v) -> pair_to_lc (IT.IT (IT.Sym k, IT.get_bt v, here), v)) kvs


(* check if a candidate term could have been the output of a given predicate *)
let rec check_pred
          (name : Sym.t)
          (def : Def.Predicate.t)
          (candidate : IT.t)
          (ctxt : C.t)
          (iarg_vals : IT.t list)
          (term_vals : (IT.t * IT.t) list)
  : (LC.t list, Pp.document) Result.t
  =
  (* ensure candidate type matches output type of predicate *)
  assert (BT.equal (IT.get_bt candidate) def.oarg_bt);
  let open Pp in
  match def.clauses with
  | None -> Result.unknown (!^"Predicate" ^^^ Sym.pp name ^^^ !^"is uninterpreted. ")
  | Some clauses ->
    (* add negation of previous clauses' guards into each clause's guard*)
    let clauses_with_guards = Def.Clause.explicit_negative_guards clauses in
    (* for each clause, check if candidate could have been its output *)
    let checked =
      List.map
        (fun c -> check_clause c candidate ctxt def.iargs iarg_vals term_vals)
        clauses_with_guards
    in
    Result.combine_results !^"Empty result list" checked


(* check if a candidate term could have been the output of a predicate clause *)
and check_clause
      (c : Def.Clause.t)
      (candidate : IT.t)
      (ctxt : C.t)
      (iargs : (Sym.t * BT.t) list)
      (iarg_vals : IT.t list)
      (term_vals : (IT.t * IT.t) list)
  =
  let open Result in
  let zipped = List.combine (List.map fst iargs) iarg_vals in
  (* get constraints on iarg values *)
  let toMap xs =
    List.fold_left (fun acc (k, v) -> Sym.Map.add k v acc) Sym.Map.empty xs
  in
  let ics = convert_symmap_to_lcs (toMap zipped) in
  (* get other constraints on terms *)
  let tcs = List.map pair_to_lc term_vals in
  (* get returned expression of c and variable dependency graph *)
  let exp, var_def_locs, lcs = organize_lines c.packing_ft in
  (* get constraints on whether candidate could have come from this clause *)
  let@ cs, vs = get_body_constraints exp var_def_locs candidate ctxt iargs term_vals in
  (* add guard and variable assignments to constraints list *)
  let cs' =
    List.concat
      [ LC.Set.elements ctxt.constraints;
        lcs;
        cs;
        ics;
        tcs;
        convert_symmap_to_lcs vs;
        [ LC.T c.guard ]
      ]
  in
  (* query solver *)
  ask_solver ctxt.global (Base.List.dedup_and_sort ~compare:LC.compare cs')


(* get a list of constraints that are satisfiable iff candidate could have come from this clause body *)
and get_body_constraints
      (exp : IT.t)
      (var_def_locs : def_line Sym.Map.t)
      (candidate : IT.t)
      (ctxt : C.t)
      (iargs : (Sym.t * BT.t) list)
      (term_vals : (IT.t * IT.t) list)
  =
  let open Result in
  let f var_cands =
    (* find constraints from checking each variable one at a time *)
    let accumulate_results acc (v, v_cand) =
      let@ acc_lcs, acc_var_cands = acc in
      let@ v_lcs, v_var_cands =
        get_var_constraints v v_cand acc_var_cands var_def_locs ctxt iargs term_vals
      in
      Result.return (List.append v_lcs acc_lcs, v_var_cands)
    in
    List.fold_left
      accumulate_results
      (return ([], var_cands))
      (Sym.Map.bindings var_cands)
  in
  (* use candidate to get terms for FVs in exp *)
  match get_var_cands exp candidate with
  | Ok var_cands -> f var_cands
  | Error (No, e) -> no e
  | Error (Err, e) -> error e
  | Error (Unknown, e) ->
    let here = Locations.other __LOC__ in
    let res =
      match ask_solver ctxt.global [ LC.T (IT.eq_ (exp, candidate) here) ] with
      | Ok _ ->
        (* not using model to get var cands because it may overconstrain *)
        return ([], Sym.Map.empty)
      | Error (No, _) -> no (Pp.( !^ ) "Solver returned no at variable assignment stage.")
      | Error (Unknown, _) -> unknown e
      | Error (Err, e') -> error e'
    in
    res


and get_var_constraints
      (v : Sym.t)
      (v_cand : IT.t)
      (var_cands : IT.t Sym.Map.t)
      (var_def_locs : def_line Sym.Map.t)
      (ctxt : C.t)
      (iargs : (Sym.t * BT.t) list)
      (term_vals : (IT.t * IT.t) list)
  =
  let open Pp in
  let open Result in
  (* find def of x *)
  match Sym.Map.find_opt v var_def_locs with
  | None ->
    (match (Sym.Map.find_opt v ctxt.logical, Sym.Map.find_opt v ctxt.computational) with
     | Some (Value it, _), _ ->
       get_body_constraints it var_def_locs v_cand ctxt iargs term_vals
     | _, Some (Value it, _) ->
       get_body_constraints it var_def_locs v_cand ctxt iargs term_vals
     | _ ->
       (* baseType case: Issue #901 *)
       let f (s, _) = Sym.equal s v in
       (match List.find_opt f iargs with
        | Some _ -> return ([], var_cands)
        | _ -> unknown (!^"Could not find variable definition line for" ^^^ Sym.pp v)))
  (* recurse with x's definition *)
  | Some (DefineL ((_, t), _)) ->
    get_body_constraints t var_def_locs v_cand ctxt iargs term_vals
  | Some (ResourceL ((_, (p, _)), _)) ->
    (match p with
     | P { name = Owned _; pointer = _; iargs = _ } ->
       (* if the predicate is Owned, get restrictions on pointer *)
       let owned_lcs = Res.derived_lc1 (p, O v_cand) in
       return (List.map (fun it -> LC.T it) owned_lcs, var_cands)
     | P { name = PName name; pointer = _; iargs } ->
       (* search for predicate definition *)
       (match Sym.Map.find_opt name ctxt.global.resource_predicates with
        | Some pdef ->
          let@ cs = check_pred name pdef v_cand ctxt iargs term_vals in
          return (cs, var_cands)
        | None -> unknown (!^"Could not find definition of predicate" ^^^ Sym.pp name))
     | Q _ -> unknown !^"Quantified predicates are out of scope for now.")
