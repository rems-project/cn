(** Tests for call graph construction in GenContext *)

open OUnit2
module BT = Cn.BaseTypes
module IT = Cn.IndexTerms
module LC = Cn.LogicalConstraints
module Sym = Cn.Sym

module Domain = Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Ownership

module Term = Cn.TestGeneration.Private.Bennet.Private.Stage1.Private.Term.Make (Domain)

module Def = Cn.TestGeneration.Private.Bennet.Private.Stage1.Private.Def.Make (Domain)

module Ctx = Cn.TestGeneration.Private.Bennet.Private.Stage1.Private.Ctx.Make (Domain)

module G = Sym.DigraphLabeled

let test_loc = Cerb_location.unknown

(** Helper: build a Def.t with a given name and body *)
let mk_def name body : Def.t =
  { Def.filename = "test.c";
    recursive = false;
    spec = false;
    name;
    iargs = [];
    oarg = BT.Unit;
    c_types = None;
    body
  }


(** Helper: return unit (leaf node, no calls) *)
let return_unit = Term.return_ (IT.unit_ test_loc) () test_loc

(** Helper: get the label of an edge, or None if no edge exists *)
let edge_label cg src dst =
  match G.find_edge cg src dst with
  | e -> Some (G.E.label e)
  | exception Not_found -> None


(* ---- Unit tests ---- *)

(** Empty body (Return) produces no edges *)
let test_empty_body _ =
  let g = Sym.fresh "g" in
  let ctx : Ctx.t = [ (g, mk_def g return_unit) ] in
  let cg = Ctx.get_call_graph ctx in
  assert_equal ~msg:"1 vertex" 1 (G.nb_vertex cg);
  assert_equal ~msg:"0 edges" 0 (G.nb_edges cg)


(** A single Call produces one edge with label 1 *)
let test_single_call _ =
  let g = Sym.fresh "g" in
  let f = Sym.fresh "f" in
  let body = Term.call_ (f, []) () BT.Unit test_loc in
  let ctx : Ctx.t = [ (g, mk_def g body); (f, mk_def f return_unit) ] in
  let cg = Ctx.get_call_graph ctx in
  assert_equal ~msg:"g->f label" (Some 1) (edge_label cg g f)


(** Two calls to the same function via LetStar produces label 2 *)
let test_two_calls_same_via_let _ =
  let g = Sym.fresh "g" in
  let f = Sym.fresh "f" in
  let x = Sym.fresh "x" in
  let call_f = Term.call_ (f, []) () BT.Unit test_loc in
  let body = Term.let_star_ ((x, call_f), call_f) () test_loc in
  let ctx : Ctx.t = [ (g, mk_def g body); (f, mk_def f return_unit) ] in
  let cg = Ctx.get_call_graph ctx in
  assert_equal ~msg:"g->f label" (Some 2) (edge_label cg g f)


(** Two calls to different functions via LetStar produces separate edges *)
let test_two_calls_diff_via_let _ =
  let g = Sym.fresh "g" in
  let f = Sym.fresh "f" in
  let h = Sym.fresh "h" in
  let x = Sym.fresh "x" in
  let body =
    Term.let_star_
      ((x, Term.call_ (f, []) () BT.Unit test_loc), Term.call_ (h, []) () BT.Unit test_loc)
      ()
      test_loc
  in
  let ctx : Ctx.t =
    [ (g, mk_def g body); (f, mk_def f return_unit); (h, mk_def h return_unit) ]
  in
  let cg = Ctx.get_call_graph ctx in
  assert_equal ~msg:"g->f label" (Some 1) (edge_label cg g f);
  assert_equal ~msg:"g->h label" (Some 1) (edge_label cg g h)


(** Call through Assert is still counted *)
let test_call_through_assert _ =
  let g = Sym.fresh "g" in
  let f = Sym.fresh "f" in
  let lc = LC.T (IT.bool_ true test_loc) in
  let body = Term.assert_ (lc, Term.call_ (f, []) () BT.Unit test_loc) () test_loc in
  let ctx : Ctx.t = [ (g, mk_def g body); (f, mk_def f return_unit) ] in
  let cg = Ctx.get_call_graph ctx in
  assert_equal ~msg:"g->f label" (Some 1) (edge_label cg g f)


(** ITE with same callee in both branches produces label 2 *)
let test_ite_both_branches _ =
  let g = Sym.fresh "g" in
  let f = Sym.fresh "f" in
  let cond = IT.bool_ true test_loc in
  let call_f = Term.call_ (f, []) () BT.Unit test_loc in
  let body = Term.ite_ (cond, call_f, call_f) () test_loc in
  let ctx : Ctx.t = [ (g, mk_def g body); (f, mk_def f return_unit) ] in
  let cg = Ctx.get_call_graph ctx in
  assert_equal ~msg:"g->f label" (Some 2) (edge_label cg g f)


(** ITE with different callees in each branch *)
let test_ite_different_branches _ =
  let g = Sym.fresh "g" in
  let f = Sym.fresh "f" in
  let h = Sym.fresh "h" in
  let cond = IT.bool_ true test_loc in
  let body =
    Term.ite_
      ( cond,
        Term.call_ (f, []) () BT.Unit test_loc,
        Term.call_ (h, []) () BT.Unit test_loc )
      ()
      test_loc
  in
  let ctx : Ctx.t =
    [ (g, mk_def g body); (f, mk_def f return_unit); (h, mk_def h return_unit) ]
  in
  let cg = Ctx.get_call_graph ctx in
  assert_equal ~msg:"g->f label" (Some 1) (edge_label cg g f);
  assert_equal ~msg:"g->h label" (Some 1) (edge_label cg g h)


(** Multiple generators: g1 calls f twice, g2 calls f once *)
let test_multi_generator_graph _ =
  let g1 = Sym.fresh "g1" in
  let g2 = Sym.fresh "g2" in
  let f = Sym.fresh "f" in
  let x = Sym.fresh "x" in
  let call_f = Term.call_ (f, []) () BT.Unit test_loc in
  let body1 = Term.let_star_ ((x, call_f), call_f) () test_loc in
  let body2 = call_f in
  let ctx : Ctx.t =
    [ (g1, mk_def g1 body1); (g2, mk_def g2 body2); (f, mk_def f return_unit) ]
  in
  let cg = Ctx.get_call_graph ctx in
  assert_equal ~msg:"g1->f label" (Some 2) (edge_label cg g1 f);
  assert_equal ~msg:"g2->f label" (Some 1) (edge_label cg g2 f);
  assert_equal ~msg:"no g1->g2" None (edge_label cg g1 g2)


(* ---- QCheck property-based tests ---- *)

(** Generate a random Term.t tree bounded by depth *)
let rec gen_term (syms : Sym.t list) (depth : int) : Term.t QCheck.Gen.t =
  let open QCheck.Gen in
  let leaf = oneof [ return return_unit; return (Term.arbitrary_ () BT.Unit test_loc) ] in
  let call_gen =
    match syms with
    | [] -> return return_unit
    | _ ->
      let* idx = int_bound (List.length syms - 1) in
      let callee = List.nth syms idx in
      return (Term.call_ (callee, []) () BT.Unit test_loc)
  in
  if depth <= 0 then
    oneof [ leaf; call_gen ]
  else (
    let smaller = gen_term syms (depth - 1) in
    oneof
      [ leaf;
        call_gen;
        (* LetStar *)
        (let* gt1 = smaller in
         let* gt2 = smaller in
         let x = Sym.fresh "x" in
         return (Term.let_star_ ((x, gt1), gt2) () test_loc));
        (* ITE *)
        (let* gt1 = smaller in
         let* gt2 = smaller in
         let cond = IT.bool_ true test_loc in
         return (Term.ite_ (cond, gt1, gt2) () test_loc));
        (* Assert *)
        (let* gt' = smaller in
         let lc = LC.T (IT.bool_ true test_loc) in
         return (Term.assert_ (lc, gt') () test_loc))
      ])


(** Generate a random context of n generators *)
let gen_ctx : Ctx.t QCheck.Gen.t =
  let open QCheck.Gen in
  let* n = int_range 1 10 in
  let syms = List.init n (fun i -> Sym.fresh (Printf.sprintf "gen_%d" i)) in
  let gen_entry sym =
    let* body = gen_term syms 3 in
    return (sym, mk_def sym body)
  in
  let* defs =
    List.fold_right
      (fun sym acc ->
         let* entry = gen_entry sym in
         let* rest = acc in
         return (entry :: rest))
      syms
      (return [])
  in
  return defs


(** Property: vertex count equals number of generators in ctx *)
let prop_vertex_count =
  QCheck.Test.make
    ~name:"vertex count equals context size"
    ~count:100
    (QCheck.make gen_ctx)
    (fun ctx ->
       let cg = Ctx.get_call_graph ctx in
       G.nb_vertex cg = List.length ctx)


(** Property: all edge labels are positive *)
let prop_edge_labels_positive =
  QCheck.Test.make
    ~name:"all edge labels are positive"
    ~count:100
    (QCheck.make gen_ctx)
    (fun ctx ->
       let cg = Ctx.get_call_graph ctx in
       G.fold_edges_e (fun e acc -> acc && G.E.label e > 0) cg true)


(** Test suite *)
let suite =
  "CallGraph Tests"
  >::: [ "empty body" >:: test_empty_body;
         "single call" >:: test_single_call;
         "two calls same via let" >:: test_two_calls_same_via_let;
         "two calls diff via let" >:: test_two_calls_diff_via_let;
         "call through assert" >:: test_call_through_assert;
         "ite both branches" >:: test_ite_both_branches;
         "ite different branches" >:: test_ite_different_branches;
         "multi generator graph" >:: test_multi_generator_graph;
         QCheck_ounit.to_ounit2_test prop_vertex_count;
         QCheck_ounit.to_ounit2_test prop_edge_labels_positive
       ]
