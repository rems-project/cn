(** Tests for PruneArgs Stage1 transformation *)

open OUnit2
module BT = Cn.BaseTypes
module IT = Cn.IndexTerms
module LC = Cn.LogicalConstraints
module Sym = Cn.Sym

module Domain = Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Ownership

module PruneArgs =
  Cn.TestGeneration.Private.Bennet.Private.Stage1.Private.PruneArgs.Make (Domain)

module Term = Cn.TestGeneration.Private.Bennet.Private.Stage1.Private.Term.Make (Domain)

module Def = Cn.TestGeneration.Private.Bennet.Private.Stage1.Private.Def.Make (Domain)

module Ctx = Cn.TestGeneration.Private.Bennet.Private.Stage1.Private.Ctx.Make (Domain)

let test_loc = Cerb_location.unknown

(** Helper: Create a simple generator with given arguments and body constraints *)
let make_simple_gen name iargs body_assertions : Def.t =
  let rec make_body = function
    | [] -> Term.return_ (IT.unit_ test_loc) () test_loc
    | (lhs, rhs) :: rest ->
      let constraint_it = IT.eq_ (lhs, rhs) test_loc in
      let constraint_lc = LC.T constraint_it in
      Term.assert_ (constraint_lc, make_body rest) () test_loc
  in
  { Def.filename = "test.c";
    recursive = false;
    spec = false;
    name;
    iargs;
    oarg = BT.Unit;
    c_types = None;
    body = make_body body_assertions
  }


(** Helper: Create generator that calls another generator *)
let make_gen_with_call name iargs callee_sym callee_args : Def.t =
  let call_body = Term.call_ (callee_sym, callee_args) () BT.Unit test_loc in
  { Def.filename = "test.c";
    recursive = false;
    spec = false;
    name;
    iargs;
    oarg = BT.Unit;
    c_types = None;
    body = call_body
  }


(** Test: Single generator with one unused argument gets it removed *)
let test_unused_arg_removed _ =
  let gen_name = Sym.fresh "test_gen" in
  let used_arg_sym = Sym.fresh "x" in
  let unused_arg_sym = Sym.fresh "y" in
  let used_arg = (used_arg_sym, BT.Bits (Signed, 32)) in
  let unused_arg = (unused_arg_sym, BT.Bits (Signed, 32)) in
  (* Body uses only x, not y *)
  let x_it = IT.sym_ (used_arg_sym, BT.Bits (Signed, 32), test_loc) in
  let five = IT.num_lit_ (Z.of_int 5) (BT.Bits (Signed, 32)) test_loc in
  let gen_def = make_simple_gen gen_name [ used_arg; unused_arg ] [ (x_it, five) ] in
  let ctx : Ctx.t = [ (gen_name, gen_def) ] in
  (* Transform *)
  let transformed_ctx = PruneArgs.transform ctx in
  let transformed_def = List.assoc gen_name transformed_ctx in
  (* Assert: only 1 argument remains (the used one) *)
  assert_equal
    ~msg:"Should have 1 argument after removing unused"
    1
    (List.length transformed_def.iargs);
  assert_equal
    ~msg:"Remaining arg should be 'x'"
    "x"
    (Sym.pp_string (fst (List.nth transformed_def.iargs 0)))


(** Test: Call sites get updated when callee's arguments are pruned *)
let test_call_site_updated _ =
  (* Create gen1 with unused argument y (only uses x) *)
  let gen1_name = Sym.fresh "gen1" in
  let x_sym = Sym.fresh "x" in
  let y_sym = Sym.fresh "y" in
  let x_arg = (x_sym, BT.Bits (Signed, 32)) in
  let y_arg = (y_sym, BT.Bits (Signed, 32)) in
  let x_it = IT.sym_ (x_sym, BT.Bits (Signed, 32), test_loc) in
  let ten = IT.num_lit_ (Z.of_int 10) (BT.Bits (Signed, 32)) test_loc in
  let gen1_def = make_simple_gen gen1_name [ x_arg; y_arg ] [ (x_it, ten) ] in
  (* Create gen2 that calls gen1 with two arguments *)
  let gen2_name = Sym.fresh "gen2" in
  let a_sym = Sym.fresh "a" in
  let b_sym = Sym.fresh "b" in
  let a_arg = (a_sym, BT.Bits (Signed, 32)) in
  let b_arg = (b_sym, BT.Bits (Signed, 32)) in
  let a_it = IT.sym_ (a_sym, BT.Bits (Signed, 32), test_loc) in
  let b_it = IT.sym_ (b_sym, BT.Bits (Signed, 32), test_loc) in
  let gen2_def = make_gen_with_call gen2_name [ a_arg; b_arg ] gen1_name [ a_it; b_it ] in
  let ctx : Ctx.t = [ (gen1_name, gen1_def); (gen2_name, gen2_def) ] in
  (* Transform *)
  let transformed_ctx = PruneArgs.transform ctx in
  let transformed_gen1 = List.assoc gen1_name transformed_ctx in
  let transformed_gen2 = List.assoc gen2_name transformed_ctx in
  (* Assert: gen1 has only 1 argument now *)
  assert_equal
    ~msg:"gen1 should have 1 argument after removing unused y"
    1
    (List.length transformed_gen1.iargs);
  (* Assert: the call in gen2 also has only 1 argument now *)
  match Term.is_call transformed_gen2.body with
  | Some (fsym, iargs) ->
    assert_equal ~msg:"Call to gen1 should have 1 argument" 1 (List.length iargs);
    assert_equal ~msg:"Called function should still be gen1" gen1_name fsym
  | None -> assert_failure "gen2 body should be a Call"


(** Test suite *)
let suite =
  "PruneArgs Tests"
  >::: [ "unused arg removed" >:: test_unused_arg_removed;
         "call site updated" >:: test_call_site_updated
       ]
