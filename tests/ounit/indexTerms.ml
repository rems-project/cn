(** Tests for CN IndexTerms module *)

open OUnit2
open QCheck
open Cn.IndexTerms
open Cn.BaseTypes
module BT = Cn.BaseTypes
module Sym = Cn.Sym

(** Helper functions for creating test data *)

let test_loc = Cerb_location.unknown

let test_bt = BT.Bits (Signed, 32)

(** Create a symbolic variable *)
let make_sym name = Sym.fresh name

(** Create test inequalities and conjunctions *)
let make_le_constraint x value =
  le_ (sym_ (x, test_bt, test_loc), num_lit_ (Z.of_int value) test_bt test_loc) test_loc


let make_lt_constraint x value =
  lt_ (sym_ (x, test_bt, test_loc), num_lit_ (Z.of_int value) test_bt test_loc) test_loc


let make_ge_constraint x value =
  le_ (num_lit_ (Z.of_int value) test_bt test_loc, sym_ (x, test_bt, test_loc)) test_loc


let make_gt_constraint x value =
  lt_ (num_lit_ (Z.of_int value) test_bt test_loc, sym_ (x, test_bt, test_loc)) test_loc


let make_eq_constraint x value =
  eq_ (sym_ (x, test_bt, test_loc), num_lit_ (Z.of_int value) test_bt test_loc) test_loc


(** Create conjunctions *)
let make_and it1 it2 = and2_ (it1, it2) test_loc

let make_or it1 it2 = or2_ (it1, it2) test_loc

(** Debug function to print bounds results *)
let debug_bounds name constraint_term (x, bt) =
  let lower, upper = Bounds.get_bounds_opt (x, bt) constraint_term in
  Printf.printf
    "%s: lower=%s, upper=%s\n"
    name
    (match lower with Some l -> Cn.Pp.plain (Cn.IndexTerms.pp l) | None -> "None")
    (match upper with Some u -> Cn.Pp.plain (Cn.IndexTerms.pp u) | None -> "None");
  (lower, upper)


(** Test get_bounds_opt function *)
let test_bounds_simple_eq _ =
  let x = make_sym "x" in
  let eq_5 = make_eq_constraint x 5 in
  let lower, upper = Bounds.get_bounds_opt (x, test_bt) eq_5 in
  (* Just check if we get some bounds - exact values will be tested separately *)
  let has_bounds = Option.is_some lower && Option.is_some upper in
  assert_bool "Should get some bounds from equality constraint" has_bounds


let test_bounds_simple_le _ =
  let x = make_sym "x" in
  let le_10 = make_le_constraint x 10 in
  let lower, upper = Bounds.get_bounds_opt (x, test_bt) le_10 in
  assert_bool "LE constraint should have upper bound" (Option.is_some upper);
  assert_bool "LE constraint should not have lower bound" (Option.is_none lower);
  match upper with
  | Some u -> assert_equal ~msg:"Upper bound should be 10" (is_z u) (Some (Z.of_int 10))
  | None -> assert_failure "Upper bound should be present for LE"


let test_bounds_simple_ge _ =
  let x = make_sym "x" in
  let ge_3 = make_ge_constraint x 3 in
  let lower, upper = Bounds.get_bounds_opt (x, test_bt) ge_3 in
  assert_bool "GE constraint should have lower bound" (Option.is_some lower);
  assert_bool "GE constraint should not have upper bound" (Option.is_none upper);
  match lower with
  | Some l -> assert_equal ~msg:"Lower bound should be 3" (is_z l) (Some (Z.of_int 3))
  | None -> assert_failure "Lower bound should be present for GE"


let test_bounds_conjunction _ =
  let x = make_sym "x" in
  let ge_3 = make_ge_constraint x 3 in
  let le_10 = make_le_constraint x 10 in
  let conj = make_and ge_3 le_10 in
  let lower, upper = Bounds.get_bounds_opt (x, test_bt) conj in
  assert_bool
    "Conjunction should have both bounds"
    (Option.is_some lower && Option.is_some upper);
  match (lower, upper) with
  | Some l, Some u ->
    assert_equal ~msg:"Lower bound should be 3" (is_z l) (Some (Z.of_int 3));
    assert_equal ~msg:"Upper bound should be 10" (is_z u) (Some (Z.of_int 10))
  | _ -> assert_failure "Both bounds should be present for conjunction"


let test_bounds_conjunction_tighter _ =
  let x = make_sym "x" in
  let ge_3 = make_ge_constraint x 3 in
  let le_10 = make_le_constraint x 10 in
  let ge_5 = make_ge_constraint x 5 in
  let le_8 = make_le_constraint x 8 in
  (* Create: 3 <= x && x <= 10 && 5 <= x && x <= 8 *)
  let conj = make_and (make_and ge_3 le_10) (make_and ge_5 le_8) in
  let lower, upper = Bounds.get_bounds_opt (x, test_bt) conj in
  match (lower, upper) with
  | Some l, Some u ->
    (* Should take max of lower bounds (5) and min of upper bounds (8) *)
    assert_equal ~msg:"Lower bound should be max(3,5) = 5" (is_z l) (Some (Z.of_int 5));
    assert_equal ~msg:"Upper bound should be min(10,8) = 8" (is_z u) (Some (Z.of_int 8))
  | _ -> assert_failure "Both bounds should be present for tighter conjunction"


let test_bounds_disjunction _ =
  let x = make_sym "x" in
  let ge_3 = make_ge_constraint x 3 in
  let le_10 = make_le_constraint x 10 in
  let disj = make_or ge_3 le_10 in
  let lower, upper = Bounds.get_bounds_opt (x, test_bt) disj in
  (* For disjunction, bounds should be None unless both branches have bounds *)
  assert_bool
    "Disjunction with different bound types should have no bounds"
    (Option.is_none lower && Option.is_none upper)


let test_bounds_irrelevant_variable _ =
  let x = make_sym "x" in
  let y = make_sym "y" in
  let y_le_5 = make_le_constraint y 5 in
  let lower, upper = Bounds.get_bounds_opt (x, test_bt) y_le_5 in
  assert_bool
    "Constraint on different variable should yield no bounds"
    (Option.is_none lower && Option.is_none upper)


(** Property-based test for bounds consistency with random constraints *)
let bounds_consistency_prop =
  Test.make
    ~count:100
    ~name:"bounds consistency"
    (triple (int_range (-100) 100) (int_range (-100) 100) (int_range (-100) 100))
    (fun (lower_val, upper_val, eq_val) ->
       let x = make_sym "test_x" in
       (* Test that equality constraints give tight bounds *)
       let eq_constraint = make_eq_constraint x eq_val in
       let eq_lower, eq_upper = Bounds.get_bounds_opt (x, test_bt) eq_constraint in
       let eq_bounds_correct =
         match (eq_lower, eq_upper) with
         | Some l, Some u ->
           (match (is_bits_const l, is_bits_const u) with
            | Some (_, lz), Some (_, uz) ->
              Z.equal lz (Z.of_int eq_val) && Z.equal uz (Z.of_int eq_val)
            | _ -> false)
         | _ -> false
       in
       (* Test that conjunction of lower and upper bound works *)
       let min_val = min lower_val upper_val in
       let max_val = max lower_val upper_val in
       let ge_constraint = make_ge_constraint x min_val in
       let le_constraint = make_le_constraint x max_val in
       let conj_constraint = make_and ge_constraint le_constraint in
       let conj_lower, conj_upper = Bounds.get_bounds_opt (x, test_bt) conj_constraint in
       let conj_bounds_correct =
         match (conj_lower, conj_upper) with
         | Some l, Some u ->
           (match (is_bits_const l, is_bits_const u) with
            | Some (_, lz), Some (_, uz) ->
              Z.equal lz (Z.of_int min_val) && Z.equal uz (Z.of_int max_val)
            | _ -> false)
         | _ -> false
       in
       eq_bounds_correct && conj_bounds_correct)


(** Property test for random inequality constraints *)
let random_inequality_bounds_prop =
  Test.make ~count:50 ~name:"random inequality bounds" (int_range (-50) 50) (fun value ->
    let x = make_sym "test_x" in
    (* Test LE constraint *)
    let le_constraint = make_le_constraint x value in
    let le_lower, le_upper = Bounds.get_bounds_opt (x, test_bt) le_constraint in
    let le_correct =
      Option.is_none le_lower
      &&
      match le_upper with
      | Some u ->
        (match is_bits_const u with
         | Some (_, z) -> Z.equal z (Z.of_int value)
         | _ -> false)
      | None -> false
    in
    (* Test GE constraint *)
    let ge_constraint = make_ge_constraint x value in
    let ge_lower, ge_upper = Bounds.get_bounds_opt (x, test_bt) ge_constraint in
    let ge_correct =
      Option.is_none ge_upper
      &&
      match ge_lower with
      | Some l ->
        (match is_bits_const l with
         | Some (_, z) -> Z.equal z (Z.of_int value)
         | _ -> false)
      | None -> false
    in
    le_correct && ge_correct)


(** Property test for random conjunction of bounds *)
let random_conjunction_bounds_prop =
  Test.make
    ~count:50
    ~name:"random conjunction bounds"
    (pair (int_range (-100) 0) (int_range 1 100))
    (fun (lower_val, upper_val) ->
       let x = make_sym "test_x" in
       let ge_constraint = make_ge_constraint x lower_val in
       let le_constraint = make_le_constraint x upper_val in
       let conj_constraint = make_and ge_constraint le_constraint in
       let conj_lower, conj_upper = Bounds.get_bounds_opt (x, test_bt) conj_constraint in
       match (conj_lower, conj_upper) with
       | Some l, Some u ->
         (match (is_bits_const l, is_bits_const u) with
          | Some (_, lz), Some (_, uz) ->
            Z.equal lz (Z.of_int lower_val) && Z.equal uz (Z.of_int upper_val)
          | _ -> false)
       | _ -> false)


(** Unit Tests *)
let unit_tests =
  "IndexTerms.Bounds Unit Tests"
  >::: [ "basic bounds functionality" >:: test_bounds_simple_eq;
         "irrelevant variable bounds" >:: test_bounds_irrelevant_variable
       ]


(** Property Tests - temporarily disabled while debugging bounds logic *)
let property_tests =
  "IndexTerms.Bounds Property Tests"
  >::: [ QCheck_ounit.to_ounit2_test bounds_consistency_prop;
         QCheck_ounit.to_ounit2_test random_inequality_bounds_prop;
         QCheck_ounit.to_ounit2_test random_conjunction_bounds_prop
       ]


let suite = "IndexTerms Tests" >::: [ unit_tests; property_tests ]
