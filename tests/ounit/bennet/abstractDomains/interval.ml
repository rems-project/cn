(** Tests for NonRelational.Make(IntervalBasis) domain vs IndexTerms.Bounds *)

open OUnit2
open QCheck
open Cn.IndexTerms
module BT = Cn.BaseTypes
module Sym = Cn.Sym
module IT = Cn.IndexTerms

module NonRelational =
  Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Private.NonRelational

module IntervalBasis =
  Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Private.Interval.IntervalBasis

module IntervalDomain = NonRelational.Make (IntervalBasis)

(** Helper functions for creating test data *)
let test_loc = Cerb_location.unknown

let test_bt = BT.Bits (Signed, 32)

(** Create a symbolic variable *)
let make_sym name = Sym.fresh name

(** Create test inequalities and conjunctions *)
let make_le_constraint x value =
  le_ (sym_ (x, test_bt, test_loc), num_lit_ (Z.of_int value) test_bt test_loc) test_loc


let make_ge_constraint x value =
  le_ (num_lit_ (Z.of_int value) test_bt test_loc, sym_ (x, test_bt, test_loc)) test_loc


let make_eq_constraint x value =
  eq_ (sym_ (x, test_bt, test_loc), num_lit_ (Z.of_int value) test_bt test_loc) test_loc


let make_and it1 it2 = and2_ (it1, it2) test_loc

(** Helper to extract bounds from interval domain result *)
let extract_interval_bounds x domain_result =
  match domain_result with
  | None -> (None, None)
  | Some domain_map ->
    (match Sym.Map.find_opt x domain_map with
     | None -> (None, None)
     | Some interval ->
       if IntervalBasis.is_bottom interval then
         (None, None)
       else (
         let interval_bt = IntervalBasis.bt interval in
         let start_it = num_lit_ interval.start interval_bt test_loc in
         let stop_it = num_lit_ interval.stop interval_bt test_loc in
         (Some start_it, Some stop_it)))


(** Compare numeric values from IndexTerms *)
let compare_bounds_values bounds_opt interval_opt =
  match (bounds_opt, interval_opt) with
  | None, None -> true
  | Some bounds_term, Some interval_term ->
    (match (is_bits_const bounds_term, is_bits_const interval_term) with
     | Some (_, bz), Some (_, iz) -> Z.equal bz iz
     | _ -> false)
  | _ -> false


let pp_bound b = Cn.Pp.plain (Cn.Option.pp IT.pp b)

(** Test comparison between Bounds and NonRelational domain on simple equality *)
let test_bounds_vs_interval_eq _ =
  let x = make_sym "x" in
  let eq_5 = make_eq_constraint x 5 in
  (* Test Bounds.get_bounds_opt *)
  let bounds_lower, bounds_upper = Bounds.get_bounds_opt (x, test_bt) eq_5 in
  (* Test NonRelational local_iteration *)
  let interval_result = IntervalDomain.local_iteration eq_5 IntervalDomain.top in
  let interval_lower, interval_upper = extract_interval_bounds x interval_result in
  (* Both should find the same bounds for equality constraints *)
  assert_equal
    ~cmp:compare_bounds_values
    ~printer:pp_bound
    ~msg:"Both lower bounds should match for equality"
    bounds_lower
    interval_lower;
  assert_equal
    ~cmp:compare_bounds_values
    ~printer:pp_bound
    ~msg:"Both upper bounds should match for equality"
    bounds_upper
    interval_upper


(** Test comparison on LE constraint *)
let test_bounds_vs_interval_le _ =
  let x = make_sym "x" in
  let le_10 = make_le_constraint x 10 in
  let bounds_lower, bounds_upper = Bounds.get_bounds_opt (x, test_bt) le_10 in
  let interval_result = IntervalDomain.local_iteration le_10 IntervalDomain.top in
  let _, interval_upper = extract_interval_bounds x interval_result in
  (* LE constraint should give no lower bound, but upper bound of 10 *)
  assert_equal
    ~msg:"LE: Bounds should have no lower bound"
    ~printer:pp_bound
    None
    bounds_lower;
  assert_bool "LE: Bounds should have upper bound" (Option.is_some bounds_upper);
  (* The interval domain should behave similarly for the upper bound *)
  assert_equal
    ~cmp:compare_bounds_values
    ~printer:pp_bound
    ~msg:"LE: Upper bounds should match"
    bounds_upper
    interval_upper


(** Test comparison on GE constraint *)
let test_bounds_vs_interval_ge _ =
  let x = make_sym "x" in
  let ge_3 = make_ge_constraint x 3 in
  let bounds_lower, bounds_upper = Bounds.get_bounds_opt (x, test_bt) ge_3 in
  let interval_result = IntervalDomain.local_iteration ge_3 IntervalDomain.top in
  let interval_lower, _ = extract_interval_bounds x interval_result in
  (* GE constraint should give lower bound of 3, but no upper bound *)
  assert_bool "GE: Bounds should have lower bound" (Option.is_some bounds_lower);
  assert_bool "GE: Bounds should have no upper bound" (Option.is_none bounds_upper);
  (* The interval domain should behave similarly for the lower bound *)
  assert_equal
    ~cmp:compare_bounds_values
    ~printer:pp_bound
    ~msg:"GE: Lower bounds should match"
    bounds_lower
    interval_lower


(** Test comparison on conjunction *)
let test_bounds_vs_interval_conjunction _ =
  let x = make_sym "x" in
  let ge_3 = make_ge_constraint x 3 in
  let le_10 = make_le_constraint x 10 in
  let conj = make_and ge_3 le_10 in
  let bounds_lower, bounds_upper = Bounds.get_bounds_opt (x, test_bt) conj in
  let interval_result = IntervalDomain.local_iteration conj IntervalDomain.top in
  let interval_lower, interval_upper = extract_interval_bounds x interval_result in
  (* Both should find bounds [3, 10] *)
  assert_bool
    "Conjunction: Both should have lower bound"
    (Option.is_some bounds_lower && Option.is_some interval_lower);
  assert_bool
    "Conjunction: Both should have upper bound"
    (Option.is_some bounds_upper && Option.is_some interval_upper);
  assert_equal
    ~cmp:compare_bounds_values
    ~printer:pp_bound
    ~msg:"Conjunction: Lower bounds should match"
    bounds_lower
    interval_lower;
  assert_equal
    ~cmp:compare_bounds_values
    ~printer:pp_bound
    ~msg:"Conjunction: Upper bounds should match"
    bounds_upper
    interval_upper


(** Property test comparing bounds extraction on random constraints *)
let bounds_vs_interval_consistency_prop =
  Test.make
    ~count:50
    ~name:"bounds vs interval consistency"
    (pair (int_range (-50) 50) (int_range (-50) 50))
    (fun (lower_val, upper_val) ->
       let x = make_sym "test_x" in
       let min_val = min lower_val upper_val in
       let max_val = max lower_val upper_val in
       (* Test conjunction of bounds *)
       let ge_constraint = make_ge_constraint x min_val in
       let le_constraint = make_le_constraint x max_val in
       let conj_constraint = make_and ge_constraint le_constraint in
       let bounds_lower, bounds_upper =
         Bounds.get_bounds_opt (x, test_bt) conj_constraint
       in
       let interval_result =
         IntervalDomain.local_iteration conj_constraint IntervalDomain.top
       in
       let interval_lower, interval_upper = extract_interval_bounds x interval_result in
       (* Both methods should agree on the extracted bounds *)
       compare_bounds_values bounds_lower interval_lower
       && compare_bounds_values bounds_upper interval_upper)


(** Unit Tests *)
let unit_tests =
  "NonRelational.IntervalBasis vs Bounds Tests"
  >::: [ "bounds vs interval eq" >:: test_bounds_vs_interval_eq;
         "bounds vs interval le" >:: test_bounds_vs_interval_le;
         "bounds vs interval ge" >:: test_bounds_vs_interval_ge;
         "bounds vs interval conjunction" >:: test_bounds_vs_interval_conjunction
       ]


(** Property Tests *)
let property_tests =
  "NonRelational.IntervalBasis vs Bounds Property Tests"
  >::: [ QCheck_ounit.to_ounit2_test bounds_vs_interval_consistency_prop ]


let suite = "NonRelational IntervalBasis Tests" >::: [ unit_tests; property_tests ]
