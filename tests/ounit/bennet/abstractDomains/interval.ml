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


(** Test interval arithmetic operations directly *)
let test_interval_add _ =
  let bt = BT.Bits (Signed, 32) in
  let i1 = IntervalBasis.of_interval bt (Z.of_int 2) (Z.of_int 5) in
  let i2 = IntervalBasis.of_interval bt (Z.of_int 3) (Z.of_int 7) in
  let result = IntervalBasis.forward_abs_binop IT.Add i1 i2 in
  match result with
  | Some interval ->
    assert_equal ~msg:"Add: start should be 5" (Z.of_int 5) interval.start;
    assert_equal ~msg:"Add: stop should be 12" (Z.of_int 12) interval.stop
  | None -> assert_failure "Add operation should not return None"


let test_interval_sub _ =
  let bt = BT.Bits (Signed, 32) in
  let i1 = IntervalBasis.of_interval bt (Z.of_int 5) (Z.of_int 10) in
  let i2 = IntervalBasis.of_interval bt (Z.of_int 2) (Z.of_int 3) in
  let result = IntervalBasis.forward_abs_binop IT.Sub i1 i2 in
  match result with
  | Some interval ->
    assert_equal ~msg:"Sub: start should be 2" (Z.of_int 2) interval.start;
    assert_equal ~msg:"Sub: stop should be 8" (Z.of_int 8) interval.stop
  | None -> assert_failure "Sub operation should not return None"


let test_interval_mul _ =
  let bt = BT.Bits (Signed, 32) in
  let i1 = IntervalBasis.of_interval bt (Z.of_int 2) (Z.of_int 3) in
  let i2 = IntervalBasis.of_interval bt (Z.of_int 4) (Z.of_int 5) in
  let result = IntervalBasis.forward_abs_binop IT.Mul i1 i2 in
  match result with
  | Some interval ->
    assert_equal ~msg:"Mul: start should be 8" (Z.of_int 8) interval.start;
    assert_equal ~msg:"Mul: stop should be 15" (Z.of_int 15) interval.stop
  | None -> assert_failure "Mul operation should not return None"


let test_interval_div _ =
  let bt = BT.Bits (Signed, 32) in
  let i1 = IntervalBasis.of_interval bt (Z.of_int 8) (Z.of_int 12) in
  let i2 = IntervalBasis.of_interval bt (Z.of_int 2) (Z.of_int 3) in
  let result = IntervalBasis.forward_abs_binop IT.Div i1 i2 in
  match result with
  | Some interval ->
    assert_equal ~msg:"Div: start should be 2" (Z.of_int 2) interval.start;
    assert_equal ~msg:"Div: stop should be 6" (Z.of_int 6) interval.stop
  | None -> assert_failure "Div operation should not return None"


let test_interval_div_by_zero _ =
  let bt = BT.Bits (Signed, 32) in
  let i1 = IntervalBasis.of_interval bt (Z.of_int 5) (Z.of_int 10) in
  let i2 = IntervalBasis.of_interval bt Z.zero Z.zero in
  let result = IntervalBasis.forward_abs_binop IT.Div i1 i2 in
  match result with
  | Some interval ->
    assert_bool "Div by zero should result in bottom" (IntervalBasis.is_bottom interval)
  | None -> assert_failure "Div operation should return Some bottom"


let test_interval_mod _ =
  let bt = BT.Bits (Signed, 32) in
  let i1 = IntervalBasis.of_interval bt (Z.of_int 5) (Z.of_int 15) in
  let i2 = IntervalBasis.of_interval bt (Z.of_int 3) (Z.of_int 7) in
  let result = IntervalBasis.forward_abs_binop IT.Mod i1 i2 in
  match result with
  | Some interval ->
    (* Modulo by [3,7] should give bounds [-6, 6] *)
    assert_equal ~msg:"Mod: start should be -6" (Z.of_int (-6)) interval.start;
    assert_equal ~msg:"Mod: stop should be 6" (Z.of_int 6) interval.stop
  | None -> assert_failure "Mod operation should not return None"


let test_interval_bw_and _ =
  let bt = BT.Bits (Signed, 32) in
  let i1 = IntervalBasis.of_interval bt (Z.of_int 4) (Z.of_int 7) in
  let i2 = IntervalBasis.of_interval bt (Z.of_int 2) (Z.of_int 6) in
  let result = IntervalBasis.forward_abs_binop IT.BW_And i1 i2 in
  match result with
  | Some interval ->
    (* Bitwise AND should be conservative *)
    assert_bool
      "BW_And result should not be bottom"
      (not (IntervalBasis.is_bottom interval));
    assert_bool "BW_And start should be <= stop" (Z.leq interval.start interval.stop)
  | None -> assert_failure "BW_And operation should not return None"


let test_interval_bw_or _ =
  let bt = BT.Bits (Signed, 32) in
  let i1 = IntervalBasis.of_interval bt (Z.of_int 2) (Z.of_int 4) in
  let i2 = IntervalBasis.of_interval bt (Z.of_int 1) (Z.of_int 3) in
  let result = IntervalBasis.forward_abs_binop IT.BW_Or i1 i2 in
  match result with
  | Some interval ->
    assert_bool
      "BW_Or result should not be bottom"
      (not (IntervalBasis.is_bottom interval));
    assert_bool "BW_Or start should be <= stop" (Z.leq interval.start interval.stop)
  | None -> assert_failure "BW_Or operation should not return None"


let test_interval_shift_left _ =
  let bt = BT.Bits (Signed, 32) in
  let i1 = IntervalBasis.of_interval bt (Z.of_int 2) (Z.of_int 4) in
  let i2 = IntervalBasis.of_interval bt (Z.of_int 1) (Z.of_int 2) in
  let result = IntervalBasis.forward_abs_binop IT.ShiftLeft i1 i2 in
  match result with
  | Some interval ->
    (* [2,4] << [1,2] should give [4,16] *)
    assert_equal ~msg:"ShiftLeft: start should be 4" (Z.of_int 4) interval.start;
    assert_equal ~msg:"ShiftLeft: stop should be 16" (Z.of_int 16) interval.stop
  | None -> assert_failure "ShiftLeft operation should not return None"


let test_interval_shift_right _ =
  let bt = BT.Bits (Signed, 32) in
  let i1 = IntervalBasis.of_interval bt (Z.of_int 8) (Z.of_int 16) in
  let i2 = IntervalBasis.of_interval bt (Z.of_int 1) (Z.of_int 2) in
  let result = IntervalBasis.forward_abs_binop IT.ShiftRight i1 i2 in
  match result with
  | Some interval ->
    (* [8,16] >> [1,2] should give [2,8] *)
    assert_equal ~msg:"ShiftRight: start should be 2" (Z.of_int 2) interval.start;
    assert_equal ~msg:"ShiftRight: stop should be 8" (Z.of_int 8) interval.stop
  | None -> assert_failure "ShiftRight operation should not return None"


(** Test interval casting without overflow **)
let test_interval_cast_no_overflow _ =
  let bt32 = BT.Bits (Signed, 32) in
  let bt16 = BT.Bits (Signed, 16) in
  let interval = IntervalBasis.of_interval bt32 (Z.of_int 100) (Z.of_int 200) in
  let result = IntervalBasis.cast interval bt16 in
  assert_bool "Cast result should not be bottom" (not (IntervalBasis.is_bottom result));
  assert_equal ~msg:"Cast: start should be 100" (Z.of_int 100) result.start;
  assert_equal ~msg:"Cast: stop should be 200" (Z.of_int 200) result.stop;
  assert_equal ~msg:"Cast: bt should be target type" bt16 (IntervalBasis.bt result)


(** Test interval casting with overflow **)
let test_interval_cast_overflow _ =
  let bt32 = BT.Bits (Signed, 32) in
  let bt8 = BT.Bits (Signed, 8) in
  let interval = IntervalBasis.of_interval bt32 (Z.of_int (-200)) (Z.of_int 200) in
  let result = IntervalBasis.cast interval bt8 in
  assert_bool "Cast with overflow should be top" (IntervalBasis.is_top result);
  assert_equal
    ~msg:"Cast overflow: bt should be target type"
    bt8
    (IntervalBasis.bt result)


(** Test interval casting from bottom **)
let test_interval_cast_bottom _ =
  let bt32 = BT.Bits (Signed, 32) in
  let bt16 = BT.Bits (Signed, 16) in
  let interval = IntervalBasis.bottom bt32 in
  let result = IntervalBasis.cast interval bt16 in
  assert_bool "Cast from bottom should be bottom" (IntervalBasis.is_bottom result);
  assert_equal ~msg:"Cast bottom: bt should be target type" bt16 (IntervalBasis.bt result)


(** Test interval casting to same type **)
let test_interval_cast_same_type _ =
  let bt = BT.Bits (Signed, 32) in
  let interval = IntervalBasis.of_interval bt (Z.of_int 10) (Z.of_int 20) in
  let result = IntervalBasis.cast interval bt in
  assert_bool
    "Cast to same type should not be bottom"
    (not (IntervalBasis.is_bottom result));
  assert_equal ~msg:"Cast same: start should be preserved" (Z.of_int 10) result.start;
  assert_equal ~msg:"Cast same: stop should be preserved" (Z.of_int 20) result.stop;
  assert_equal ~msg:"Cast same: bt should be preserved" bt (IntervalBasis.bt result)


(** Test interval casting unsigned to signed **)
let test_interval_cast_unsigned_to_signed _ =
  let bt_u32 = BT.Bits (Unsigned, 32) in
  let bt_s32 = BT.Bits (Signed, 32) in
  let interval = IntervalBasis.of_interval bt_u32 (Z.of_int 100) (Z.of_int 200) in
  let result = IntervalBasis.cast interval bt_s32 in
  assert_bool "Cast u32->s32 should not be bottom" (not (IntervalBasis.is_bottom result));
  assert_equal ~msg:"Cast u32->s32: start should be 100" (Z.of_int 100) result.start;
  assert_equal ~msg:"Cast u32->s32: stop should be 200" (Z.of_int 200) result.stop;
  assert_equal
    ~msg:"Cast u32->s32: bt should be target type"
    bt_s32
    (IntervalBasis.bt result)


(** Test interval casting with narrow to wide **)
let test_interval_cast_narrow_to_wide _ =
  let bt8 = BT.Bits (Signed, 8) in
  let bt32 = BT.Bits (Signed, 32) in
  let interval = IntervalBasis.of_interval bt8 (Z.of_int (-50)) (Z.of_int 50) in
  let result = IntervalBasis.cast interval bt32 in
  assert_bool
    "Cast narrow->wide should not be bottom"
    (not (IntervalBasis.is_bottom result));
  assert_equal ~msg:"Cast narrow->wide: start should be -50" (Z.of_int (-50)) result.start;
  assert_equal ~msg:"Cast narrow->wide: stop should be 50" (Z.of_int 50) result.stop;
  assert_equal
    ~msg:"Cast narrow->wide: bt should be target type"
    bt32
    (IntervalBasis.bt result)


(** Test interval casting to location type **)
let test_interval_cast_to_loc _ =
  let bt32 = BT.Bits (Signed, 32) in
  let bt_loc = BT.Loc () in
  let interval = IntervalBasis.of_interval bt32 (Z.of_int 100) (Z.of_int 200) in
  let result = IntervalBasis.cast interval bt_loc in
  assert_bool "Cast to Loc should not be bottom" (not (IntervalBasis.is_bottom result));
  assert_equal ~msg:"Cast to Loc: bt should be Loc" bt_loc (IntervalBasis.bt result)


(** Property test for arithmetic operations soundness *)
let arithmetic_soundness_prop =
  Test.make
    ~count:100
    ~name:"arithmetic operations soundness"
    (quad
       (int_range (-20) 20)
       (int_range (-20) 20)
       (int_range (-20) 20)
       (int_range (-20) 20))
    (fun (a1, a2, b1, b2) ->
       let bt = BT.Bits (Signed, 32) in
       let min_a, max_a = (min a1 a2, max a1 a2) in
       let min_b, max_b = (min b1 b2, max b1 b2) in
       let i1 = IntervalBasis.of_interval bt (Z.of_int min_a) (Z.of_int max_a) in
       let i2 = IntervalBasis.of_interval bt (Z.of_int min_b) (Z.of_int max_b) in
       (* Test addition soundness *)
       let add_result = IntervalBasis.forward_abs_binop IT.Add i1 i2 in
       let add_sound =
         match add_result with
         | Some interval ->
           let expected_min = min_a + min_b in
           let expected_max = max_a + max_b in
           Z.leq (Z.of_int expected_min) interval.start
           && Z.leq interval.stop (Z.of_int expected_max)
         | None -> false
       in
       (* Test subtraction soundness *)
       let sub_result = IntervalBasis.forward_abs_binop IT.Sub i1 i2 in
       let sub_sound =
         match sub_result with
         | Some interval ->
           let expected_min = min_a - max_b in
           let expected_max = max_a - min_b in
           Z.leq (Z.of_int expected_min) interval.start
           && Z.leq interval.stop (Z.of_int expected_max)
         | None -> false
       in
       add_sound && sub_sound)


(** Property test for division by non-zero *)
let division_soundness_prop =
  Test.make
    ~count:50
    ~name:"division soundness (non-zero divisor)"
    (triple (int_range (-20) 20) (int_range (-20) 20) (int_range 1 10))
    (fun (a1, a2, b) ->
       let bt = BT.Bits (Signed, 32) in
       let min_a, max_a = (min a1 a2, max a1 a2) in
       let i1 = IntervalBasis.of_interval bt (Z.of_int min_a) (Z.of_int max_a) in
       let i2 = IntervalBasis.of_interval bt (Z.of_int b) (Z.of_int b) in
       let div_result = IntervalBasis.forward_abs_binop IT.Div i1 i2 in
       match div_result with
       | Some interval ->
         (not (IntervalBasis.is_bottom interval)) && Z.leq interval.start interval.stop
       | None -> false)


(** Property test for cast soundness *)
let cast_soundness_prop =
  Test.make
    ~count:100
    ~name:"cast operation soundness"
    (pair (int_range (-1000) 1000) (int_range (-1000) 1000))
    (fun (start_val, stop_val) ->
       let source_bt = BT.Bits (Signed, 32) in
       let target_bt = BT.Bits (Signed, 16) in
       let min_val, max_val = (min start_val stop_val, max start_val stop_val) in
       let interval =
         IntervalBasis.of_interval source_bt (Z.of_int min_val) (Z.of_int max_val)
       in
       let result = IntervalBasis.cast interval target_bt in
       (* Basic properties that should always hold *)
       let target_min, target_max = IntervalBasis.get_extrema target_bt in
       let basic_props =
         BT.equal (IntervalBasis.bt result) target_bt
         && (IntervalBasis.is_bottom result || Z.leq result.start result.stop)
         && (IntervalBasis.is_bottom result
             || (Z.leq target_min result.start && Z.leq result.stop target_max))
       in
       (* If no overflow occurs, the values should be preserved *)
       let overflow_check =
         let in_range =
           Z.geq (Z.of_int min_val) target_min && Z.leq (Z.of_int max_val) target_max
         in
         if in_range then (* No overflow expected *)
           (not (IntervalBasis.is_bottom result))
           && Z.equal result.start (Z.of_int min_val)
           && Z.equal result.stop (Z.of_int max_val)
         else (* Overflow expected - should be top *)
           IntervalBasis.is_top result
       in
       basic_props && overflow_check)


(** Property test for cast preserves bottom *)
let cast_bottom_prop =
  Test.make ~count:50 ~name:"cast preserves bottom" unit (fun () ->
    let source_bt = BT.Bits (Signed, 32) in
    let target_bt = BT.Bits (Signed, 16) in
    let bottom_interval = IntervalBasis.bottom source_bt in
    let result = IntervalBasis.cast bottom_interval target_bt in
    IntervalBasis.is_bottom result && BT.equal (IntervalBasis.bt result) target_bt)


(** Property test for cast between same types is identity *)
let cast_identity_prop =
  Test.make
    ~count:50
    ~name:"cast to same type is identity"
    (pair (int_range (-100) 100) (int_range (-100) 100))
    (fun (start_val, stop_val) ->
       let bt = BT.Bits (Signed, 32) in
       let min_val, max_val = (min start_val stop_val, max start_val stop_val) in
       let interval =
         IntervalBasis.of_interval bt (Z.of_int min_val) (Z.of_int max_val)
       in
       let result = IntervalBasis.cast interval bt in
       Z.equal result.start interval.start
       && Z.equal result.stop interval.stop
       && BT.equal (IntervalBasis.bt result) bt
       && Bool.equal (IntervalBasis.is_bottom result) (IntervalBasis.is_bottom interval))


(** New unit tests for arithmetic operations *)
let arithmetic_tests =
  "Interval Arithmetic Operations"
  >::: [ "interval add" >:: test_interval_add;
         "interval sub" >:: test_interval_sub;
         "interval mul" >:: test_interval_mul;
         "interval div" >:: test_interval_div;
         "interval div by zero" >:: test_interval_div_by_zero;
         "interval mod" >:: test_interval_mod;
         "interval bw_and" >:: test_interval_bw_and;
         "interval bw_or" >:: test_interval_bw_or;
         "interval shift_left" >:: test_interval_shift_left;
         "interval shift_right" >:: test_interval_shift_right
       ]


(** Unit tests for cast operations *)
let cast_tests =
  "Interval Cast Operations"
  >::: [ "cast no overflow" >:: test_interval_cast_no_overflow;
         "cast overflow" >:: test_interval_cast_overflow;
         "cast bottom" >:: test_interval_cast_bottom;
         "cast same type" >:: test_interval_cast_same_type;
         "cast unsigned to signed" >:: test_interval_cast_unsigned_to_signed;
         "cast narrow to wide" >:: test_interval_cast_narrow_to_wide;
         "cast to loc" >:: test_interval_cast_to_loc
       ]


(** New property tests for arithmetic operations *)
let arithmetic_property_tests =
  "Interval Arithmetic Property Tests"
  >::: [ QCheck_ounit.to_ounit2_test arithmetic_soundness_prop;
         QCheck_ounit.to_ounit2_test division_soundness_prop
       ]


(** Property tests for cast operations *)
let cast_property_tests =
  "Interval Cast Property Tests"
  >::: [ QCheck_ounit.to_ounit2_test cast_soundness_prop;
         QCheck_ounit.to_ounit2_test cast_bottom_prop;
         QCheck_ounit.to_ounit2_test cast_identity_prop
       ]


let suite =
  "NonRelational IntervalBasis Tests"
  >::: [ unit_tests;
         property_tests;
         arithmetic_tests;
         arithmetic_property_tests;
         cast_tests;
         cast_property_tests
       ]
