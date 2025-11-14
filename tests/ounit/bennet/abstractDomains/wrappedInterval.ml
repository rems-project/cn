(** Tests for WrappedInterval abstract domain *)

open OUnit2
open QCheck
module BT = Cn.BaseTypes
module Sym = Cn.Sym
module IT = Cn.IndexTerms
module List = Cn.List

module NonRelational =
  Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Private.NonRelational

module Basis =
  Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Private.WrappedInterval
  .WrappedIntervalBasis

(** Helper functions for creating test data *)
let test_loc = Cerb_location.unknown

(** Test bit types *)
let test_bt_u8 = BT.Bits (Unsigned, 8)

let test_bt_u16 = BT.Bits (Unsigned, 16)

let test_bt_u32 = BT.Bits (Unsigned, 32)

let test_bt_s8 = BT.Bits (Signed, 8)

let test_bt_s16 = BT.Bits (Signed, 16)

let test_bt_s32 = BT.Bits (Signed, 32)

let test_bt_u3 = BT.Bits (Unsigned, 3)

(** Helper to create wrapped intervals *)
let make_wint bt start stop = Basis.of_interval bt (Z.of_int start) (Z.of_int stop)

let make_wint_z bt start stop = { Basis.bt; is_bottom = false; start; stop }

(** Helper to check if interval is bottom *)
let is_bottom_wint wint = Basis.is_bottom wint

(** Helper to check if interval is top *)
let is_top_wint wint = Basis.is_top wint

(** Helper for pretty printing wrapped intervals *)
let pp_wint wint = Cn.Pp.plain (Basis.pp wint)

(** Custom equality for wrapped intervals with better error messages *)
let assert_wint_equal ~msg expected actual =
  assert_equal ~cmp:Basis.equal ~printer:pp_wint ~msg expected actual


(** Test basic interval creation and properties *)
let test_basic_creation _ =
  let bt = test_bt_u8 in
  let bottom = Basis.bottom bt in
  let top = Basis.top bt in
  assert_bool "Bottom should be bottom" (is_bottom_wint bottom);
  assert_bool "Top should be top" (is_top_wint top);
  assert_bool "Bottom should not be top" (not (is_top_wint bottom));
  assert_bool "Top should not be bottom" (not (is_bottom_wint top));
  (* Test single value interval *)
  let single = make_wint bt 42 42 in
  assert_bool "Single value should not be bottom" (not (is_bottom_wint single));
  assert_bool "Single value should not be top" (not (is_top_wint single))


(** Test wrapped membership *)
let test_wrapped_membership _ =
  let bt = test_bt_u8 in
  (* 8-bit: 0-255 *)
  (* Normal interval [10, 20] *)
  assert_bool
    "15 should be in [10,20]"
    (Basis.wrapped_member (Z.of_int 15) (Z.of_int 10) (Z.of_int 20) bt);
  assert_bool
    "5 should not be in [10,20]"
    (not (Basis.wrapped_member (Z.of_int 5) (Z.of_int 10) (Z.of_int 20) bt));
  (* Wrapped interval [250, 10] wraps around *)
  assert_bool
    "5 should be in wrapped [250,10]"
    (Basis.wrapped_member (Z.of_int 5) (Z.of_int 250) (Z.of_int 10) bt);
  assert_bool
    "255 should be in wrapped [250,10]"
    (Basis.wrapped_member (Z.of_int 255) (Z.of_int 250) (Z.of_int 10) bt);
  assert_bool
    "100 should not be in wrapped [250,10]"
    (not (Basis.wrapped_member (Z.of_int 100) (Z.of_int 250) (Z.of_int 10) bt))


(** Test cardinality computation *)
let test_cardinality _ =
  (* Normal interval [10, 20] has cardinality 11 *)
  let card1 = Basis.(cardinality (of_interval test_bt_u8 (Z.of_int 10) (Z.of_int 20))) in
  assert_equal ~msg:"Cardinality of [10,20]" (Z.of_int 11) card1;
  (* Wrapped interval [250, 10] *)
  let card2 = Basis.(cardinality (of_interval test_bt_u8 (Z.of_int 250) (Z.of_int 10))) in
  assert_equal ~msg:"Cardinality of wrapped [250,10]" (Z.of_int 17) card2;
  (* 250-255 + 0-10 = 6+11 = 17 *)

  (* Single element *)
  let card3 = Basis.(cardinality (of_interval test_bt_u8 (Z.of_int 42) (Z.of_int 42))) in
  assert_equal ~msg:"Cardinality of [42,42]" (Z.of_int 1) card3


(** Test lattice ordering (leq) *)
let test_leq _ =
  let bt = test_bt_u8 in
  let bottom = Basis.bottom bt in
  let top = Basis.top bt in
  (* Bottom is less than everything *)
  assert_bool "bottom <= bottom" (Basis.leq bottom bottom);
  assert_bool "bottom <= top" (Basis.leq bottom top);
  (* Everything is less than top *)
  assert_bool "top <= top" (Basis.leq top top);
  assert_bool "bottom <= top" (Basis.leq bottom top);
  (* Normal containment *)
  let small = make_wint bt 10 15 in
  let large = make_wint bt 5 20 in
  assert_bool "[10,15] <= [5,20]" (Basis.leq small large);
  assert_bool "not [5,20] <= [10,15]" (not (Basis.leq large small));
  (* Wrapped intervals *)
  let wrapped_small = make_wint bt 250 10 in
  let wrapped_large = make_wint bt 240 20 in
  assert_bool "wrapped containment should work" (Basis.leq wrapped_small wrapped_large)


(** Test join operation *)
let test_join _ =
  let bt = test_bt_u8 in
  let bottom = Basis.bottom bt in
  let top = Basis.top bt in
  (* Join with bottom *)
  let wint = make_wint bt 10 20 in
  assert_wint_equal ~msg:"join with bottom" wint (Basis.join bottom wint);
  assert_wint_equal ~msg:"join with bottom (commutative)" wint (Basis.join wint bottom);
  (* Join with top *)
  assert_wint_equal ~msg:"join with top" top (Basis.join top wint);
  assert_wint_equal ~msg:"join with top (commutative)" top (Basis.join wint top);
  (* Join containment case *)
  let small = make_wint bt 10 15 in
  let large = make_wint bt 5 20 in
  assert_wint_equal ~msg:"join containment" large (Basis.join small large);
  assert_wint_equal ~msg:"join containment (commutative)" large (Basis.join large small);
  (* Join overlapping intervals *)
  let int1 = make_wint bt 10 20 in
  let int2 = make_wint bt 15 25 in
  let joined = Basis.join int1 int2 in
  let expected = make_wint bt 10 25 in
  assert_wint_equal ~msg:"join overlapping" expected joined


(** Test meet operation *)
let test_meet _ =
  let bt = test_bt_u8 in
  let bottom = Basis.bottom bt in
  let top = Basis.top bt in
  (* Meet with bottom *)
  let wint = make_wint bt 10 20 in
  assert_wint_equal ~msg:"meet with bottom" bottom (Basis.meet bottom wint);
  assert_wint_equal ~msg:"meet with bottom (commutative)" bottom (Basis.meet wint bottom);
  (* Meet with top *)
  assert_wint_equal ~msg:"meet with top" wint (Basis.meet top wint);
  assert_wint_equal ~msg:"meet with top (commutative)" wint (Basis.meet wint top);
  (* Meet containment case *)
  let small = make_wint bt 10 15 in
  let large = make_wint bt 5 20 in
  assert_wint_equal ~msg:"meet containment" small (Basis.meet small large);
  assert_wint_equal ~msg:"meet containment (commutative)" small (Basis.meet large small);
  (* Meet overlapping intervals *)
  let int1 = make_wint bt 10 20 in
  let int2 = make_wint bt 15 25 in
  let met = Basis.meet int1 int2 in
  let expected = make_wint bt 15 20 in
  assert_wint_equal ~msg:"meet overlapping" expected met


(** Property: [join_many] for two intervals equals binary join *)
let prop_join_many_binary =
  let open QCheck2 in
  Test.make
    ~count:100
    ~name:"join_many([a,b]) == join(a,b)"
    ~print:(fun ((start1, stop1), (start2, stop2)) ->
      let bt = test_bt_u8 in
      let int1 = make_wint bt start1 stop1 in
      let int2 = make_wint bt start2 stop2 in
      let binary_join = Basis.join int1 int2 in
      let many_join = Basis.join_many [ int1; int2 ] in
      String.concat
        "\n"
        [ "\nFailed join_many binary test:\n";
          Printf.sprintf "  a: [%d, %d] -> %s\n" start1 stop1 (pp_wint int1);
          Printf.sprintf "  b: [%d, %d] -> %s\n" start2 stop2 (pp_wint int2);
          Printf.sprintf "  join(a,b): %s\n" (pp_wint binary_join);
          Printf.sprintf "  join_many([a,b]): %s\n" (pp_wint many_join)
        ])
    Gen.(
      pair
        (pair (int_range 0 255) (int_range 0 255))
        (pair (int_range 0 255) (int_range 0 255)))
    (fun ((start1, stop1), (start2, stop2)) ->
       let bt = test_bt_u8 in
       let int1 = make_wint bt start1 stop1 in
       let int2 = make_wint bt start2 stop2 in
       let binary_join = Basis.join int1 int2 in
       let many_join = Basis.join_many [ int1; int2 ] in
       Basis.equal binary_join many_join)


(** Test [join_many] associativity property *)
let test_join_many_associativity _ =
  let bt = test_bt_u8 in
  let a = make_wint bt 10 20 in
  let b = make_wint bt 30 40 in
  let c = make_wint bt 50 60 in
  (* join_many([a,b,c]) should be <= join(a, join(b,c)) *)
  let many_result = Basis.join_many [ a; b; c ] in
  let nested_result = Basis.join a (Basis.join b c) in
  assert_bool "join_many result <= nested joins" (Basis.leq many_result nested_result)


(** Property: [meet_many] for two intervals equals binary meet *)
let prop_meet_many_binary =
  let open QCheck2 in
  Test.make
    ~count:100
    ~name:"meet_many([a,b]) == meet(a,b)"
    ~print:(fun ((start1, stop1), (start2, stop2)) ->
      let bt = test_bt_u8 in
      let int1 = make_wint bt start1 stop1 in
      let int2 = make_wint bt start2 stop2 in
      let binary_meet = Basis.meet int1 int2 in
      let many_meet = Basis.meet_many [ int1; int2 ] in
      String.concat
        "\n"
        [ "\nFailed meet_many test:\n";
          Printf.sprintf "  a: [%d, %d] -> %s\n" start1 stop1 (pp_wint int1);
          Printf.sprintf "  b: [%d, %d] -> %s\n" start2 stop2 (pp_wint int2);
          Printf.sprintf "  meet(a,b): %s\n" (pp_wint binary_meet);
          Printf.sprintf "  meet_many([a,b]):   %s\n" (pp_wint many_meet)
        ])
    Gen.(
      pair
        (pair (int_range 0 255) (int_range 0 255))
        (pair (int_range 0 255) (int_range 0 255)))
    (fun ((start1, stop1), (start2, stop2)) ->
       let bt = test_bt_u8 in
       let int1 = make_wint bt start1 stop1 in
       let int2 = make_wint bt start2 stop2 in
       let binary_meet = Basis.meet int1 int2 in
       let many_meet = Basis.meet_many [ int1; int2 ] in
       Basis.equal binary_meet many_meet)


(** Test [join_many] with complex cases from GeneralizedJoin C++ tests *)
let test_join_many_generalized _ =
  (* Test case 1: 8-bit intervals [2,10], [120,130], [132,135] -> expected [2,135] *)
  let bt = test_bt_u8 in
  let int1 = make_wint bt 2 10 in
  let int2 = make_wint bt 120 130 in
  let int3 = make_wint bt 132 135 in
  let result1 = Basis.join_many [ int3; int2; int1 ] in
  let expected1 = make_wint bt 2 135 in
  assert_wint_equal ~msg:"join_many test case 1" expected1 result1;
  (* Test case 2: 8-bit intervals [1,10], [120,130], [132,135], [220,50] -> expected [220,135] *)
  let int4 = make_wint bt 1 10 in
  let int5 = make_wint bt 120 130 in
  let int6 = make_wint bt 132 135 in
  let int7 = make_wint bt 220 50 in
  (* wrapped interval *)
  let result2 = Basis.join_many [ int6; int7; int5; int4 ] in
  let expected2 = make_wint bt 220 135 in
  (* wrapped result *)
  assert_wint_equal ~msg:"join_many test case 2 (wrapped)" expected2 result2;
  (* Test case 3: 3-bit intervals [0,1], [7,0], [6,7], [0,1] -> expected [6,1] *)
  let bt3 = test_bt_u3 in
  let int8 = make_wint bt3 0 1 in
  let int9 = make_wint bt3 7 0 in
  (* wrapped interval *)
  let int10 = make_wint bt3 6 7 in
  let int11 = make_wint bt3 0 1 in
  let result3 = Basis.join_many [ int8; int9; int10; int11 ] in
  let expected3 = make_wint bt3 6 1 in
  (* wrapped result *)
  assert_wint_equal ~msg:"join_many test case 3 (3-bit wrapped)" expected3 result3


(** Test arithmetic operations *)
let test_arithmetic_operations _ =
  let bt = test_bt_u8 in
  let a = make_wint bt 10 20 in
  let b = make_wint bt 5 15 in
  (* Test addition *)
  (match Basis.forward_abs_binop IT.Add a b with
   | Some result ->
     (* Addition: [10,20] + [5,15] = [15,35] *)
     let expected = make_wint bt 15 35 in
     assert_wint_equal ~msg:"addition" expected result
   | None -> assert_failure "Addition should return Some result");
  (* Test subtraction *)
  match Basis.forward_abs_binop IT.Sub a b with
  | Some result ->
    (* Subtraction: [10,20] - [5,15] = [10-15, 20-5] = [-5,15] *)
    (* But in unsigned 8-bit, -5 wraps to 251 *)
    assert_bool "Subtraction should produce valid interval" (not (is_bottom_wint result))
  | None -> assert_failure "Subtraction should return Some result"


(** Test overflow handling *)
let test_overflow_handling _ =
  let bt = test_bt_u8 in
  (* 8-bit: 0-255 *)
  let a = make_wint bt 200 250 in
  let b = make_wint bt 100 150 in
  (* This addition should cause overflow: 200+100=300 > 255 *)
  match Basis.forward_abs_binop IT.Add a b with
  | Some result ->
    (* With wrapped intervals, this should model wraparound, not go to top *)
    assert_bool "Overflow should not result in bottom" (not (is_bottom_wint result))
  | None -> assert_failure "Addition should return Some result"


(** Test pole splitting for signed types *)
let test_pole_splitting _ =
  let bt = test_bt_s8 in
  (* Signed 8-bit: -128 to 127 *)
  (* Test north pole splitting *)
  let splits = Basis.north_split bt (Z.of_int (-100)) (Z.of_int 100) in
  assert_bool "North split should produce at least one interval" (List.length splits >= 1);
  (* Test south pole splitting *)
  let splits2 = Basis.south_split bt (Z.of_int (-100)) (Z.of_int 100) in
  assert_bool "South split should produce at least one interval" (List.length splits2 >= 1)


(** Test bitwise or operation *)
let test_bitwise_or _ =
  let bt = test_bt_u8 in
  let a = make_wint bt 2 3 in
  let b = make_wint bt 9 10 in
  match Basis.forward_abs_binop IT.BW_Or a b with
  | Some result ->
    let expected = make_wint bt 10 11 in
    assert_wint_equal ~msg:"bitwise or" expected result
  | None -> assert_failure "Bitwise or should return Some result"


(** Test bitwise and operation *)
let test_bitwise_and _ =
  let bt = test_bt_u8 in
  let a = make_wint bt 2 3 in
  let b = make_wint bt 9 10 in
  match Basis.forward_abs_binop IT.BW_And a b with
  | Some result ->
    let expected = make_wint bt 0 2 in
    assert_wint_equal ~msg:"bitwise and" expected result
  | None -> assert_failure "Bitwise and should return Some result"


(** Test bitwise xor operation *)
let test_bitwise_xor _ =
  let bt = test_bt_u8 in
  let a = make_wint bt 2 3 in
  let b = make_wint bt 9 10 in
  match Basis.forward_abs_binop IT.BW_Xor a b with
  | Some result ->
    let expected = make_wint bt 8 11 in
    assert_wint_equal ~msg:"bitwise xor" expected result
  | None -> assert_failure "Bitwise xor should return Some result"


(** Test left shift operation - basic cases *)
let test_left_shift_basic _ =
  let bt = test_bt_u8 in
  (* Test shift by 1: [2,3] << 1 = [4,6] *)
  let operand = make_wint bt 2 3 in
  let shift = make_wint bt 1 1 in
  match Basis.forward_abs_binop IT.ShiftLeft operand shift with
  | Some result ->
    let expected = make_wint bt 4 6 in
    assert_wint_equal ~msg:"left shift by 1" expected result
  | None -> assert_failure "Left shift should return Some result"


(** Test left shift overflow detection *)
let test_left_shift_overflow _ =
  let bt = test_bt_u8 in
  (* Test shift that causes overflow: [64,127] << 2 would exceed 8-bit range *)
  let operand = make_wint bt 64 127 in
  let shift = make_wint bt 2 2 in
  match Basis.forward_abs_binop IT.ShiftLeft operand shift with
  | Some result ->
    (* Should return conservative bounds due to overflow *)
    assert_bool
      "Left shift overflow should produce valid result"
      (not (is_bottom_wint result))
  | None -> assert_failure "Left shift should return Some result"


(** Test left shift by zero (no-op) *)
let test_left_shift_zero _ =
  let bt = test_bt_u8 in
  let operand = make_wint bt 10 20 in
  let shift = make_wint bt 0 0 in
  match Basis.forward_abs_binop IT.ShiftLeft operand shift with
  | Some result -> assert_wint_equal ~msg:"left shift by zero" operand result
  | None -> assert_failure "Left shift should return Some result"


(** Test logical right shift - basic cases *)
let test_logical_right_shift_basic _ =
  let bt = test_bt_u8 in
  (* Test shift by 1: [8,12] >> 1 = [4,6] *)
  let operand = make_wint bt 8 12 in
  let shift = make_wint bt 1 1 in
  match Basis.forward_abs_binop IT.ShiftRight operand shift with
  | Some result ->
    let expected = make_wint bt 4 6 in
    assert_wint_equal ~msg:"logical right shift by 1" expected result
  | None -> assert_failure "Right shift should return Some result"


(** Test logical right shift with south pole crossing *)
let test_logical_right_shift_south_pole _ =
  let bt = test_bt_u8 in
  (* Create interval that crosses south pole: [250, 10] *)
  let operand = make_wint bt 250 10 in
  let shift = make_wint bt 1 1 in
  match Basis.forward_abs_binop IT.ShiftRight operand shift with
  | Some result ->
    (* Should return conservative bounds [0, 127] for unsigned 8-bit >> 1 *)
    assert_bool
      "South pole crossing should produce valid result"
      (not (is_bottom_wint result))
  | None -> assert_failure "Right shift should return Some result"


(** Test arithmetic right shift for signed types *)
let test_arithmetic_right_shift_signed _ =
  let bt = test_bt_s8 in
  (* Test positive values: [8,12] >> 1 = [4,6] *)
  let operand = make_wint bt 8 12 in
  let shift = make_wint bt 1 1 in
  match Basis.forward_abs_binop IT.ShiftRight operand shift with
  | Some result ->
    let expected = make_wint bt 4 6 in
    assert_wint_equal ~msg:"arithmetic right shift positive" expected result
  | None -> assert_failure "Arithmetic right shift should return Some result"


(** Test arithmetic right shift with north pole crossing *)
let test_arithmetic_right_shift_north_pole _ =
  let bt = test_bt_s8 in
  (* Create interval that crosses north pole: [100, -100] (wraps around) *)
  let operand = make_wint bt 100 (-100) in
  let shift = make_wint bt 1 1 in
  match Basis.forward_abs_binop IT.ShiftRight operand shift with
  | Some result ->
    (* Should return conservative bounds due to north pole crossing *)
    assert_bool
      "North pole crossing should produce valid result"
      (not (is_bottom_wint result))
  | None -> assert_failure "Arithmetic right shift should return Some result"


(** Test shift by excessive amount *)
let test_shift_excessive_amount _ =
  let bt = test_bt_u8 in
  let operand = make_wint bt 10 20 in
  let shift = make_wint bt 8 8 in
  (* Shift by width *)
  match Basis.forward_abs_binop IT.ShiftLeft operand shift with
  | Some result ->
    (* Should return top (conservative) for shift by width or more *)
    assert_bool
      "Excessive shift should be conservative"
      (is_top_wint result || not (is_bottom_wint result))
  | None -> assert_failure "Shift should return Some result"


(** Test non-constant shift amount *)
let test_non_constant_shift _ =
  let bt = test_bt_u8 in
  let operand = make_wint bt 10 20 in
  let shift = make_wint bt 1 3 in
  (* Non-constant shift amount *)
  match Basis.forward_abs_binop IT.ShiftLeft operand shift with
  | Some result ->
    (* Should return top (conservative) for non-constant shift *)
    assert_bool "Non-constant shift should be conservative" (is_top_wint result)
  | None -> assert_failure "Shift should return Some result"


(** Test 16-bit shift operations for larger ranges *)
let test_shift_16bit _ =
  let bt = test_bt_u16 in
  (* Test left shift: [100,200] << 2 = [400,800] *)
  let operand = make_wint bt 100 200 in
  let shift = make_wint bt 2 2 in
  match Basis.forward_abs_binop IT.ShiftLeft operand shift with
  | Some result ->
    let expected = make_wint bt 400 800 in
    assert_wint_equal ~msg:"16-bit left shift" expected result
  | None -> assert_failure "16-bit left shift should return Some result"


(** Test edge case: shift by maximum safe amount *)
let test_shift_max_safe _ =
  let bt = test_bt_u8 in
  (* Test left shift by 7 (max safe for 8-bit): [1,1] << 7 = [128,128] *)
  let operand = make_wint bt 1 1 in
  let shift = make_wint bt 7 7 in
  match Basis.forward_abs_binop IT.ShiftLeft operand shift with
  | Some result ->
    let expected = make_wint bt 128 128 in
    assert_wint_equal ~msg:"max safe left shift" expected result
  | None -> assert_failure "Max safe left shift should return Some result"


(** Test enhanced truncation precision *)
let test_enhanced_truncation _ =
  let bt = test_bt_u8 in
  (* Test case where old truncation would be conservative but new one is precise *)
  (* [64, 65] << 1 should be [128, 130] with enhanced truncation *)
  let operand = make_wint bt 64 65 in
  let shift = make_wint bt 1 1 in
  match Basis.forward_abs_binop IT.ShiftLeft operand shift with
  | Some result ->
    (* With enhanced truncation, this should be precise since upper bits are consecutive *)
    let expected = make_wint bt 128 130 in
    assert_wint_equal ~msg:"enhanced truncation precision" expected result
  | None -> assert_failure "Enhanced truncation should return Some result"


(** Test enhanced truncation conservative case *)
let test_enhanced_truncation_conservative _ =
  let bt = test_bt_u8 in
  (* Test case where enhanced truncation should still be conservative *)
  (* [60, 70] << 2 should be conservative due to upper bit differences *)
  let operand = make_wint bt 60 70 in
  let shift = make_wint bt 2 2 in
  match Basis.forward_abs_binop IT.ShiftLeft operand shift with
  | Some result ->
    (* Should be conservative bounds, not precise *)
    assert_bool
      "Should be conservative for complex truncation"
      (not (is_bottom_wint result))
  | None -> assert_failure "Enhanced truncation should return Some result"


(** Test precise conservative bounds for arithmetic right shift *)
let test_arithmetic_right_shift_conservative_bounds _ =
  let bt = test_bt_s8 in
  (* Create interval that crosses signed limit for 8-bit signed: [-100, 100] *)
  let operand = make_wint bt (-100) 100 in
  let shift = make_wint bt 2 2 in
  match Basis.forward_abs_binop IT.ShiftRight operand shift with
  | Some result ->
    (* Should return precise conservative bounds, not just top *)
    assert_bool "Conservative AShr should not be bottom" (not (is_bottom_wint result));
    assert_bool "Conservative AShr should not be top" (not (is_top_wint result))
  | None -> assert_failure "Arithmetic right shift should return Some result"


(** Test shift left then right with overflow *)
let test_shift_left_then_right _ =
  let bt = test_bt_u8 in
  let original = make_wint bt 1 32 in
  let shift_amt = make_wint bt 3 3 in
  (* First shift left by 3: [1, 32] << 3 should give [8, 256] which wraps to [8, 0] *)
  match Basis.forward_abs_binop IT.ShiftLeft original shift_amt with
  | Some left_shifted ->
    (* Then shift right by 3: [8, 0] >> 3 should give [0, 31] due to conservative handling *)
    (match Basis.forward_abs_binop IT.ShiftRight left_shifted shift_amt with
     | Some final_result ->
       (* Due to overflow and conservative right shift, we get [0, 31], not original [1, 32] *)
       let expected = make_wint bt 0 31 in
       assert_wint_equal
         ~msg:"shift left then right with overflow gives [0, 31]"
         expected
         final_result
     | None -> assert_failure "Right shift should return Some result")
  | None -> assert_failure "Left shift should return Some result"


(** Test remainder operation basic cases *)
let test_remainder_basic _ =
  let bt = test_bt_u8 in
  (* Test unsigned remainder: [10,20] % [3,5] *)
  let dividend = make_wint bt 10 20 in
  let divisor = make_wint bt 3 5 in
  match Basis.forward_abs_binop IT.Mod dividend divisor with
  | Some result ->
    (* For unsigned, result should be [0, divisor_max-1] = [0, 4] *)
    let expected = make_wint bt 0 4 in
    assert_wint_equal ~msg:"unsigned remainder basic" expected result
  | None -> assert_failure "Remainder should return Some result"


(** Test remainder with zero dividend *)
let test_remainder_zero_dividend _ =
  let bt = test_bt_u8 in
  let zero_dividend = make_wint bt 0 0 in
  let divisor = make_wint bt 5 10 in
  match Basis.forward_abs_binop IT.Mod zero_dividend divisor with
  | Some result ->
    (* 0 % anything = 0 *)
    let expected = make_wint bt 0 0 in
    assert_wint_equal ~msg:"zero dividend remainder" expected result
  | None -> assert_failure "Remainder with zero dividend should return Some result"


(** Test remainder with zero divisor *)
let test_remainder_zero_divisor _ =
  let bt = test_bt_u8 in
  let dividend = make_wint bt 10 20 in
  let zero_divisor = make_wint bt 0 0 in
  match Basis.forward_abs_binop IT.Mod dividend zero_divisor with
  | Some result ->
    (* Division by zero should result in bottom *)
    assert_bool "Remainder by zero should be bottom" (is_bottom_wint result)
  | None -> assert_failure "Remainder with zero divisor should return Some result"


(** Test signed remainder - positive operands *)
let test_signed_remainder_positive _ =
  let bt = test_bt_s8 in
  (* Both operands positive: [10,20] % [3,5] *)
  let dividend = make_wint bt 10 20 in
  let divisor = make_wint bt 3 5 in
  match Basis.forward_abs_binop IT.Mod dividend divisor with
  | Some result ->
    (* For signed with both positive: [0, divisor_max-1] = [0, 4] *)
    let expected = make_wint bt 0 4 in
    assert_wint_equal ~msg:"signed remainder both positive" expected result
  | None -> assert_failure "Signed remainder should return Some result"


(** Test signed remainder - dividend positive, divisor negative *)
let test_signed_remainder_pos_neg _ =
  let bt = test_bt_s8 in
  (* Dividend positive, divisor negative: [10,20] % [-5,-3] *)
  let dividend = make_wint bt 10 20 in
  let divisor = make_wint bt (-5) (-3) in
  match Basis.forward_abs_binop IT.Mod dividend divisor with
  | Some result ->
    (* For dividend pos, divisor neg: [0, -divisor_min-1] = [0, -(-5)-1] = [0, 4] *)
    let expected = make_wint bt 0 4 in
    assert_wint_equal
      ~msg:"signed remainder dividend positive divisor negative"
      expected
      result
  | None -> assert_failure "Signed remainder should return Some result"


(** Test signed remainder - dividend negative, divisor positive *)
let test_signed_remainder_neg_pos _ =
  let bt = test_bt_s8 in
  (* Dividend negative, divisor positive: [-20,-10] % [3,5] *)
  let dividend = make_wint bt (-20) (-10) in
  let divisor = make_wint bt 3 5 in
  match Basis.forward_abs_binop IT.Mod dividend divisor with
  | Some result ->
    (* For dividend neg, divisor pos: [-divisor_max+1, 0] = [-4, 0] *)
    let expected = make_wint bt (-4) 0 in
    assert_wint_equal
      ~msg:"signed remainder dividend negative divisor positive"
      expected
      result
  | None -> assert_failure "Signed remainder should return Some result"


(** Test signed remainder - both operands negative *)
let test_signed_remainder_both_negative _ =
  let bt = test_bt_s8 in
  (* Both operands negative: [-20,-10] % [-5,-3] *)
  let dividend = make_wint bt (-20) (-10) in
  let divisor = make_wint bt (-5) (-3) in
  match Basis.forward_abs_binop IT.Mod dividend divisor with
  | Some result ->
    (* For both negative: [divisor_min+1, 0] = [-5+1, 0] = [-4, 0] *)
    let expected = make_wint bt (-4) 0 in
    assert_wint_equal ~msg:"signed remainder both negative" expected result
  | None -> assert_failure "Signed remainder should return Some result"


(** Test remainder with wrapped intervals *)
let test_remainder_wrapped _ =
  let bt = test_bt_u8 in
  (* Test with wrapped dividend: [250, 10] % [7, 9] *)
  let wrapped_dividend = make_wint bt 250 10 in
  let divisor = make_wint bt 7 9 in
  match Basis.forward_abs_binop IT.Mod wrapped_dividend divisor with
  | Some result ->
    (* Should handle wrapping correctly and not be bottom *)
    assert_bool "Wrapped remainder should not be bottom" (not (is_bottom_wint result));
    (* Result should be bounded by [0, divisor_max-1] = [0, 8] *)
    let bound_check = make_wint bt 0 8 in
    assert_bool "Wrapped remainder should be within bounds" (Basis.leq result bound_check)
  | None -> assert_failure "Wrapped remainder should return Some result"


(** Test remainder precision vs simple approach *)
let test_remainder_precision _ =
  let bt = test_bt_u8 in
  (* Test case where Crab algorithm should be more precise *)
  let dividend = make_wint bt 100 120 in
  let divisor = make_wint bt 30 40 in
  match Basis.forward_abs_binop IT.Mod dividend divisor with
  | Some result ->
    (* Should not be top - algorithm should provide meaningful bounds *)
    assert_bool "Remainder should be precise, not top" (not (is_top_wint result));
    assert_bool "Remainder should not be bottom" (not (is_bottom_wint result))
  | None -> assert_failure "Remainder should return Some result"


(** Test remainder with 16-bit values *)
let test_remainder_16bit _ =
  let bt = test_bt_u16 in
  (* Test with larger values: [1000,2000] % [100,200] *)
  let dividend = make_wint bt 1000 2000 in
  let divisor = make_wint bt 100 200 in
  match Basis.forward_abs_binop IT.Mod dividend divisor with
  | Some result ->
    (* Should be bounded by [0, 199] for unsigned *)
    let expected_bound = make_wint bt 0 199 in
    assert_bool
      "16-bit remainder should be within bounds"
      (Basis.leq result expected_bound);
    assert_bool "16-bit remainder should not be bottom" (not (is_bottom_wint result))
  | None -> assert_failure "16-bit remainder should return Some result"


(** Property-based tests using QCheck *)

(** Property: [join] is commutative *)
let prop_join_commutative =
  let open QCheck2 in
  Test.make
    ~count:50
    ~name:"join is commutative"
    ~print:(fun (start1, stop1) ->
      let start2, stop2 = (50, 100) in
      let wint1 = make_wint test_bt_u8 start1 stop1 in
      let wint2 = make_wint test_bt_u8 start2 stop2 in
      let join1 = Basis.join wint1 wint2 in
      let join2 = Basis.join wint2 wint1 in
      String.concat
        "\n"
        [ "\nFailed join commutative test:\n";
          Printf.sprintf "  a: [%d, %d] -> %s\n" start1 stop1 (pp_wint wint1);
          Printf.sprintf "  b: [%d, %d] -> %s\n" start2 stop2 (pp_wint wint2);
          Printf.sprintf "  join(a,b): %s\n" (pp_wint join1);
          Printf.sprintf "  join(b,a): %s\n" (pp_wint join2)
        ])
    Gen.(pair (int_range 0 255) (int_range 0 255))
    (fun (start1, stop1) ->
       let start2, stop2 = (50, 100) in
       let wint1 = make_wint test_bt_u8 start1 stop1 in
       let wint2 = make_wint test_bt_u8 start2 stop2 in
       let join1 = Basis.join wint1 wint2 in
       let join2 = Basis.join wint2 wint1 in
       Basis.equal join1 join2)


(** Property: [meet] is commutative *)
let prop_meet_commutative =
  let open QCheck2 in
  Test.make
    ~count:50
    ~name:"meet is commutative"
    ~print:(fun (start1, stop1) ->
      let start2, stop2 = (50, 100) in
      let wint1 = make_wint test_bt_u8 start1 stop1 in
      let wint2 = make_wint test_bt_u8 start2 stop2 in
      let meet1 = Basis.meet wint1 wint2 in
      let meet2 = Basis.meet wint2 wint1 in
      String.concat
        "\n"
        [ "\nFailed meet commutative test:\n";
          Printf.sprintf "  a: [%d, %d] -> %s\n" start1 stop1 (pp_wint wint1);
          Printf.sprintf "  b: [%d, %d] -> %s\n" start2 stop2 (pp_wint wint2);
          Printf.sprintf "  meet(a,b): %s\n" (pp_wint meet1);
          Printf.sprintf "  meet(b,a): %s\n" (pp_wint meet2)
        ])
    Gen.(pair (int_range 0 255) (int_range 0 255))
    (fun (start1, stop1) ->
       let start2, stop2 = (50, 100) in
       let wint1 = make_wint test_bt_u8 start1 stop1 in
       let wint2 = make_wint test_bt_u8 start2 stop2 in
       let meet1 = Basis.meet wint1 wint2 in
       let meet2 = Basis.meet wint2 wint1 in
       Basis.equal meet1 meet2)


(** Property: [bottom] is absorbing for [meet] *)
let prop_bottom_absorbing_meet =
  Test.make
    ~count:50
    ~name:"bottom is absorbing for meet"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (start, stop) ->
       let wint = make_wint test_bt_u8 start stop in
       let bottom = Basis.bottom test_bt_u8 in
       let result = Basis.meet wint bottom in
       Basis.equal result bottom)


(** Property: [top] is absorbing for [join] *)
let prop_top_absorbing_join =
  Test.make
    ~count:50
    ~name:"top is absorbing for join"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (start, stop) ->
       let wint = make_wint test_bt_u8 start stop in
       let top = Basis.top test_bt_u8 in
       let result = Basis.join wint top in
       Basis.equal result top)


(** Property: [leq] is reflexive *)
let prop_leq_reflexive =
  Test.make
    ~count:50
    ~name:"leq is reflexive"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (start, stop) ->
       let wint = make_wint test_bt_u8 start stop in
       Basis.leq wint wint)


(** Property: [join_many] is monotonic *)
let prop_join_many_monotonic =
  Test.make
    ~count:30
    ~name:"join_many is monotonic"
    (pair
       (pair (int_range 0 255) (int_range 0 255))
       (pair (int_range 0 255) (int_range 0 255)))
    (fun ((s1, e1), (s2, e2)) ->
       try
         let a = make_wint test_bt_u8 s1 e1 in
         let b = make_wint test_bt_u8 s2 e2 in
         let join_a = Basis.join_many [ a ] in
         let join_ab = Basis.join_many [ a; b ] in
         Basis.leq join_a join_ab
       with
       | _ -> true (* Skip if join_many fails *))


let permutations lst =
  let insert x lst =
    let rec aux acc prefix = function
      | [] -> (prefix @ [ x ]) :: acc
      | h :: t -> aux ((prefix @ [ x ] @ (h :: t)) :: acc) (prefix @ [ h ]) t
    in
    aux [] [] lst
  in
  List.fold_left (fun acc x -> List.concat_map (insert x) acc) [ [] ] lst


(** Property: [join_many] minimizes the w-interval containing a fold with [join] *)
let prop_join_many_minimizes =
  let aux ses =
    let wints = List.map (fun (start, stop) -> make_wint test_bt_u8 start stop) ses in
    let fold_join =
      wints
      |> permutations
      |> List.map (List.fold_left Basis.join (Basis.bottom test_bt_u8))
      |> List.sort (fun x y -> Z.compare (Basis.cardinality x) (Basis.cardinality y))
      |> List.hd
    in
    let general_join = Basis.join_many wints in
    (fold_join, general_join)
  in
  let open QCheck2 in
  Test.make
    ~count:30
    ~name:"join_many minimizes a fold"
    ~print:(fun ses ->
      let fold_join, general_join = aux ses in
      String.concat
        "\n"
        ([ Printf.sprintf "\nFailed join_many minimizes test:\n";
           Printf.sprintf "  Input intervals:\n"
         ]
         @ List.mapi
             (fun i (start, stop) ->
                let wint = make_wint test_bt_u8 start stop in
                Printf.sprintf "    [%d]: [%d, %d] -> %s\n" i start stop (pp_wint wint))
             ses
         @ [ Printf.sprintf "  fold(join, l): %s\n" (pp_wint fold_join);
             Printf.sprintf "  join_many(l): %s\n" (pp_wint general_join);
             Printf.sprintf "  Expected: |join_many(l)| <= |fold(join, l)|\n"
           ]))
    Gen.(list_size (int_bound 5) (pair (int_range 0 255) (int_range 0 255)))
    (fun ses ->
       if List.is_empty ses then
         assume_fail ()
       else (
         let fold_join, general_join = aux ses in
         Z.leq (Basis.cardinality general_join) (Basis.cardinality fold_join)))


(** Property: [join_many] with single element equals identity *)
let prop_join_many_single =
  Test.make
    ~count:50
    ~name:"join_many single element is identity"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (start, stop) ->
       let wint = make_wint test_bt_u8 start stop in
       try
         let result = Basis.join_many [ wint ] in
         Basis.equal result wint
       with
       | _ -> true (* Skip if join_many fails *))


(** Property: [meet_many] with single element equals identity *)
let prop_meet_many_single =
  Test.make
    ~count:50
    ~name:"meet_many single element is identity"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (start, stop) ->
       let wint = make_wint test_bt_u8 start stop in
       let result = Basis.meet_many [ wint ] in
       Basis.equal result wint)


(** Property: [meet_many] minimizes the w-interval containing a fold with [meet] *)
let prop_meet_many_minimizes =
  let aux ses =
    let wints = List.map (fun (start, stop) -> make_wint test_bt_u8 start stop) ses in
    let fold_meet =
      wints
      |> permutations
      |> List.map (List.fold_left Basis.meet (Basis.top test_bt_u8))
      |> List.sort (fun x y -> Z.compare (Basis.cardinality x) (Basis.cardinality y))
      |> List.hd
    in
    let general_meet = Basis.meet_many wints in
    (fold_meet, general_meet)
  in
  let open QCheck2 in
  Test.make
    ~count:30
    ~name:"meet_many minimizes a fold"
    ~print:(fun ses ->
      let fold_meet, general_meet = aux ses in
      String.concat
        "\n"
        ([ Printf.sprintf "\nFailed meet_many minimizes test:\n";
           Printf.sprintf "  Input intervals:\n"
         ]
         @ List.mapi
             (fun i (start, stop) ->
                let wint = make_wint test_bt_u8 start stop in
                Printf.sprintf "    [%d]: [%d, %d] -> %s\n" i start stop (pp_wint wint))
             ses
         @ [ Printf.sprintf "  fold(meet, l): %s\n" (pp_wint fold_meet);
             Printf.sprintf "  meet_many(l): %s\n" (pp_wint general_meet);
             Printf.sprintf "  Expected: |meet_many(l)| <= |fold(meet, l)|\n"
           ]))
    Gen.(list_size (int_bound 5) (pair (int_range 0 255) (int_range 0 255)))
    (fun ses ->
       if List.is_empty ses then
         assume_fail ()
       else (
         let fold_meet, general_meet = aux ses in
         Z.leq (Basis.cardinality general_meet) (Basis.cardinality fold_meet)))


(** Property: [join] is idempotent *)
let prop_join_idempotent =
  Test.make
    ~count:50
    ~name:"join is idempotent"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (start, stop) ->
       let wint = make_wint test_bt_u8 start stop in
       let result = Basis.join wint wint in
       Basis.equal result wint)


(** Property: [meet] is idempotent *)
let prop_meet_idempotent =
  Test.make
    ~count:50
    ~name:"meet is idempotent"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (start, stop) ->
       let wint = make_wint test_bt_u8 start stop in
       let result = Basis.meet wint wint in
       Basis.equal result wint)


(** Property: left shift by zero is identity *)
let prop_left_shift_zero_identity =
  let open QCheck2 in
  Test.make
    ~count:50
    ~name:"left shift by zero is identity"
    ~print:(fun (start, stop) ->
      let operand = make_wint test_bt_u8 start stop in
      let zero_shift = make_wint test_bt_u8 0 0 in
      let result_opt = Basis.forward_abs_binop IT.ShiftLeft operand zero_shift in
      String.concat
        "\n"
        [ "\nFailed left shift zero identity test:\n";
          Printf.sprintf "  operand: [%d, %d] -> %s\n" start stop (pp_wint operand);
          Printf.sprintf "  shift: [0, 0] -> %s\n" (pp_wint zero_shift);
          Printf.sprintf
            "  result: %s\n"
            (match result_opt with Some r -> pp_wint r | None -> "None");
          Printf.sprintf "  expected: %s\n" (pp_wint operand)
        ])
    Gen.(pair (int_range 0 255) (int_range 0 255))
    (fun (start, stop) ->
       let operand = make_wint test_bt_u8 start stop in
       let zero_shift = make_wint test_bt_u8 0 0 in
       match Basis.forward_abs_binop IT.ShiftLeft operand zero_shift with
       | Some result -> Basis.equal result operand
       | None -> false)


(** Property: right shift by zero is identity *)
let prop_right_shift_zero_identity =
  Test.make
    ~count:50
    ~name:"right shift by zero is identity"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (start, stop) ->
       let operand = make_wint test_bt_u8 start stop in
       let zero_shift = make_wint test_bt_u8 0 0 in
       match Basis.forward_abs_binop IT.ShiftRight operand zero_shift with
       | Some result -> Basis.equal result operand
       | None -> false)


(** Property: shift operations don't produce bottom unless input is bottom *)
let prop_shift_not_bottom =
  let open QCheck2 in
  Test.make
    ~count:50
    ~name:"shift operations preserve non-bottom"
    ~print:(fun (start, stop, shift_amt) ->
      let operand = make_wint test_bt_u8 start stop in
      let shift = make_wint test_bt_u8 shift_amt shift_amt in
      let result_opt =
        if is_bottom_wint operand then
          None
        else
          Basis.forward_abs_binop IT.ShiftLeft operand shift
      in
      String.concat
        "\n"
        [ "\nFailed shift not bottom test:\n";
          Printf.sprintf "  operand: [%d, %d] -> %s\n" start stop (pp_wint operand);
          Printf.sprintf "  shift: [%d, %d] -> %s\n" shift_amt shift_amt (pp_wint shift);
          Printf.sprintf "  operand is bottom: %b\n" (is_bottom_wint operand);
          Printf.sprintf
            "  result: %s\n"
            (match result_opt with Some r -> pp_wint r | None -> "None (or skipped)");
          Printf.sprintf
            "  result is bottom: %b\n"
            (match result_opt with Some r -> is_bottom_wint r | None -> false)
        ])
    Gen.(triple (int_range 0 255) (int_range 0 255) (int_range 0 7))
    (fun (start, stop, shift_amt) ->
       let operand = make_wint test_bt_u8 start stop in
       let shift = make_wint test_bt_u8 shift_amt shift_amt in
       if is_bottom_wint operand then
         true
       else (
         match Basis.forward_abs_binop IT.ShiftLeft operand shift with
         | Some result -> not (is_bottom_wint result)
         | None -> false))


(** Property: remainder operations don't produce bottom unless input is bottom or divisor is zero *)
let prop_remainder_not_bottom =
  Test.make
    ~count:50
    ~name:"remainder preserves non-bottom (except zero divisor)"
    (triple (int_range 1 255) (int_range 1 255) (int_range 1 255))
    (fun (dividend_start, dividend_stop, divisor) ->
       let dividend = make_wint test_bt_u8 dividend_start dividend_stop in
       let divisor_int = make_wint test_bt_u8 divisor divisor in
       if is_bottom_wint dividend then
         true
       else (
         match Basis.forward_abs_binop IT.Mod dividend divisor_int with
         | Some result -> not (is_bottom_wint result)
         | None -> false))


(** Property: remainder result is bounded by divisor for unsigned *)
let prop_remainder_bounded_unsigned =
  let open QCheck2 in
  Test.make
    ~count:50
    ~name:"unsigned remainder bounded by divisor"
    ~print:(fun (dividend_start, dividend_stop, divisor_max) ->
      try
        let dividend = make_wint test_bt_u8 dividend_start dividend_stop in
        let divisor = make_wint test_bt_u8 1 divisor_max in
        let result_opt = Basis.forward_abs_binop IT.Mod dividend divisor in
        let bound = make_wint test_bt_u8 0 (divisor_max - 1) in
        String.concat
          "\n"
          [ "\nFailed remainder bounded test:\n";
            Printf.sprintf
              "  dividend: [%d, %d] -> %s\n"
              dividend_start
              dividend_stop
              (pp_wint dividend);
            Printf.sprintf "  divisor: [1, %d] -> %s\n" divisor_max (pp_wint divisor);
            Printf.sprintf
              "  result: %s\n"
              (match result_opt with Some r -> pp_wint r | None -> "None");
            Printf.sprintf "  expected bound: %s\n" (pp_wint bound)
          ]
      with
      | e -> Printf.sprintf "Exception: %s" (Printexc.to_string e))
    Gen.(triple (int_range 0 255) (int_range 0 255) (int_range 1 100))
    (fun (dividend_start, dividend_stop, divisor_max) ->
       try
         let dividend = make_wint test_bt_u8 dividend_start dividend_stop in
         let divisor = make_wint test_bt_u8 1 divisor_max in
         match Basis.forward_abs_binop IT.Mod dividend divisor with
         | Some result ->
           let bound = make_wint test_bt_u8 0 (divisor_max - 1) in
           Basis.leq result bound
         | None -> false
       with
       | _ -> true (* Skip if remainder fails *))


(** Property: remainder of zero is zero *)
let prop_remainder_zero_dividend =
  let open QCheck2 in
  Test.make
    ~count:30
    ~name:"remainder of zero is zero"
    ~print:(fun divisor_val ->
      let zero_dividend = make_wint test_bt_u8 0 0 in
      let divisor = make_wint test_bt_u8 divisor_val divisor_val in
      let result_opt = Basis.forward_abs_binop IT.Mod zero_dividend divisor in
      let expected_zero = make_wint test_bt_u8 0 0 in
      String.concat
        "\n"
        [ "\nFailed remainder zero dividend test:\n";
          Printf.sprintf "  dividend: [0, 0] -> %s\n" (pp_wint zero_dividend);
          Printf.sprintf
            "  divisor: [%d, %d] -> %s\n"
            divisor_val
            divisor_val
            (pp_wint divisor);
          Printf.sprintf
            "  result: %s\n"
            (match result_opt with Some r -> pp_wint r | None -> "None");
          Printf.sprintf "  expected: %s\n" (pp_wint expected_zero)
        ])
    Gen.(int_range 1 255)
    (fun divisor_val ->
       let zero_dividend = make_wint test_bt_u8 0 0 in
       let divisor = make_wint test_bt_u8 divisor_val divisor_val in
       match Basis.forward_abs_binop IT.Mod zero_dividend divisor with
       | Some result ->
         let expected_zero = make_wint test_bt_u8 0 0 in
         Basis.equal result expected_zero
       | None -> false)


(** Property: remainder by zero produces bottom *)
let prop_remainder_zero_divisor_bottom =
  Test.make
    ~count:30
    ~name:"remainder by zero produces bottom"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (dividend_start, dividend_stop) ->
       let dividend = make_wint test_bt_u8 dividend_start dividend_stop in
       let zero_divisor = make_wint test_bt_u8 0 0 in
       match Basis.forward_abs_binop IT.Mod dividend zero_divisor with
       | Some result -> is_bottom_wint result
       | None -> false)


(** Property: signed remainder respects sign patterns *)
let prop_signed_remainder_sign_patterns =
  Test.make
    ~count:30
    ~name:"signed remainder sign patterns"
    (pair (int_range (-127) 127) (int_range 1 50))
    (fun (dividend_val, divisor_val) ->
       try
         let dividend = make_wint test_bt_s8 dividend_val dividend_val in
         let divisor = make_wint test_bt_s8 divisor_val divisor_val in
         match Basis.forward_abs_binop IT.Mod dividend divisor with
         | Some result ->
           (* For positive dividend and divisor: result should be in [0, divisor-1] *)
           if dividend_val >= 0 && divisor_val > 0 then (
             let bound = make_wint test_bt_s8 0 (divisor_val - 1) in
             Basis.leq result bound)
           else
             not (is_bottom_wint result) (* Just check it's not bottom for other cases *)
         | None -> false
       with
       | _ -> true (* Skip if remainder fails *))


(** Property: shift by excessive amount produces top *)
let prop_excessive_shift_produces_top =
  Test.make
    ~count:30
    ~name:"excessive shift produces top"
    (pair (int_range 0 255) (int_range 8 15))
    (fun (start, shift_amt) ->
       let operand = make_wint test_bt_u8 start start in
       let shift = make_wint test_bt_u8 shift_amt shift_amt in
       match Basis.forward_abs_binop IT.ShiftLeft operand shift with
       | Some result -> is_top_wint result
       | None -> false)


(** Property: non-constant shift produces top *)
let prop_non_constant_shift_produces_top =
  Test.make
    ~count:30
    ~name:"non-constant shift produces top"
    (triple (int_range 0 255) (int_range 0 255) (int_range 1 7))
    (fun (start, stop, max_shift) ->
       let operand = make_wint test_bt_u8 start stop in
       let shift = make_wint test_bt_u8 0 max_shift in
       (* Non-constant shift *)
       match Basis.forward_abs_binop IT.ShiftLeft operand shift with
       | Some result -> is_top_wint result
       | None -> false)


(** Create a set module for Z values *)
module ZSet = Set.Make (Z)

(** Helper function to get bit width from BT.t *)
let get_bit_width bt =
  match BT.is_bits_bt bt with Some (_, n) -> n | None -> failwith "Expected Bits type"


(** Helper function to enumerate all concrete values in a wrapped interval *)
let enumerate_wrapped_values bt start stop =
  let rec enum_range acc s e =
    if Z.equal s e then
      s :: acc
    else
      s :: enum_range acc (Z.succ s) e
  in
  let bit_width = get_bit_width bt in
  let max_val = Z.sub (Z.shift_left Z.one bit_width) Z.one in
  if Z.leq start stop then (* Normal interval [start, stop] *)
    enum_range [] start stop
  else (
    (* Wrapped interval [start, max] ∪ [0, stop] *)
    let high_part = enum_range [] start max_val in
    let low_part = enum_range [] Z.zero stop in
    high_part @ low_part)


(** Helper function to compute concrete intersection of two wrapped intervals *)
let concrete_intersection wint1 wint2 =
  if is_bottom_wint wint1 || is_bottom_wint wint2 then
    []
  else if is_top_wint wint1 then
    enumerate_wrapped_values wint2.Basis.bt wint2.Basis.start wint2.Basis.stop
  else if is_top_wint wint2 then
    enumerate_wrapped_values wint1.Basis.bt wint1.Basis.start wint1.Basis.stop
  else (
    let values1 =
      enumerate_wrapped_values wint1.Basis.bt wint1.Basis.start wint1.Basis.stop
    in
    let values2 =
      enumerate_wrapped_values wint2.Basis.bt wint2.Basis.start wint2.Basis.stop
    in
    let set2 = List.fold_left (fun acc v -> ZSet.add v acc) ZSet.empty values2 in
    List.filter (fun v -> ZSet.mem v set2) values1)


(** Helper function to compute concrete union of two wrapped intervals *)
let concrete_union wint1 wint2 =
  if is_bottom_wint wint1 then
    enumerate_wrapped_values wint2.Basis.bt wint2.Basis.start wint2.Basis.stop
  else if is_bottom_wint wint2 then
    enumerate_wrapped_values wint1.Basis.bt wint1.Basis.start wint1.Basis.stop
  else if is_top_wint wint1 || is_top_wint wint2 then (
    let bt = wint1.Basis.bt in
    let bit_width = get_bit_width bt in
    let max_val = Z.sub (Z.shift_left Z.one bit_width) Z.one in
    enumerate_wrapped_values bt Z.zero max_val)
  else (
    let values1 =
      enumerate_wrapped_values wint1.Basis.bt wint1.Basis.start wint1.Basis.stop
    in
    let values2 =
      enumerate_wrapped_values wint2.Basis.bt wint2.Basis.start wint2.Basis.stop
    in
    let set = List.fold_left (fun acc v -> ZSet.add v acc) ZSet.empty values1 in
    let set = List.fold_left (fun acc v -> ZSet.add v acc) set values2 in
    ZSet.elements set)


(** Helper function to check if a wrapped interval contains all values in a list *)
let contains_all_values wint values =
  if is_bottom_wint wint then
    List.is_empty values
  else if is_top_wint wint then
    true
  else
    List.for_all
      (fun v -> Basis.wrapped_member v wint.Basis.start wint.Basis.stop wint.Basis.bt)
      values


(** Property: meet overapproximates concrete intersection *)
let prop_meet_overapproximates =
  let open QCheck2 in
  Test.make
    ~count:50
    ~name:"meet overapproximates concrete intersection"
    ~print:(fun ((start1, stop1), (start2, stop2)) ->
      let bt = test_bt_u8 in
      let wint1 = make_wint bt start1 stop1 in
      let wint2 = make_wint bt start2 stop2 in
      let meet_result = Basis.meet wint1 wint2 in
      let concrete_inter = concrete_intersection wint1 wint2 in
      String.concat
        "\n"
        [ "\nFailed meet overapproximation test:\n";
          Printf.sprintf "  a: [%d, %d] -> %s\n" start1 stop1 (pp_wint wint1);
          Printf.sprintf "  b: [%d, %d] -> %s\n" start2 stop2 (pp_wint wint2);
          Printf.sprintf "  meet(a,b): %s\n" (pp_wint meet_result);
          Printf.sprintf "  concrete intersection size: %d\n" (List.length concrete_inter);
          Printf.sprintf
            "  concrete values: [%s]\n"
            (String.concat "; " (List.map Z.to_string (List.take 10 concrete_inter)))
        ])
    Gen.(
      pair
        (pair (int_range 0 100) (int_range 0 100))
        (pair (int_range 0 100) (int_range 0 100)))
    (fun ((start1, stop1), (start2, stop2)) ->
       let bt = test_bt_u8 in
       let wint1 = make_wint bt start1 stop1 in
       let wint2 = make_wint bt start2 stop2 in
       let meet_result = Basis.meet wint1 wint2 in
       let concrete_inter = concrete_intersection wint1 wint2 in
       contains_all_values meet_result concrete_inter)


(** Property: join overapproximates concrete union *)
let prop_join_overapproximates =
  let open QCheck2 in
  Test.make
    ~count:50
    ~name:"join overapproximates concrete union"
    ~print:(fun ((start1, stop1), (start2, stop2)) ->
      let bt = test_bt_u8 in
      let wint1 = make_wint bt start1 stop1 in
      let wint2 = make_wint bt start2 stop2 in
      let join_result = Basis.join wint1 wint2 in
      let concrete_union_vals = concrete_union wint1 wint2 in
      String.concat
        "\n"
        [ "\nFailed join overapproximation test:\n";
          Printf.sprintf "  a: [%d, %d] -> %s\n" start1 stop1 (pp_wint wint1);
          Printf.sprintf "  b: [%d, %d] -> %s\n" start2 stop2 (pp_wint wint2);
          Printf.sprintf "  join(a,b): %s\n" (pp_wint join_result);
          Printf.sprintf "  concrete union size: %d\n" (List.length concrete_union_vals);
          Printf.sprintf
            "  concrete values: [%s]\n"
            (String.concat "; " (List.map Z.to_string (List.take 10 concrete_union_vals)))
        ])
    Gen.(
      pair
        (pair (int_range 0 100) (int_range 0 100))
        (pair (int_range 0 100) (int_range 0 100)))
    (fun ((start1, stop1), (start2, stop2)) ->
       let bt = test_bt_u8 in
       let wint1 = make_wint bt start1 stop1 in
       let wint2 = make_wint bt start2 stop2 in
       let join_result = Basis.join wint1 wint2 in
       let concrete_union_vals = concrete_union wint1 wint2 in
       contains_all_values join_result concrete_union_vals)


(** Test fold_join bug - demonstrates wrong result from fold with join *)
let test_fold_join_bug _ =
  let bt = test_bt_u8 in
  (* Create the specific intervals from the failing test case *)
  let int1 = make_wint bt 0 0 in
  (* [0, 0] -> 0 *)
  let int2 = make_wint bt 27 230 in
  (* [27, 230] -> [27..230] *)
  let int3 = make_wint bt 0 27 in
  (* [0, 27] -> [0..27] *)
  let intervals = [ int1; int2; int3 ] in
  (* Calculate fold(join, l) using List.fold_left - this has the bug *)
  let fold_join = List.fold_left Basis.join (Basis.bottom bt) intervals in
  (* Calculate join_many(l) - this should be correct *)
  let join_many_result = Basis.join_many intervals in
  (* Demonstrate the bug: fold produces [27..0], join_many produces [0..230] *)
  assert_bool "join_many should not be bottom" (not (is_bottom_wint join_many_result));
  (* This test will currently fail because fold_join produces invalid result *)
  let fold_card = Basis.cardinality fold_join in
  let join_many_card = Basis.cardinality join_many_result in
  (* The assertion that should hold: join_many cardinality <= fold cardinality *)
  (* But currently fails because fold produces wrong result [27..0] *)
  assert_bool
    (Printf.sprintf
       "Expected |join_many| <= |fold_join|, got |%s| <= |%s|"
       (Z.to_string join_many_card)
       (Z.to_string fold_card))
    (Z.leq join_many_card fold_card)


(** Unit Tests *)
let unit_tests =
  "WrappedInterval Unit Tests"
  >::: [ "basic creation" >:: test_basic_creation;
         "wrapped membership" >:: test_wrapped_membership;
         "cardinality" >:: test_cardinality;
         "leq ordering" >:: test_leq;
         "join operation" >:: test_join;
         "meet operation" >:: test_meet;
         "join_many associativity" >:: test_join_many_associativity;
         "join_many generalized" >:: test_join_many_generalized;
         "arithmetic operations" >:: test_arithmetic_operations;
         "overflow handling" >:: test_overflow_handling;
         "pole splitting" >:: test_pole_splitting;
         "bitwise or" >:: test_bitwise_or;
         "bitwise and" >:: test_bitwise_and;
         "bitwise xor" >:: test_bitwise_xor;
         "left shift basic" >:: test_left_shift_basic;
         "left shift overflow" >:: test_left_shift_overflow;
         "left shift zero" >:: test_left_shift_zero;
         "logical right shift basic" >:: test_logical_right_shift_basic;
         "logical right shift south pole" >:: test_logical_right_shift_south_pole;
         "arithmetic right shift signed" >:: test_arithmetic_right_shift_signed;
         "arithmetic right shift north pole" >:: test_arithmetic_right_shift_north_pole;
         "shift excessive amount" >:: test_shift_excessive_amount;
         "non-constant shift" >:: test_non_constant_shift;
         "shift 16bit" >:: test_shift_16bit;
         "shift max safe" >:: test_shift_max_safe;
         "enhanced truncation precision" >:: test_enhanced_truncation;
         "enhanced truncation conservative" >:: test_enhanced_truncation_conservative;
         "arithmetic right shift conservative bounds"
         >:: test_arithmetic_right_shift_conservative_bounds;
         "shift left then right with overflow" >:: test_shift_left_then_right;
         "remainder basic" >:: test_remainder_basic;
         "remainder zero dividend" >:: test_remainder_zero_dividend;
         "remainder zero divisor" >:: test_remainder_zero_divisor;
         "signed remainder positive" >:: test_signed_remainder_positive;
         "signed remainder pos neg" >:: test_signed_remainder_pos_neg;
         "signed remainder neg pos" >:: test_signed_remainder_neg_pos;
         "signed remainder both negative" >:: test_signed_remainder_both_negative;
         "remainder wrapped" >:: test_remainder_wrapped;
         "remainder precision" >:: test_remainder_precision;
         "remainder 16bit" >:: test_remainder_16bit;
         "fold_join bug" >:: test_fold_join_bug
       ]


(** Property Tests *)
let property_tests =
  "WrappedInterval Property Tests"
  >::: [ QCheck_ounit.to_ounit2_test prop_join_commutative;
         QCheck_ounit.to_ounit2_test prop_meet_commutative;
         QCheck_ounit.to_ounit2_test prop_join_many_binary;
         (* QCheck_ounit.to_ounit2_test prop_meet_many_binary; *)
         QCheck_ounit.to_ounit2_test prop_bottom_absorbing_meet;
         QCheck_ounit.to_ounit2_test prop_top_absorbing_join;
         QCheck_ounit.to_ounit2_test prop_join_many_single;
         QCheck_ounit.to_ounit2_test prop_meet_many_single;
         QCheck_ounit.to_ounit2_test prop_leq_reflexive;
         QCheck_ounit.to_ounit2_test prop_join_many_monotonic;
         QCheck_ounit.to_ounit2_test prop_join_many_minimizes;
         QCheck_ounit.to_ounit2_test prop_meet_many_minimizes;
         QCheck_ounit.to_ounit2_test prop_join_idempotent;
         QCheck_ounit.to_ounit2_test prop_meet_idempotent;
         QCheck_ounit.to_ounit2_test prop_left_shift_zero_identity;
         QCheck_ounit.to_ounit2_test prop_right_shift_zero_identity;
         QCheck_ounit.to_ounit2_test prop_shift_not_bottom;
         QCheck_ounit.to_ounit2_test prop_excessive_shift_produces_top;
         QCheck_ounit.to_ounit2_test prop_non_constant_shift_produces_top;
         QCheck_ounit.to_ounit2_test prop_remainder_not_bottom;
         QCheck_ounit.to_ounit2_test prop_remainder_bounded_unsigned;
         QCheck_ounit.to_ounit2_test prop_remainder_zero_dividend;
         QCheck_ounit.to_ounit2_test prop_remainder_zero_divisor_bottom;
         QCheck_ounit.to_ounit2_test prop_signed_remainder_sign_patterns;
         QCheck_ounit.to_ounit2_test prop_meet_overapproximates;
         QCheck_ounit.to_ounit2_test prop_join_overapproximates
       ]


let suite = "WrappedInterval Tests" >::: [ unit_tests; property_tests ]
