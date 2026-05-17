(** Tests for the Tristate Number (tnum) abstract domain *)

open OUnit2
open QCheck
module BT = Cn.BaseTypes
module Sym = Cn.Sym
module IT = Cn.IndexTerms

module NonRelational =
  Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Private.NonRelational

module TristateBasis =
  Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Private.Tristate.TristateBasis

module TristateDomain = NonRelational.Make (TristateBasis)

(** Helper functions for creating test data *)
let test_loc = Cerb_location.unknown

let test_bt = BT.Bits (Unsigned, 8)

let test_bt_32 = BT.Bits (Unsigned, 32)

let test_bt_signed = BT.Bits (Signed, 8)

(** Create a symbolic variable *)
let make_sym name = Sym.fresh name

(** Create test constraints *)
let make_eq_constraint x value bt =
  IT.eq_ (IT.sym_ (x, bt, test_loc), IT.num_lit_ (Z.of_int value) bt test_loc) test_loc


let make_le_constraint x value bt =
  IT.le_ (IT.sym_ (x, bt, test_loc), IT.num_lit_ (Z.of_int value) bt test_loc) test_loc


let make_ge_constraint x value bt =
  IT.le_ (IT.num_lit_ (Z.of_int value) bt test_loc, IT.sym_ (x, bt, test_loc)) test_loc


let make_and it1 it2 = IT.and2_ (it1, it2) test_loc

(** Test bottom element *)
let test_bottom _ =
  let bottom = TristateBasis.bottom test_bt in
  assert_bool "bottom is_bottom" (TristateBasis.is_bottom bottom);
  assert_bool "bottom not is_top" (not (TristateBasis.is_top bottom))


(** Test top element *)
let test_top _ =
  let top = TristateBasis.top test_bt in
  assert_bool "top is_top" (TristateBasis.is_top top);
  assert_bool "top not is_bottom" (not (TristateBasis.is_bottom top))


(** Test constant creation *)
let test_const _ =
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  assert_bool "const is not bottom" (not (TristateBasis.is_bottom const_5));
  assert_bool "const is not top" (not (TristateBasis.is_top const_5));
  assert_equal ~msg:"value is 5" (Z.of_int 5) const_5.value;
  assert_equal ~msg:"mask is 0" Z.zero const_5.mask


(** Test of_interval *)
let test_of_interval _ =
  (* Single value interval *)
  let single = TristateBasis.of_interval test_bt (Z.of_int 5) (Z.of_int 5) in
  assert_equal ~msg:"single value" (Z.of_int 5) single.value;
  assert_equal ~msg:"single mask is 0" Z.zero single.mask;
  (* Range [4, 7] -> values are 100, 101, 110, 111 *)
  (* Common prefix is 1xx, so value=4 (100), mask=3 (011) *)
  let range = TristateBasis.of_interval test_bt (Z.of_int 4) (Z.of_int 7) in
  assert_equal ~msg:"range value" (Z.of_int 4) range.value;
  assert_equal ~msg:"range mask" (Z.of_int 3) range.mask;
  (* Empty range *)
  let empty = TristateBasis.of_interval test_bt (Z.of_int 7) (Z.of_int 4) in
  assert_bool "empty range is bottom" (TristateBasis.is_bottom empty)


(** Test of_tnum *)
let test_of_tnum _ =
  let t = TristateBasis.of_tnum test_bt (Z.of_int 5) (Z.of_int 2) in
  (* value=5 (101), mask=2 (010) -> normalized to value=5&~2=5 (101), mask=2 *)
  assert_equal ~msg:"tnum value" (Z.of_int 5) t.value;
  assert_equal ~msg:"tnum mask" (Z.of_int 2) t.mask


(** Test lattice ordering *)
let test_leq _ =
  let bottom = TristateBasis.bottom test_bt in
  let top = TristateBasis.top test_bt in
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  let partial = TristateBasis.of_tnum test_bt (Z.of_int 4) (Z.of_int 3) in
  (* Bottom is below everything *)
  assert_bool "bottom <= top" (TristateBasis.leq bottom top);
  assert_bool "bottom <= const" (TristateBasis.leq bottom const_5);
  assert_bool "bottom <= partial" (TristateBasis.leq bottom partial);
  (* Top is above everything except bottom *)
  assert_bool "const <= top" (TristateBasis.leq const_5 top);
  assert_bool "partial <= top" (TristateBasis.leq partial top);
  assert_bool "not top <= const" (not (TristateBasis.leq top const_5));
  (* Constants are ordered by refinement *)
  let const_7 = TristateBasis.of_const test_bt (Z.of_int 7) in
  assert_bool "const_5 <= partial (5 in 4..7)" (TristateBasis.leq const_5 partial);
  assert_bool "const_7 <= partial (7 in 4..7)" (TristateBasis.leq const_7 partial)


(** Test join operation *)
let test_join _ =
  let bottom = TristateBasis.bottom test_bt in
  let top = TristateBasis.top test_bt in
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  let const_6 = TristateBasis.of_const test_bt (Z.of_int 6) in
  (* Join with bottom *)
  let join_bottom = TristateBasis.join bottom const_5 in
  assert_equal ~msg:"join bottom value" (Z.of_int 5) join_bottom.value;
  assert_equal ~msg:"join bottom mask" Z.zero join_bottom.mask;
  (* Join with top *)
  let join_top = TristateBasis.join top const_5 in
  assert_bool "join with top is top" (TristateBasis.is_top join_top);
  (* Join of two constants with different values *)
  (* 5 = 101, 6 = 110 -> differ in all bits, so join should have mask covering those *)
  let join_5_6 = TristateBasis.join const_5 const_6 in
  (* Bits 0,1,2 all differ or have uncertainty *)
  assert_bool "join 5,6 not bottom" (not (TristateBasis.is_bottom join_5_6));
  assert_bool "join 5,6 not const" (not (Z.equal join_5_6.mask Z.zero))


(** Test meet operation *)
let test_meet _ =
  let bottom = TristateBasis.bottom test_bt in
  let top = TristateBasis.top test_bt in
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  let const_6 = TristateBasis.of_const test_bt (Z.of_int 6) in
  (* Meet with bottom *)
  let meet_bottom = TristateBasis.meet bottom const_5 in
  assert_bool "meet with bottom is bottom" (TristateBasis.is_bottom meet_bottom);
  (* Meet with top *)
  let meet_top = TristateBasis.meet top const_5 in
  assert_equal ~msg:"meet top value" const_5.value meet_top.value;
  assert_equal ~msg:"meet top mask" const_5.mask meet_top.mask;
  (* Meet of two conflicting constants *)
  let meet_conflict = TristateBasis.meet const_5 const_6 in
  assert_bool
    "meet of conflicting constants is bottom"
    (TristateBasis.is_bottom meet_conflict);
  (* Meet of compatible tnums *)
  let partial_4_mask_3 = TristateBasis.of_tnum test_bt (Z.of_int 4) (Z.of_int 3) in
  let meet_partial = TristateBasis.meet const_5 partial_4_mask_3 in
  assert_equal ~msg:"meet partial value" (Z.of_int 5) meet_partial.value;
  assert_equal ~msg:"meet partial mask" Z.zero meet_partial.mask


(** Test bitwise AND *)
let test_tnum_and _ =
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  (* 5 = 101 *)
  let const_3 = TristateBasis.of_const test_bt (Z.of_int 3) in
  (* 3 = 011 *)
  let result = TristateBasis.tnum_and const_5 const_3 in
  (* 5 & 3 = 001 = 1 *)
  assert_equal ~msg:"5 & 3 = 1" (Z.of_int 1) result.value;
  assert_equal ~msg:"mask is 0 for constants" Z.zero result.mask;
  (* AND with unknown bits *)
  let partial = TristateBasis.of_tnum test_bt Z.zero (Z.of_int 0xff) in
  let and_partial = TristateBasis.tnum_and const_5 partial in
  (* AND with all-unknown should preserve known 0s from const_5 *)
  assert_bool "and with unknown not top" (not (TristateBasis.is_top and_partial))


(** Test bitwise OR *)
let test_tnum_or _ =
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  (* 5 = 101 *)
  let const_3 = TristateBasis.of_const test_bt (Z.of_int 3) in
  (* 3 = 011 *)
  let result = TristateBasis.tnum_or const_5 const_3 in
  (* 5 | 3 = 111 = 7 *)
  assert_equal ~msg:"5 | 3 = 7" (Z.of_int 7) result.value;
  assert_equal ~msg:"mask is 0 for constants" Z.zero result.mask


(** Test bitwise XOR *)
let test_tnum_xor _ =
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  (* 5 = 101 *)
  let const_3 = TristateBasis.of_const test_bt (Z.of_int 3) in
  (* 3 = 011 *)
  let result = TristateBasis.tnum_xor const_5 const_3 in
  (* 5 ^ 3 = 110 = 6 *)
  assert_equal ~msg:"5 ^ 3 = 6" (Z.of_int 6) result.value;
  assert_equal ~msg:"mask is 0 for constants" Z.zero result.mask;
  (* XOR with unknown bits *)
  let partial = TristateBasis.of_tnum test_bt (Z.of_int 1) (Z.of_int 2) in
  let xor_partial = TristateBasis.tnum_xor const_5 partial in
  (* Unknown bits remain unknown *)
  assert_equal ~msg:"xor mask propagates" (Z.of_int 2) xor_partial.mask


(** Test bitwise NOT *)
let test_tnum_not _ =
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  (* 5 = 00000101 *)
  let result = TristateBasis.tnum_not const_5 in
  (* ~5 = 11111010 = 250 *)
  assert_equal ~msg:"~5 = 250" (Z.of_int 250) result.value;
  assert_equal ~msg:"mask is 0 for constants" Z.zero result.mask


(** Test addition *)
let test_tnum_add _ =
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  let const_3 = TristateBasis.of_const test_bt (Z.of_int 3) in
  let result = TristateBasis.tnum_add const_5 const_3 in
  assert_equal ~msg:"5 + 3 = 8" (Z.of_int 8) result.value;
  assert_equal ~msg:"mask is 0 for constants" Z.zero result.mask;
  (* Addition with unknown bits *)
  let partial = TristateBasis.of_tnum test_bt (Z.of_int 4) (Z.of_int 3) in
  (* 4..7 *)
  let add_partial = TristateBasis.tnum_add const_5 partial in
  (* 5 + (4..7) = 9..12 *)
  assert_bool "add partial not constant" (not (Z.equal add_partial.mask Z.zero))


(** Test subtraction *)
let test_tnum_sub _ =
  let const_8 = TristateBasis.of_const test_bt (Z.of_int 8) in
  let const_3 = TristateBasis.of_const test_bt (Z.of_int 3) in
  let result = TristateBasis.tnum_sub const_8 const_3 in
  assert_equal ~msg:"8 - 3 = 5" (Z.of_int 5) result.value;
  assert_equal ~msg:"mask is 0 for constants" Z.zero result.mask


(** Test left shift *)
let test_tnum_shl _ =
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  let shift_2 = TristateBasis.of_const test_bt (Z.of_int 2) in
  let result = TristateBasis.tnum_shl const_5 shift_2 in
  (* 5 << 2 = 20 *)
  assert_equal ~msg:"5 << 2 = 20" (Z.of_int 20) result.value;
  assert_equal ~msg:"mask is 0 for constants" Z.zero result.mask;
  (* Shift with unknown bits in value *)
  let partial = TristateBasis.of_tnum test_bt (Z.of_int 4) (Z.of_int 3) in
  let shl_partial = TristateBasis.tnum_shl partial shift_2 in
  (* Mask should be shifted too *)
  assert_equal ~msg:"shifted mask" (Z.of_int 12) shl_partial.mask;
  (* value = 4 << 2 = 16 *)
  assert_equal ~msg:"shifted value" (Z.of_int 16) shl_partial.value


(** Test right shift *)
let test_tnum_lshr _ =
  let const_20 = TristateBasis.of_const test_bt (Z.of_int 20) in
  let shift_2 = TristateBasis.of_const test_bt (Z.of_int 2) in
  let result = TristateBasis.tnum_lshr const_20 shift_2 in
  (* 20 >> 2 = 5 *)
  assert_equal ~msg:"20 >> 2 = 5" (Z.of_int 5) result.value;
  assert_equal ~msg:"mask is 0 for constants" Z.zero result.mask


(** Test multiplication *)
let test_tnum_mul _ =
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  let const_3 = TristateBasis.of_const test_bt (Z.of_int 3) in
  let result = TristateBasis.tnum_mul const_5 const_3 in
  assert_equal ~msg:"5 * 3 = 15" (Z.of_int 15) result.value;
  assert_equal ~msg:"mask is 0 for constants" Z.zero result.mask;
  (* Multiplication by 0 *)
  let const_0 = TristateBasis.of_const test_bt Z.zero in
  let mul_0 = TristateBasis.tnum_mul const_5 const_0 in
  assert_equal ~msg:"5 * 0 = 0" Z.zero mul_0.value;
  assert_equal ~msg:"5 * 0 mask is 0" Z.zero mul_0.mask


(** Test division *)
let test_tnum_div _ =
  let const_15 = TristateBasis.of_const test_bt (Z.of_int 15) in
  let const_3 = TristateBasis.of_const test_bt (Z.of_int 3) in
  let result = TristateBasis.tnum_div const_15 const_3 in
  assert_equal ~msg:"15 / 3 = 5" (Z.of_int 5) result.value;
  assert_equal ~msg:"mask is 0 for constants" Z.zero result.mask;
  (* Division by 0 *)
  let const_0 = TristateBasis.of_const test_bt Z.zero in
  let div_0 = TristateBasis.tnum_div const_15 const_0 in
  assert_bool "division by 0 is bottom" (TristateBasis.is_bottom div_0)


(** Test modulo *)
let test_tnum_mod _ =
  let const_17 = TristateBasis.of_const test_bt (Z.of_int 17) in
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  let result = TristateBasis.tnum_mod const_17 const_5 in
  assert_equal ~msg:"17 % 5 = 2" (Z.of_int 2) result.value;
  assert_equal ~msg:"mask is 0 for constants" Z.zero result.mask


(** Test cast - truncation *)
let test_cast_truncation _ =
  let const_257 = TristateBasis.of_const test_bt_32 (Z.of_int 257) in
  (* 257 = 0x101 *)
  let result = TristateBasis.cast const_257 test_bt in
  (* Truncate to 8 bits: 257 & 0xFF = 1 *)
  assert_equal ~msg:"257 truncated to 8 bits = 1" (Z.of_int 1) result.value;
  assert_equal ~msg:"mask is 0" Z.zero result.mask


(** Test cast - extension *)
let test_cast_extension _ =
  let const_5 = TristateBasis.of_const test_bt (Z.of_int 5) in
  let result = TristateBasis.cast const_5 test_bt_32 in
  (* Zero extension: 5 remains 5 *)
  assert_equal ~msg:"5 extended to 32 bits = 5" (Z.of_int 5) result.value;
  assert_equal ~msg:"mask is 0" Z.zero result.mask


(** Test backward refinement with equality *)
let test_backward_eq _ =
  let x = make_sym "x" in
  let eq_5 = make_eq_constraint x 5 test_bt in
  let result = TristateDomain.local_iteration eq_5 TristateDomain.top in
  match result with
  | None -> assert_failure "result should not be bottom"
  | Some domain_map ->
    (match Sym.Map.find_opt x domain_map with
     | None -> assert_failure "x should be in domain map"
     | Some tnum ->
       assert_equal ~msg:"refined to constant 5" (Z.of_int 5) tnum.value;
       assert_equal ~msg:"mask is 0" Z.zero tnum.mask)


(** Test backward refinement with bitwise AND: (x & 0x0F) == 5 *)
let test_backward_and _ =
  let x = make_sym "x" in
  (* (x & 0x0F) == 5 should refine x to have low 4 bits = 0101 *)
  let x_and_mask =
    IT.arith_binop
      IT.BW_And
      (IT.sym_ (x, test_bt, test_loc), IT.num_lit_ (Z.of_int 0x0F) test_bt test_loc)
      test_loc
  in
  let constraint_it =
    IT.eq_ (x_and_mask, IT.num_lit_ (Z.of_int 5) test_bt test_loc) test_loc
  in
  let result = TristateDomain.local_iteration constraint_it TristateDomain.top in
  match result with
  | None -> assert_failure "result should not be bottom"
  | Some domain_map ->
    (match Sym.Map.find_opt x domain_map with
     | None -> assert_failure "x should be in domain map"
     | Some tnum ->
       (* Low 4 bits should be 5 (0101), upper 4 bits unknown *)
       assert_equal ~msg:"value has low bits = 5" (Z.of_int 5) tnum.value;
       assert_equal ~msg:"mask has upper 4 bits unknown" (Z.of_int 0xF0) tnum.mask)


(** Test backward refinement with bitwise OR: (x | 0x0F) == 0x0F *)
let test_backward_or _ =
  let x = make_sym "x" in
  (* (x | 0x0F) == 0x0F means x's low 4 bits can be anything, but
     for the high 4 bits to remain 0 after OR, x must have high 4 bits = 0 *)
  let x_or_mask =
    IT.arith_binop
      IT.BW_Or
      (IT.sym_ (x, test_bt, test_loc), IT.num_lit_ (Z.of_int 0x0F) test_bt test_loc)
      test_loc
  in
  let constraint_it =
    IT.eq_ (x_or_mask, IT.num_lit_ (Z.of_int 0x0F) test_bt test_loc) test_loc
  in
  let result = TristateDomain.local_iteration constraint_it TristateDomain.top in
  match result with
  | None -> assert_failure "result should not be bottom"
  | Some domain_map ->
    (match Sym.Map.find_opt x domain_map with
     | None -> assert_failure "x should be in domain map"
     | Some tnum ->
       (* High 4 bits must be 0, low 4 bits unknown *)
       assert_equal ~msg:"value has high bits = 0" Z.zero tnum.value;
       assert_equal ~msg:"mask has low 4 bits unknown" (Z.of_int 0x0F) tnum.mask)


(** Test backward refinement with bitwise XOR: (x ^ 0x0F) == 0x0A *)
let test_backward_xor _ =
  let x = make_sym "x" in
  (* (x ^ 0x0F) == 0x0A means x = 0x0A ^ 0x0F = 0x05 *)
  let x_xor_mask =
    IT.arith_binop
      IT.BW_Xor
      (IT.sym_ (x, test_bt, test_loc), IT.num_lit_ (Z.of_int 0x0F) test_bt test_loc)
      test_loc
  in
  let constraint_it =
    IT.eq_ (x_xor_mask, IT.num_lit_ (Z.of_int 0x0A) test_bt test_loc) test_loc
  in
  let result = TristateDomain.local_iteration constraint_it TristateDomain.top in
  match result with
  | None -> assert_failure "result should not be bottom"
  | Some domain_map ->
    (match Sym.Map.find_opt x domain_map with
     | None -> assert_failure "x should be in domain map"
     | Some tnum ->
       (* x should be 0x05 in low 4 bits, high 4 bits constrained to 0 by result *)
       assert_equal ~msg:"value is 0x05" (Z.of_int 0x05) tnum.value;
       assert_equal ~msg:"mask is 0 (fully refined)" Z.zero tnum.mask)


(** Test backward refinement with left shift: (x << 2) == 20 *)
let test_backward_shl _ =
  let x = make_sym "x" in
  (* (x << 2) == 20 means x = 20 >> 2 = 5 *)
  let x_shl =
    IT.arith_binop
      IT.ShiftLeft
      (IT.sym_ (x, test_bt, test_loc), IT.num_lit_ (Z.of_int 2) test_bt test_loc)
      test_loc
  in
  let constraint_it =
    IT.eq_ (x_shl, IT.num_lit_ (Z.of_int 20) test_bt test_loc) test_loc
  in
  let result = TristateDomain.local_iteration constraint_it TristateDomain.top in
  match result with
  | None -> assert_failure "result should not be bottom"
  | Some domain_map ->
    (match Sym.Map.find_opt x domain_map with
     | None -> assert_failure "x should be in domain map"
     | Some tnum ->
       (* x should be refined to 5 *)
       assert_equal ~msg:"value is 5" (Z.of_int 5) tnum.value;
       assert_equal ~msg:"mask is 0" Z.zero tnum.mask)


(** Test backward refinement with right shift: (x >> 2) == 5 *)
let test_backward_lshr _ =
  let x = make_sym "x" in
  (* (x >> 2) == 5 means x = 5 << 2 = 20, but low 2 bits are unknown *)
  let x_lshr =
    IT.arith_binop
      IT.ShiftRight
      (IT.sym_ (x, test_bt, test_loc), IT.num_lit_ (Z.of_int 2) test_bt test_loc)
      test_loc
  in
  let constraint_it =
    IT.eq_ (x_lshr, IT.num_lit_ (Z.of_int 5) test_bt test_loc) test_loc
  in
  let result = TristateDomain.local_iteration constraint_it TristateDomain.top in
  match result with
  | None -> assert_failure "result should not be bottom"
  | Some domain_map ->
    (match Sym.Map.find_opt x domain_map with
     | None -> assert_failure "x should be in domain map"
     | Some tnum ->
       (* x should be 20 (5 << 2), with low 2 bits unknown *)
       assert_equal ~msg:"value is 20" (Z.of_int 20) tnum.value;
       assert_equal ~msg:"mask has low 2 bits unknown" (Z.of_int 3) tnum.mask)


(** Test backward refinement with less-than-or-equal: x <= 10 *)
let test_backward_le _ =
  let x = make_sym "x" in
  let le_10 = make_le_constraint x 10 test_bt in
  let result = TristateDomain.local_iteration le_10 TristateDomain.top in
  match result with
  | None -> assert_failure "result should not be bottom"
  | Some domain_map ->
    (match Sym.Map.find_opt x domain_map with
     | None -> assert_failure "x should be in domain map"
     | Some tnum ->
       (* x <= 10 means x in [0, 10], which has value 0 and mask 15 (0..15 covers it) *)
       assert_bool "tnum is not bottom" (not (TristateBasis.is_bottom tnum));
       (* The mask should have enough bits to cover 0-10 *)
       assert_bool "mask covers range" (Z.geq tnum.mask (Z.of_int 7)))


(** Test backward refinement with NOT: ~x == 250 (for 8-bit) *)
let test_backward_not _ =
  let x = make_sym "x" in
  (* ~x == 250 means x = ~250 = 5 (for 8-bit unsigned) *)
  let not_x = IT.arith_unop IT.BW_Compl (IT.sym_ (x, test_bt, test_loc)) test_loc in
  let constraint_it =
    IT.eq_ (not_x, IT.num_lit_ (Z.of_int 250) test_bt test_loc) test_loc
  in
  let result = TristateDomain.local_iteration constraint_it TristateDomain.top in
  match result with
  | None -> assert_failure "result should not be bottom"
  | Some domain_map ->
    (match Sym.Map.find_opt x domain_map with
     | None -> assert_failure "x should be in domain map"
     | Some tnum ->
       (* x should be refined to 5 *)
       assert_equal ~msg:"value is 5" (Z.of_int 5) tnum.value;
       assert_equal ~msg:"mask is 0" Z.zero tnum.mask)


(** Test backward refinement with not-equal: x != 5 when x is {4, 5} *)
let test_backward_ne _ =
  let x = make_sym "x" in
  (* Create a constraint that x != 5 *)
  let x_it = IT.sym_ (x, test_bt, test_loc) in
  let ne_constraint =
    IT.not_ (IT.eq_ (x_it, IT.num_lit_ (Z.of_int 5) test_bt test_loc) test_loc) test_loc
  in
  (* Start with x in {4, 5} (value=4, mask=1) *)
  let initial_tnum = TristateBasis.of_tnum test_bt (Z.of_int 4) (Z.of_int 1) in
  let initial_map = Sym.Map.singleton x initial_tnum in
  let result = TristateDomain.local_iteration ne_constraint (Some initial_map) in
  match result with
  | None -> assert_failure "result should not be bottom"
  | Some domain_map ->
    (match Sym.Map.find_opt x domain_map with
     | None -> assert_failure "x should be in domain map"
     | Some tnum ->
       (* x was {4, 5}, after x != 5, x should be exactly 4 *)
       assert_equal ~msg:"value is 4" (Z.of_int 4) tnum.value;
       assert_equal ~msg:"mask is 0 (refined to constant)" Z.zero tnum.mask)


(** Test backward refinement with not-equal: contradiction case *)
let test_backward_ne_contradiction _ =
  let x = make_sym "x" in
  (* Create a constraint that x != 5 *)
  let x_it = IT.sym_ (x, test_bt, test_loc) in
  let ne_constraint =
    IT.not_ (IT.eq_ (x_it, IT.num_lit_ (Z.of_int 5) test_bt test_loc) test_loc) test_loc
  in
  (* Start with x = 5 exactly *)
  let initial_tnum = TristateBasis.of_const test_bt (Z.of_int 5) in
  let initial_map = Sym.Map.singleton x initial_tnum in
  let result = TristateDomain.local_iteration ne_constraint (Some initial_map) in
  (* x = 5 and x != 5 is a contradiction, should be bottom *)
  assert_bool "contradiction should yield bottom" (Option.is_none result)


(** Property tests *)

(** Property: join is commutative *)
let join_commutative_prop =
  Test.make
    ~count:100
    ~name:"join is commutative"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (a, b) ->
       let t1 = TristateBasis.of_const test_bt (Z.of_int a) in
       let t2 = TristateBasis.of_const test_bt (Z.of_int b) in
       TristateBasis.equal (TristateBasis.join t1 t2) (TristateBasis.join t2 t1))


(** Property: meet is commutative *)
let meet_commutative_prop =
  Test.make
    ~count:100
    ~name:"meet is commutative"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (a, b) ->
       let t1 = TristateBasis.of_const test_bt (Z.of_int a) in
       let t2 = TristateBasis.of_const test_bt (Z.of_int b) in
       TristateBasis.equal (TristateBasis.meet t1 t2) (TristateBasis.meet t2 t1))


(** Property: AND is sound *)
let and_soundness_prop =
  Test.make
    ~count:100
    ~name:"AND is sound"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (a, b) ->
       let t1 = TristateBasis.of_const test_bt (Z.of_int a) in
       let t2 = TristateBasis.of_const test_bt (Z.of_int b) in
       let result = TristateBasis.tnum_and t1 t2 in
       let expected = a land b in
       Z.equal result.value (Z.of_int expected) && Z.equal result.mask Z.zero)


(** Property: OR is sound *)
let or_soundness_prop =
  Test.make
    ~count:100
    ~name:"OR is sound"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (a, b) ->
       let t1 = TristateBasis.of_const test_bt (Z.of_int a) in
       let t2 = TristateBasis.of_const test_bt (Z.of_int b) in
       let result = TristateBasis.tnum_or t1 t2 in
       let expected = a lor b in
       Z.equal result.value (Z.of_int expected) && Z.equal result.mask Z.zero)


(** Property: XOR is sound *)
let xor_soundness_prop =
  Test.make
    ~count:100
    ~name:"XOR is sound"
    (pair (int_range 0 255) (int_range 0 255))
    (fun (a, b) ->
       let t1 = TristateBasis.of_const test_bt (Z.of_int a) in
       let t2 = TristateBasis.of_const test_bt (Z.of_int b) in
       let result = TristateBasis.tnum_xor t1 t2 in
       let expected = a lxor b in
       Z.equal result.value (Z.of_int expected) && Z.equal result.mask Z.zero)


(** Property: ADD is sound for constants *)
let add_soundness_prop =
  Test.make
    ~count:100
    ~name:"ADD is sound"
    (pair (int_range 0 127) (int_range 0 127))
    (fun (a, b) ->
       let t1 = TristateBasis.of_const test_bt (Z.of_int a) in
       let t2 = TristateBasis.of_const test_bt (Z.of_int b) in
       let result = TristateBasis.tnum_add t1 t2 in
       let expected = (a + b) land 255 in
       Z.equal result.value (Z.of_int expected) && Z.equal result.mask Z.zero)


(** Property: leq is reflexive *)
let leq_reflexive_prop =
  Test.make ~count:100 ~name:"leq is reflexive" (int_range 0 255) (fun a ->
    let t = TristateBasis.of_const test_bt (Z.of_int a) in
    TristateBasis.leq t t)


(** Property: bottom is below everything *)
let bottom_minimal_prop =
  Test.make ~count:100 ~name:"bottom is minimal" (int_range 0 255) (fun a ->
    let t = TristateBasis.of_const test_bt (Z.of_int a) in
    let bottom = TristateBasis.bottom test_bt in
    TristateBasis.leq bottom t)


(** Property: top is above everything *)
let top_maximal_prop =
  Test.make ~count:100 ~name:"top is maximal" (int_range 0 255) (fun a ->
    let t = TristateBasis.of_const test_bt (Z.of_int a) in
    let top = TristateBasis.top test_bt in
    TristateBasis.leq t top)


(** Unit Tests *)
let lattice_tests =
  "Tristate Lattice Operations"
  >::: [ "bottom" >:: test_bottom;
         "top" >:: test_top;
         "const" >:: test_const;
         "of_interval" >:: test_of_interval;
         "of_tnum" >:: test_of_tnum;
         "leq" >:: test_leq;
         "join" >:: test_join;
         "meet" >:: test_meet
       ]


let bitwise_tests =
  "Tristate Bitwise Operations"
  >::: [ "tnum_and" >:: test_tnum_and;
         "tnum_or" >:: test_tnum_or;
         "tnum_xor" >:: test_tnum_xor;
         "tnum_not" >:: test_tnum_not
       ]


let arithmetic_tests =
  "Tristate Arithmetic Operations"
  >::: [ "tnum_add" >:: test_tnum_add;
         "tnum_sub" >:: test_tnum_sub;
         "tnum_shl" >:: test_tnum_shl;
         "tnum_lshr" >:: test_tnum_lshr;
         "tnum_mul" >:: test_tnum_mul;
         "tnum_div" >:: test_tnum_div;
         "tnum_mod" >:: test_tnum_mod
       ]


let cast_tests =
  "Tristate Cast Operations"
  >::: [ "cast truncation" >:: test_cast_truncation;
         "cast extension" >:: test_cast_extension
       ]


let backward_tests =
  "Tristate Backward Operations"
  >::: [ "backward eq" >:: test_backward_eq;
         "backward and" >:: test_backward_and;
         "backward or" >:: test_backward_or;
         "backward xor" >:: test_backward_xor;
         "backward shl" >:: test_backward_shl;
         "backward lshr" >:: test_backward_lshr;
         "backward le" >:: test_backward_le;
         "backward not" >:: test_backward_not;
         "backward ne" >:: test_backward_ne;
         "backward ne contradiction" >:: test_backward_ne_contradiction
       ]


let property_tests =
  "Tristate Property Tests"
  >::: [ QCheck_ounit.to_ounit2_test join_commutative_prop;
         QCheck_ounit.to_ounit2_test meet_commutative_prop;
         QCheck_ounit.to_ounit2_test and_soundness_prop;
         QCheck_ounit.to_ounit2_test or_soundness_prop;
         QCheck_ounit.to_ounit2_test xor_soundness_prop;
         QCheck_ounit.to_ounit2_test add_soundness_prop;
         QCheck_ounit.to_ounit2_test leq_reflexive_prop;
         QCheck_ounit.to_ounit2_test bottom_minimal_prop;
         QCheck_ounit.to_ounit2_test top_maximal_prop
       ]


let suite =
  "Tristate Number (tnum) Tests"
  >::: [ lattice_tests;
         bitwise_tests;
         arithmetic_tests;
         cast_tests;
         backward_tests;
         property_tests
       ]
