(** Tests for the Congruence abstract domain *)

open OUnit2
module BT = Cn.BaseTypes
module Sym = Cn.Sym
module MT = Cn.MakeTerm
module LC = Cn.LogicalConstraints
module Memory = Cn.Memory

module NonRelational =
  Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Private.NonRelational

module CongruenceBasis =
  Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Private.Congruence
  .CongruenceBasis

module CongruenceDomain = NonRelational.Make (CongruenceBasis)

(** Helper functions *)
let test_loc = Cerb_location.unknown

let test_bt = BT.Bits (Unsigned, 8)

let test_bt_16 = BT.Bits (Unsigned, 16)

let test_bt_32 = BT.Bits (Unsigned, 32)

let test_bt_signed = BT.Bits (Signed, 8)

let make_sym name = Sym.fresh name

let make_eq_constraint x value bt =
  MT.eq_ (MT.sym_ (x, bt, test_loc), MT.num_lit_ (Z.of_int value) bt test_loc) test_loc


let make_and it1 it2 = MT.and2_ (it1, it2) test_loc

(* ---- Lattice tests ---- *)

let test_bottom _ =
  let bottom = CongruenceBasis.bottom test_bt in
  assert_bool "bottom is_bottom" (CongruenceBasis.is_bottom bottom);
  assert_bool "bottom not is_top" (not (CongruenceBasis.is_top bottom))


let test_top _ =
  let top = CongruenceBasis.top test_bt in
  assert_bool "top is_top" (CongruenceBasis.is_top top);
  assert_bool "top not is_bottom" (not (CongruenceBasis.is_bottom top));
  assert_equal ~msg:"top modulus" Z.one top.modulus;
  assert_equal ~msg:"top residue" Z.zero top.residue


let test_const _ =
  let const_5 = CongruenceBasis.of_const test_bt (Z.of_int 5) in
  assert_bool "const is not bottom" (not (CongruenceBasis.is_bottom const_5));
  assert_bool "const is not top" (not (CongruenceBasis.is_top const_5));
  assert_equal ~msg:"modulus is 0" Z.zero const_5.modulus;
  assert_equal ~msg:"residue is 5" (Z.of_int 5) const_5.residue


let test_const_wrapping _ =
  (* 300 mod 256 = 44 for uint8 *)
  let const_300 = CongruenceBasis.of_const test_bt (Z.of_int 300) in
  assert_equal ~msg:"residue wraps" (Z.of_int 44) const_300.residue


let test_of_interval _ =
  (* Single value *)
  let single = CongruenceBasis.of_interval test_bt (Z.of_int 5) (Z.of_int 5) in
  assert_equal ~msg:"single modulus" Z.zero single.modulus;
  assert_equal ~msg:"single residue" (Z.of_int 5) single.residue;
  (* Range -> top *)
  let range = CongruenceBasis.of_interval test_bt (Z.of_int 4) (Z.of_int 7) in
  assert_bool "range is top" (CongruenceBasis.is_top range);
  (* Empty range -> bottom *)
  let empty = CongruenceBasis.of_interval test_bt (Z.of_int 7) (Z.of_int 4) in
  assert_bool "empty is bottom" (CongruenceBasis.is_bottom empty)


let test_to_interval _ =
  (* Singleton *)
  let c5 = CongruenceBasis.of_const test_bt (Z.of_int 5) in
  (match CongruenceBasis.to_interval c5 with
   | Some (lo, hi) ->
     assert_equal ~msg:"singleton lo" (Z.of_int 5) lo;
     assert_equal ~msg:"singleton hi" (Z.of_int 5) hi
   | None -> assert_failure "singleton should have interval");
  (* Top -> None *)
  let top = CongruenceBasis.top test_bt in
  assert_equal ~msg:"top has no interval" None (CongruenceBasis.to_interval top);
  (* 4Z+1 in [0,255]: min=1, max=1+4*63=253 *)
  let t = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 1) in
  match CongruenceBasis.to_interval t with
  | Some (lo, hi) ->
    assert_equal ~msg:"4Z+1 lo" (Z.of_int 1) lo;
    assert_equal ~msg:"4Z+1 hi" (Z.of_int 253) hi
  | None -> assert_failure "4Z+1 should have interval"


(* ---- Ordering tests ---- *)

let test_leq _ =
  let bottom = CongruenceBasis.bottom test_bt in
  let top = CongruenceBasis.top test_bt in
  let c5 = CongruenceBasis.of_const test_bt (Z.of_int 5) in
  let c3 = CongruenceBasis.of_const test_bt (Z.of_int 3) in
  let mod4_r1 = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 1) in
  let mod2_r1 = CongruenceBasis.mk test_bt (Z.of_int 2) (Z.of_int 1) in
  (* Bottom is below everything *)
  assert_bool "bottom <= top" (CongruenceBasis.leq bottom top);
  assert_bool "bottom <= const" (CongruenceBasis.leq bottom c5);
  (* Everything is below top *)
  assert_bool "const <= top" (CongruenceBasis.leq c5 top);
  assert_bool "mod4 <= top" (CongruenceBasis.leq mod4_r1 top);
  (* Top is not below non-top *)
  assert_bool "top </= const" (not (CongruenceBasis.leq top c5));
  (* Singleton: 5 ≡ 1 (mod 4), so of_const 5 <= 4Z+1 *)
  assert_bool "5 <= 4Z+1" (CongruenceBasis.leq c5 mod4_r1);
  (* 3 ≢ 1 (mod 4) *)
  assert_bool "3 </= 4Z+1" (not (CongruenceBasis.leq c3 mod4_r1));
  (* 4Z+1 ⊆ 2Z+1 since 2|4 and 1≡1(mod 2) *)
  assert_bool "4Z+1 <= 2Z+1" (CongruenceBasis.leq mod4_r1 mod2_r1);
  (* But not the other way *)
  assert_bool "2Z+1 </= 4Z+1" (not (CongruenceBasis.leq mod2_r1 mod4_r1))


(* ---- Join tests ---- *)

let test_join _ =
  let bottom = CongruenceBasis.bottom test_bt in
  let c5 = CongruenceBasis.of_const test_bt (Z.of_int 5) in
  let c9 = CongruenceBasis.of_const test_bt (Z.of_int 9) in
  (* Join with bottom *)
  let j1 = CongruenceBasis.join bottom c5 in
  assert_bool "join(bottom, 5) = 5" (CongruenceBasis.equal j1 c5);
  (* Join of two singletons: gcd(0, 0, |5-9|) = 4, residue = 5 mod 4 = 1 *)
  let j2 = CongruenceBasis.join c5 c9 in
  assert_equal ~msg:"join(5,9) modulus" (Z.of_int 4) j2.modulus;
  assert_equal ~msg:"join(5,9) residue" (Z.of_int 1) j2.residue


let test_join_same _ =
  let c5 = CongruenceBasis.of_const test_bt (Z.of_int 5) in
  let j = CongruenceBasis.join c5 c5 in
  assert_bool "join(5,5) = 5" (CongruenceBasis.equal j c5)


let test_join_with_stride _ =
  (* 4Z+1 ⊔ 4Z+3 = gcd(4, 4, |1-3|) Z + (1 mod gcd(4,4,2)) = 2Z+1 *)
  let t1 = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 1) in
  let t2 = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 3) in
  let j = CongruenceBasis.join t1 t2 in
  assert_equal ~msg:"join modulus" (Z.of_int 2) j.modulus;
  assert_equal ~msg:"join residue" (Z.of_int 1) j.residue


(* ---- Meet tests ---- *)

let test_meet _ =
  let top = CongruenceBasis.top test_bt in
  let c5 = CongruenceBasis.of_const test_bt (Z.of_int 5) in
  (* Meet with top *)
  let m1 = CongruenceBasis.meet top c5 in
  assert_bool "meet(top, 5) = 5" (CongruenceBasis.equal m1 c5);
  (* Meet of incompatible singletons *)
  let c3 = CongruenceBasis.of_const test_bt (Z.of_int 3) in
  let m2 = CongruenceBasis.meet c5 c3 in
  assert_bool "meet(5, 3) = bottom" (CongruenceBasis.is_bottom m2)


let test_meet_crt _ =
  (* 2Z+1 ⊓ 3Z+1 via CRT: lcm(2,3)=6, r=1, so 6Z+1.
     But for uint8: gcd(6, 256) = 2, so ξ-normalize gives 2Z+1.
     Wait, 6 divides 256? gcd(6, 256) = 2. So modulus = 2, residue = 1 mod 2 = 1.
     Hmm, that's not right. Let me reconsider.
     Actually for uint8, 2^w = 256. gcd(6, 256) = 2.
     So after ξ-normalization: modulus = 2, residue = 1 mod 2 = 1.
     That means the meet of 2Z+1 and 3Z+1 in uint8 gives 2Z+1?
     That doesn't seem right. Let me think again...
     Actually 3Z+1 in uint8 after ξ gives gcd(3, 256) = 1, residue = 1 mod 1 = 0.
     So 3Z+1 normalizes to 1Z+0 = top in uint8!
     Let me use a wider type or different example. *)
  (* Use 32-bit for meaningful CRT *)
  let bt = test_bt_32 in
  (* For 32-bit: 2^32 = 4294967296
     2Z+0 ⊓ 3Z+0: gcd(2,3) = 1, (0-0) mod 1 = 0, lcm = 6
     gcd(6, 2^32) = 2 (since 6 = 2*3 and 2^32 has factor 2)
     Wait: gcd(6, 4294967296). 4294967296 = 2^32.
     6 = 2 * 3. gcd(6, 2^32) = 2. So modulus = 2, residue = 0.
     That's 2Z+0, which is the same as input t1. Not a useful CRT test.

     Better: 4Z+1 ⊓ 8Z+5: gcd(4,8) = 4. (5-1)=4, 4 mod 4 = 0, so compatible.
     lcm(4,8) = 8. CRT gives 8Z+5.
     gcd(8, 2^32) = 8. ξ gives 8Z + (5 mod 8) = 8Z+5. *)
  let t1 = CongruenceBasis.mk bt (Z.of_int 4) (Z.of_int 1) in
  let t2 = CongruenceBasis.mk bt (Z.of_int 8) (Z.of_int 5) in
  let m = CongruenceBasis.meet t1 t2 in
  assert_bool "meet not bottom" (not (CongruenceBasis.is_bottom m));
  assert_equal ~msg:"meet modulus" (Z.of_int 8) m.modulus;
  assert_equal ~msg:"meet residue" (Z.of_int 5) m.residue


let test_meet_incompatible _ =
  (* 4Z+1 ⊓ 4Z+2: gcd(4,4)=4, (2-1)=1, 1 mod 4 ≠ 0 → bottom *)
  let t1 = CongruenceBasis.mk test_bt_32 (Z.of_int 4) (Z.of_int 1) in
  let t2 = CongruenceBasis.mk test_bt_32 (Z.of_int 4) (Z.of_int 2) in
  let m = CongruenceBasis.meet t1 t2 in
  assert_bool "incompatible meet is bottom" (CongruenceBasis.is_bottom m)


(* ---- Forward arithmetic tests ---- *)

let test_add _ =
  (* 4Z+1 + 4Z+2 = gcd(4,4,256)Z + ((1+2) mod 256) = 4Z+3 *)
  let t1 = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 1) in
  let t2 = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 2) in
  let r = CongruenceBasis.congr_add t1 t2 in
  assert_equal ~msg:"add modulus" (Z.of_int 4) r.modulus;
  assert_equal ~msg:"add residue" (Z.of_int 3) r.residue


let test_sub _ =
  (* 8Z+5 - 4Z+2 = gcd(8,4,256)Z + ((5-2) mod 256) = 4Z+3 *)
  let t1 = CongruenceBasis.mk test_bt (Z.of_int 8) (Z.of_int 5) in
  let t2 = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 2) in
  let r = CongruenceBasis.congr_sub t1 t2 in
  assert_equal ~msg:"sub modulus" (Z.of_int 4) r.modulus;
  assert_equal ~msg:"sub residue" (Z.of_int 3) r.residue


let test_neg _ =
  (* -(4Z+3) in uint8 = 4Z + ((256-3) mod 256) = 4Z+253.
     ξ: gcd(4,256) = 4, 253 mod 4 = 1. So 4Z+1. *)
  let t = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 3) in
  let r = CongruenceBasis.congr_neg t in
  assert_equal ~msg:"neg modulus" (Z.of_int 4) r.modulus;
  assert_equal ~msg:"neg residue" (Z.of_int 1) r.residue


let test_mul _ =
  (* 4Z+3 * 0Z+2: a=4,b=3,c=0,d=2. ac=0,ad=8,bc=0. gcd(0,8,0,256)=8. bd=6. → 8Z+6 *)
  let t1 = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 3) in
  let t2 = CongruenceBasis.of_const test_bt (Z.of_int 2) in
  let r = CongruenceBasis.congr_mul t1 t2 in
  assert_equal ~msg:"mul modulus" (Z.of_int 8) r.modulus;
  assert_equal ~msg:"mul residue" (Z.of_int 6) r.residue


let test_mul_constants _ =
  let c3 = CongruenceBasis.of_const test_bt (Z.of_int 3) in
  let c5 = CongruenceBasis.of_const test_bt (Z.of_int 5) in
  let r = CongruenceBasis.congr_mul c3 c5 in
  assert_equal ~msg:"3*5 modulus" Z.zero r.modulus;
  assert_equal ~msg:"3*5 residue" (Z.of_int 15) r.residue


let test_div _ =
  (* 8Z+6 / 0Z+2: 2|8 and 2|6, so (8/2)Z + (6/2) = 4Z+3 *)
  let t1 = CongruenceBasis.mk test_bt (Z.of_int 8) (Z.of_int 6) in
  let t2 = CongruenceBasis.of_const test_bt (Z.of_int 2) in
  let r = CongruenceBasis.congr_div t1 t2 in
  assert_equal ~msg:"div modulus" (Z.of_int 4) r.modulus;
  assert_equal ~msg:"div residue" (Z.of_int 3) r.residue


let test_div_inexact _ =
  (* 4Z+3 / 0Z+2: 2 does not divide 3, so top *)
  let t1 = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 3) in
  let t2 = CongruenceBasis.of_const test_bt (Z.of_int 2) in
  let r = CongruenceBasis.congr_div t1 t2 in
  assert_bool "inexact div is top" (CongruenceBasis.is_top r)


let test_mod _ =
  (* 12Z+7 mod 0Z+4: gcd(12, 4) = 4, residue = 7 mod 4 = 3, so 4Z+3.
     ξ for uint8: gcd(4, 256) = 4, so 4Z+3. *)
  let t1 = CongruenceBasis.mk test_bt (Z.of_int 12) (Z.of_int 7) in
  let t2 = CongruenceBasis.of_const test_bt (Z.of_int 4) in
  let r = CongruenceBasis.congr_mod t1 t2 in
  (* gcd(12,4) = 4, 7 mod 4 = 3 *)
  (* But wait: ξ-norm of 12Z+7 for uint8: gcd(12,256)=4, 7 mod 4 = 3. So t1 becomes 4Z+3.
     Then 4Z+3 mod 4: gcd(4,4)=4, 3 mod 4 = 3. Result: 4Z+3. *)
  assert_equal ~msg:"mod modulus" (Z.of_int 4) r.modulus;
  assert_equal ~msg:"mod residue" (Z.of_int 3) r.residue


(* ---- Bitwise tests ---- *)

let test_and _ =
  (* 8Z+5 & 4Z+3: k1=trailing_zeros(8)=3, k2=trailing_zeros(4)=2, k=2
     mask = 2^2-1 = 3; r = (5 & 3) & 3 = 1 & 3 = 1. Result: 4Z+1 *)
  let t1 = CongruenceBasis.mk test_bt (Z.of_int 8) (Z.of_int 5) in
  let t2 = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 3) in
  let r = CongruenceBasis.congr_and t1 t2 in
  assert_equal ~msg:"and modulus" (Z.of_int 4) r.modulus;
  assert_equal ~msg:"and residue" (Z.of_int 1) r.residue


let test_or _ =
  let t1 = CongruenceBasis.mk test_bt (Z.of_int 8) (Z.of_int 5) in
  let t2 = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 2) in
  let r = CongruenceBasis.congr_or t1 t2 in
  (* k=min(3,2)=2, mask=3, r = (5|2)&3 = 7&3 = 3. Result: 4Z+3 *)
  assert_equal ~msg:"or modulus" (Z.of_int 4) r.modulus;
  assert_equal ~msg:"or residue" (Z.of_int 3) r.residue


let test_xor _ =
  let t1 = CongruenceBasis.mk test_bt (Z.of_int 8) (Z.of_int 5) in
  let t2 = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 2) in
  let r = CongruenceBasis.congr_xor t1 t2 in
  (* k=2, mask=3, r = (5 xor 2) & 3 = 7 & 3 = 3. Result: 4Z+3 *)
  assert_equal ~msg:"xor modulus" (Z.of_int 4) r.modulus;
  assert_equal ~msg:"xor residue" (Z.of_int 3) r.residue


(* ---- Shift tests ---- *)

let test_shl _ =
  (* 4Z+1 << 2 = gcd(4*4, 256)Z + ((1*4) mod 256) = gcd(16,256)Z+4 = 16Z+4 *)
  let t = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 1) in
  let k = CongruenceBasis.of_const test_bt (Z.of_int 2) in
  let r = CongruenceBasis.congr_shl t k in
  assert_equal ~msg:"shl modulus" (Z.of_int 16) r.modulus;
  assert_equal ~msg:"shl residue" (Z.of_int 4) r.residue


let test_lshr _ =
  (* 8Z+4 >> 2: trailing_zeros(8)=3, k=2 <= 3, so (8>>2)Z+(4>>2) = 2Z+1 *)
  let t = CongruenceBasis.mk test_bt (Z.of_int 8) (Z.of_int 4) in
  let k = CongruenceBasis.of_const test_bt (Z.of_int 2) in
  let r = CongruenceBasis.congr_lshr t k in
  assert_equal ~msg:"lshr modulus" (Z.of_int 2) r.modulus;
  assert_equal ~msg:"lshr residue" (Z.of_int 1) r.residue


let test_lshr_top _ =
  (* 4Z+1 >> 3: trailing_zeros(4)=2, k=3 > 2, so top *)
  let t = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 1) in
  let k = CongruenceBasis.of_const test_bt (Z.of_int 3) in
  let r = CongruenceBasis.congr_lshr t k in
  assert_bool "lshr insufficient trailing zeros gives top" (CongruenceBasis.is_top r)


let test_shl_ge_width _ =
  (* Shift >= width is top (project-wide shift>=width = top); previously shl
     returned the singleton {0}. *)
  let t = CongruenceBasis.of_const test_bt (Z.of_int 5) in
  let k = CongruenceBasis.of_const test_bt (Z.of_int 8) in
  let r = CongruenceBasis.congr_shl t k in
  assert_bool "shl by width is top" (CongruenceBasis.is_top r)


let test_shl_huge_amount _ =
  (* A 64-bit shift amount >= 2^62 exceeds native int; the clamp must return top
     WITHOUT raising Z.Overflow from Z.to_int. *)
  let bt64 = BT.Bits (Unsigned, 64) in
  let t = CongruenceBasis.of_const bt64 (Z.of_int 5) in
  let k = CongruenceBasis.of_const bt64 (Z.shift_left Z.one 62) in
  let r = CongruenceBasis.congr_shl t k in
  assert_bool "huge shl amount is top (no overflow)" (CongruenceBasis.is_top r)


(* ---- Cast tests ---- *)

let test_cast_truncation _ =
  (* 16Z+5 in uint16 -> uint8: ξ gives gcd(16, 256)=16, 5 mod 16 = 5 *)
  let t = CongruenceBasis.mk test_bt_16 (Z.of_int 16) (Z.of_int 5) in
  let r = CongruenceBasis.cast t test_bt in
  assert_equal ~msg:"cast modulus" (Z.of_int 16) r.modulus;
  assert_equal ~msg:"cast residue" (Z.of_int 5) r.residue


let test_cast_extension _ =
  (* 4Z+1 in uint8 -> uint16: ξ gives gcd(4, 65536)=4, 1 mod 4 = 1 *)
  let t = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 1) in
  let r = CongruenceBasis.cast t test_bt_16 in
  assert_equal ~msg:"cast modulus" (Z.of_int 4) r.modulus;
  assert_equal ~msg:"cast residue" (Z.of_int 1) r.residue


(* ---- Backward tests ---- *)

let test_backward_eq _ =
  (* EQ refines both to meet *)
  let x = make_sym "x" in
  let eq_it = make_eq_constraint x 5 test_bt in
  let x_domain = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 1) in
  let const_5 = CongruenceBasis.of_const test_bt (Z.of_int 5) in
  let result = CongruenceBasis.backward_abs_it eq_it [ x_domain; const_5 ] in
  match result with
  | [ r1; r2 ] ->
    (* Both should be meet(4Z+1, 0Z+5) = 0Z+5 since 5 ≡ 1 (mod 4) *)
    assert_equal ~msg:"backward eq r1 modulus" Z.zero r1.modulus;
    assert_equal ~msg:"backward eq r1 residue" (Z.of_int 5) r1.residue;
    assert_equal ~msg:"backward eq r2 modulus" Z.zero r2.modulus;
    assert_equal ~msg:"backward eq r2 residue" (Z.of_int 5) r2.residue
  | _ -> assert_failure "backward eq should return 2 elements"


let test_backward_ne_contradiction _ =
  (* NE with same singletons -> bottom *)
  let x = make_sym "x" in
  let eq_it = make_eq_constraint x 5 test_bt in
  let ne_it = MT.not_ eq_it test_loc in
  let c5a = CongruenceBasis.of_const test_bt (Z.of_int 5) in
  let c5b = CongruenceBasis.of_const test_bt (Z.of_int 5) in
  let result = CongruenceBasis.backward_abs_it ne_it [ c5a; c5b ] in
  match result with
  | [ r1; r2 ] ->
    assert_bool "ne contradiction r1 bottom" (CongruenceBasis.is_bottom r1);
    assert_bool "ne contradiction r2 bottom" (CongruenceBasis.is_bottom r2)
  | _ -> assert_failure "backward ne should return 2 elements"


let test_backward_add _ =
  (* z = x + y, z = 0Z+7, x = 4Z+1, y = 2Z+0
     x' = meet(4Z+1, sub(0Z+7, 2Z+0)) = meet(4Z+1, 2Z+1) = 4Z+1 (4Z+1 ⊆ 2Z+1)
     y' = meet(2Z+0, sub(0Z+7, 4Z+1)) = meet(2Z+0, 4Z+2) = 4Z+2 (narrowed!) *)
  let z = CongruenceBasis.of_const test_bt (Z.of_int 7) in
  let x = CongruenceBasis.mk test_bt (Z.of_int 4) (Z.of_int 1) in
  let y = CongruenceBasis.mk test_bt (Z.of_int 2) Z.zero in
  let add_it =
    MT.binop
      Cn.Terms.Add
      ( MT.sym_ (make_sym "x", test_bt, test_loc),
        MT.sym_ (make_sym "y", test_bt, test_loc) )
      test_loc
      test_bt
  in
  let result = CongruenceBasis.backward_abs_it add_it [ z; x; y ] in
  match result with
  | [ r1; r2 ] ->
    (* x stays at 4Z+1 *)
    assert_equal ~msg:"backward add x modulus" (Z.of_int 4) r1.modulus;
    assert_equal ~msg:"backward add x residue" (Z.of_int 1) r1.residue;
    (* y narrows from 2Z+0 to 4Z+2 *)
    assert_equal ~msg:"backward add y modulus" (Z.of_int 4) r2.modulus;
    assert_equal ~msg:"backward add y residue" (Z.of_int 2) r2.residue
  | _ -> assert_failure "backward add should return 2 elements"


(* ---- Xi-normalization tests ---- *)

let test_xi_normalization _ =
  (* 6Z+1 in uint8: gcd(6, 256) = 2, 1 mod 2 = 1. Result: 2Z+1 *)
  let t = CongruenceBasis.mk test_bt (Z.of_int 6) (Z.of_int 1) in
  assert_equal ~msg:"xi modulus" (Z.of_int 2) t.modulus;
  assert_equal ~msg:"xi residue" (Z.of_int 1) t.residue


let test_xi_power_of_two _ =
  (* 8Z+3 in uint8: gcd(8, 256) = 8, 3 mod 8 = 3. Unchanged. *)
  let t = CongruenceBasis.mk test_bt (Z.of_int 8) (Z.of_int 3) in
  assert_equal ~msg:"xi pow2 modulus" (Z.of_int 8) t.modulus;
  assert_equal ~msg:"xi pow2 residue" (Z.of_int 3) t.residue


(* ---- Non-relational domain tests ---- *)

let test_domain_abs_assert _ =
  let x = make_sym "x" in
  let eq5 = make_eq_constraint x 5 test_bt in
  let d : CongruenceDomain.t = Some Sym.Map.empty in
  let d' = CongruenceDomain.abs_assert (LC.T eq5) d in
  match d' with
  | Some m ->
    (match Sym.Map.find_opt x m with
     | Some basis ->
       assert_equal ~msg:"abs_assert modulus" Z.zero basis.modulus;
       assert_equal ~msg:"abs_assert residue" (Z.of_int 5) basis.residue
     | None -> assert_failure "x should be in domain after abs_assert")
  | None -> assert_failure "domain should not be bottom"


let test_domain_conjunction _ =
  let x = make_sym "x" in
  let eq5 = make_eq_constraint x 5 test_bt in
  let eq7 = make_eq_constraint x 7 test_bt in
  let conj = make_and eq5 eq7 in
  let d : CongruenceDomain.t = Some Sym.Map.empty in
  let d' = CongruenceDomain.abs_assert (LC.T conj) d in
  (* x == 5 && x == 7 is unsatisfiable *)
  assert_equal ~msg:"contradictory conjunction is bottom" None d'


(** Test suite *)
let suite =
  "Congruence Domain Tests"
  >::: [ "bottom" >:: test_bottom;
         "top" >:: test_top;
         "const" >:: test_const;
         "const_wrapping" >:: test_const_wrapping;
         "of_interval" >:: test_of_interval;
         "to_interval" >:: test_to_interval;
         "leq" >:: test_leq;
         "join" >:: test_join;
         "join_same" >:: test_join_same;
         "join_with_stride" >:: test_join_with_stride;
         "meet" >:: test_meet;
         "meet_crt" >:: test_meet_crt;
         "meet_incompatible" >:: test_meet_incompatible;
         "add" >:: test_add;
         "sub" >:: test_sub;
         "neg" >:: test_neg;
         "mul" >:: test_mul;
         "mul_constants" >:: test_mul_constants;
         "div" >:: test_div;
         "div_inexact" >:: test_div_inexact;
         "mod" >:: test_mod;
         "and" >:: test_and;
         "or" >:: test_or;
         "xor" >:: test_xor;
         "shl" >:: test_shl;
         "lshr" >:: test_lshr;
         "lshr_top" >:: test_lshr_top;
         "shl_ge_width" >:: test_shl_ge_width;
         "shl_huge_amount" >:: test_shl_huge_amount;
         "cast_truncation" >:: test_cast_truncation;
         "cast_extension" >:: test_cast_extension;
         "backward_eq" >:: test_backward_eq;
         "backward_ne_contradiction" >:: test_backward_ne_contradiction;
         "backward_add" >:: test_backward_add;
         "xi_normalization" >:: test_xi_normalization;
         "xi_power_of_two" >:: test_xi_power_of_two;
         "domain_abs_assert" >:: test_domain_abs_assert;
         "domain_conjunction" >:: test_domain_conjunction
       ]
