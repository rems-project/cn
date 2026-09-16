namespace CN_Lib

-- CN's represents C integer types as mathematical integers with bounds.
-- The following bounds are enforced by the ownership predicates defined in CN_Lib_Iris.
-- Bounds on the unsigned integer types.
abbrev min_UChar : Int := 0
abbrev max_UChar : Int := 2 ^ 8 - 1
abbrev min_UShort := 0
abbrev max_UShort := 2 ^ 16 - 1
abbrev min_UInt : Int := 0
abbrev max_UInt : Int := 2 ^ 32 - 1
abbrev min_ULong : Int := 0
abbrev max_ULong : Int := 2 ^ 64 - 1

-- Bounds on the signed integer types.
abbrev min_Char : Int := - (2 ^ 7)
abbrev max_Char : Int := 2 ^ 7 - 1
abbrev min_Short : Int := - (2 ^ 15)
abbrev max_Short : Int := 2 ^ 15 - 1
abbrev min_Int : Int := - (2 ^ 31)
abbrev max_Int : Int := 2 ^ 31 - 1
abbrev min_Long : Int := - (2 ^ 63)
abbrev max_Long : Int := 2 ^ 63 - 1

-- We roll our own pseudo-bitvectors below to match CN's representation of C
-- integer types as mathematical integers with bounds, which is slightly
-- different from what Lean's bitvector library provides.

-- This function converts an integer to a vector of n bytes (represented as
--    integers) in big-endian order.
@[simp, reducible]
def to_CN_BitVec (n : Nat) : Int → (Fin n → Int) :=
  fun v i => (v % 2 ^(8 * (n - i.toNat))) / (2 ^ (8 * (n - 1 - i.toNat)))
@[simp, reducible]
def from_CN_BitVec {n : Nat} (f : Fin n → Int) : Int :=
  List.foldl
    (fun acc i => acc + f i * 2 ^ (8 * (n - 1 - i.toNat)))
    0
    (List.finRange n)

@[simp, reducible]
def UShort_bytes (v : Int) : Fin 2 → Int := to_CN_BitVec 2 v

@[simp, reducible]
def UInt_bytes (v : Int) : Fin 4 → Int := to_CN_BitVec 4 v

@[simp, reducible]
def ULong_bytes (v : Int) : Fin 8 → Int := to_CN_BitVec 8 v

@[simp, reducible]
def bytes_UShort (f : Fin 2 → Int) : Int := from_CN_BitVec f

@[simp, reducible]
def bytes_UInt (f : Fin 4 → Int) : Int := from_CN_BitVec f

@[simp, reducible]
def bytes_ULong (f : Fin 8 → Int) : Int := from_CN_BitVec f

theorem UShort_idem {v : Int} :
   min_UShort ≤ v ∧ v ≤ max_UShort
  → v = bytes_UShort (UShort_bytes v) :=
by
  intro ⟨Hv , Hv'⟩
  simp [List.finRange]; lia

theorem UInt_idem {v : Int} :
   min_UInt ≤ v ∧ v ≤ max_UInt
  → v = bytes_UInt (UInt_bytes v) :=
by
  intro ⟨Hv , Hv'⟩
  unfold UInt_bytes  bytes_UInt from_CN_BitVec to_CN_BitVec List.finRange
  simp only [List.ofFn, Fin.foldr, Fin.foldr.loop, List.foldl]
  simp only [Fin.toNat]
  lia

theorem ULong_idem {v : Int} :
   min_ULong ≤ v ∧ v ≤ max_ULong
  → v = bytes_ULong (ULong_bytes v) :=
by
  intro ⟨Hv , Hv'⟩
  unfold ULong_bytes  bytes_ULong from_CN_BitVec to_CN_BitVec List.finRange
  simp only [List.ofFn, Fin.foldr, Fin.foldr.loop, List.foldl]
  simp only [Fin.toNat]
  lia

theorem UShort_eq {v v' : Int} :
    min_UShort ≤ v ∧ v ≤ max_UShort
  → min_UShort ≤ v' ∧ v' ≤ max_UShort
  → UShort_bytes v = UShort_bytes v'
  → v = v' :=
by
  intros Hv Hv' H
  unfold UShort_bytes at H
  have HEq : v = bytes_UShort (UShort_bytes v) := by
    apply UShort_idem; assumption
  have HEq' : v' = bytes_UShort (UShort_bytes v') := by
    apply UShort_idem; assumption
  unfold UShort_bytes at HEq HEq'
  rw [H] at HEq; symm at HEq'
  rw [HEq'] at HEq; assumption

theorem UInt_eq {v v' : Int} :
    min_UInt ≤ v ∧ v ≤ max_UInt
  → min_UInt ≤ v' ∧ v' ≤ max_UInt
  → UInt_bytes v = UInt_bytes v'
  → v = v' :=
by
  intros Hv Hv' H
  unfold UInt_bytes at H
  have HEq : v = bytes_UInt (UInt_bytes v) := by
    apply UInt_idem; assumption
  have HEq' : v' = bytes_UInt (UInt_bytes v') := by
    apply UInt_idem; assumption
  unfold UInt_bytes at HEq HEq'
  rw [H] at HEq; symm at HEq'
  rw [HEq'] at HEq; assumption

theorem ULong_eq {v v' : Int} :
    min_ULong ≤ v ∧ v ≤ max_ULong
  → min_ULong ≤ v' ∧ v' ≤ max_ULong
  → ULong_bytes v = ULong_bytes v'
  → v = v' :=
by
  intros Hv Hv' H
  unfold ULong_bytes at H
  have HEq : v = bytes_ULong (ULong_bytes v) := by
    apply ULong_idem; assumption
  have HEq' : v' = bytes_ULong (ULong_bytes v') := by
    apply ULong_idem; assumption
  unfold ULong_bytes at HEq HEq'
  rw [H] at HEq; symm at HEq'
  rw [HEq'] at HEq; assumption

-- wrapI and related theorems
def wrapI (minInt : Int) (maxInt : Int) x :=
  let delta := ((maxInt - minInt) + 1)
  let r := x % delta
  (if (r <= maxInt) then r else r - delta)

theorem wrapI_within_bounds :
  ∀ {min max x : Int}, (min ≤ 0 ∧ 0 < max) →
  min ≤ wrapI min max x ∧ wrapI min max x ≤ max :=
by
  intro min max x H
  unfold wrapI
  let delta := max - min + 1
  simp; constructor
  · by_cases h : x % (max - min + 1) ≤ max
    · simp [h]
      have hnonneg : 0 ≤ x % (max - min + 1) := by
        exact Int.emod_nonneg x (by omega)
      omega
    · simp [h]; omega
  · by_cases h : x % (max - min + 1) ≤ max
    · simp [h]
    · simp [h]
      have hd : 0 < max - min + 1 := by omega
      have hmod : x % (max - min + 1) < max - min + 1 := by
        exact Int.emod_lt_of_pos x hd
      omega

theorem dvd_mod_minus (a b c : Int):
  (0 ≤ c) ∧ (c < b) → (b ∣ a - c) → a % b = c :=
by
  rintro ⟨hc0, hcb⟩ ⟨k, hk⟩
  have ha : a = c + b * k := by omega
  simp [ha, Int.add_mul_emod_self_left, Int.emod_eq_of_lt hc0 hcb]

theorem wrapI_idem:
  ∀ {min max x : Int},
  (min ≤ x ∧ x ≤ max) → (min ≤ 0 ∧ 0 < max) →
  wrapI min max x = x :=
by
  intros min max x H1 H2
  unfold wrapI
  let delta := ((max - min) + 1)
  by_cases hx : 0 ≤ x
  · simp
    rw [Int.emod_eq_of_lt]
    omega; exact hx ;omega
  · simp
    rw [dvd_mod_minus _ _ (x + delta)]
    · by_cases hx' : (x + delta <= max)
      · omega
      · omega
    · omega
    · exists (-1); omega

theorem wrapI_pos_neg_neq :
  ∀ {max Umax v v' : Int} {_: Umax = 2 * max + 1},
  0 ≤ v ∧ v ≤ max → -max - 1 ≤ v' ∧ v' < 0 →
  wrapI 0 Umax v ≠ wrapI 0 Umax v' :=
by
  intros max Umax v v' inv Hv Hv'
  unfold wrapI
  simp [inv, Int.add_assoc]
  have hvrem : v % (2 * max + 2) = v := by
    apply Int.emod_eq_of_lt
    · exact ‹0 ≤ v ∧ v ≤ max›.1
    · omega
  have hvrem' : v' % (2 * max + 2) = v' + (2 * max + 2) := by
    rw [Int.emod_eq_add_self_emod]
    apply Int.emod_eq_of_lt; omega; omega
  rw [hvrem, hvrem']; omega

theorem wrapI_cong_pos :
  ∀ {max Umax v v' : Int} {_ : 0 < max}
    {_ : Umax = (2 * max + 1)},
    0 ≤ v ∧ v ≤ max → 0 ≤ v' ∧ v' ≤ max
  → wrapI 0 Umax v = wrapI 0 Umax v'
  → v = v' :=
by
  intro max Umax v v' hpos inv ⟨H1, H1'⟩ ⟨H2, H2'⟩ Hwrap
  have HEq : wrapI 0 Umax v = v := by
    apply wrapI_idem; lia; simp [inv]; lia
  have HEq' : wrapI 0 Umax v' = v' := by
    apply wrapI_idem; lia; simp [inv]; lia
  rw [HEq, HEq'] at Hwrap; assumption

theorem wrapI_cong_neg :
  ∀ {max Umax v v' : Int} {_ : 0 < max}
    {_ : Umax = (2 * max + 1)},
    -max - 1 ≤ v ∧ v < 0 → -max - 1 ≤ v' ∧ v' < 0
  → wrapI 0 Umax v = wrapI 0 Umax v'
  → v = v' :=
by
  intro max Umax v v' hmax hU hv hv' hEq
  have hmod : 0 < 2 * max + 2 := by omega
  have hrem : v % (2 * max + 2) < 2 * max + 2 := by
    exact Int.emod_lt_of_pos v hmod
  have hrem' : v' % (2 * max + 2) < 2 * max + 2 := by
    exact Int.emod_lt_of_pos v' hmod
  have h : v % (2 * max + 2) ≤ 2 * max + 1 := by omega
  have h' : v' % (2 * max + 2) ≤ 2 * max + 1 := by omega
  simp [wrapI, hU, Int.add_assoc, h, h'] at hEq
  have hvrem : v % (2 * max + 2) = v + (2 * max + 2) := by
    rw [Int.emod_eq_add_self_emod]
    apply Int.emod_eq_of_lt <;> omega
  have hvrem' : v' % (2 * max + 2) = v' + (2 * max + 2) := by
    rw [Int.emod_eq_add_self_emod]
    apply Int.emod_eq_of_lt <;> omega
  rw [hvrem, hvrem'] at hEq; omega

theorem wrapI_cong :
  ∀ {max v v' : Int}, 0 < max
  → -max - 1 ≤ v ∧ v ≤ max → -max - 1 ≤ v' ∧ v' ≤ max
  → wrapI 0 (2 * max + 1) v = wrapI 0 (2 * max + 1) v'
  → v = v' :=
by
  intro max v v' hpos H1 H2 Hwrap
  by_cases h0 : 0 ≤ v
  · have h : 0 ≤ v ∧ v ≤ max := by omega
    by_cases h0' : 0 ≤ v'
    · have h' : 0 ≤ v' ∧ v' ≤ max := by omega
      apply wrapI_cong_pos h h' Hwrap
      assumption; rfl
    · exfalso
      have h' : -max - 1 ≤ v' ∧ v' < 0 := by omega
      apply wrapI_pos_neg_neq h h' Hwrap; rfl
  · have h : -max - 1 ≤ v ∧ v < 0 := by omega
    by_cases h0' : 0 ≤ v'
    · exfalso
      have h' : 0 ≤ v' ∧ v' ≤ max := by omega
      apply wrapI_pos_neg_neq h' h Hwrap.symm; rfl
    · have h' : -max - 1 ≤ v' ∧ v' < 0 := by omega
      apply wrapI_cong_neg h h' Hwrap
      assumption; rfl

theorem wrapI_Char_cong :
  min_Char ≤ v ∧ v ≤ max_Char
  → min_Char ≤ v' ∧ v' ≤ max_Char
  → wrapI min_UChar max_UChar v = wrapI min_UChar max_UChar v'
  → v = v' :=
by
  intro Hv Hv' Hwrap
  apply wrapI_cong (max := max_Char) (by lia) Hv Hv' Hwrap

theorem wrapI_Short_cong :
  min_Short ≤ v ∧ v ≤ max_Short
  → min_Short ≤ v' ∧ v' ≤ max_Short
  → wrapI min_UShort max_UShort v = wrapI min_UShort max_UShort v'
  → v = v' :=
by
  intro Hv Hv' Hwrap
  apply wrapI_cong (max := max_Short) (by lia) Hv Hv' Hwrap

theorem wrapI_Int_cong :
  min_Int ≤ v ∧ v ≤ max_Int
  → min_Int ≤ v' ∧ v' ≤ max_Int
  → wrapI min_UInt max_UInt v = wrapI min_UInt max_UInt v'
  → v = v' :=
by
  intro Hv Hv' Hwrap
  apply wrapI_cong (max := max_Int) (by lia) Hv Hv' Hwrap

theorem wrapI_Long_cong :
  min_Long ≤ v ∧ v ≤ max_Long
  → min_Long ≤ v' ∧ v' ≤ max_Long
  → wrapI min_ULong max_ULong v = wrapI min_ULong max_ULong v'
  → v = v' :=
by
  intro Hv Hv' Hwrap
  apply wrapI_cong (max := max_Long) (by lia) Hv Hv' Hwrap
