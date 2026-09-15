import Iris.ProgramLogic.WeakestPre
import Iris.ProgramLogic.Adequacy
import Iris.ProgramLogic.Lifting
import Iris.BI.Lib.GenHeap
import Iris.Std.GenSetsInstances
import Iris.ProofMode
import Std.Data.ExtTreeMap
import CN_Lib.CN_Lib
import CN_Lib.CN_Lib_Tactics

open Iris ProgramLogic Language.Notation Std FromMathlib BI CN_Lib

namespace CN_Lib

-- Library for things needed by CN lemma exports.

-- 1, Resource algebra for heaps that are maps from integers
--  to integers.

abbrev myHeapF := fun V => Std.ExtTreeMap Int V compare

-- Pointers are just integers for now
abbrev Ptr := Int
-- Values are option integers for now
abbrev Val := Option Int

-- class MyHeap (hlc : outParam HasLC) (GF : BundledGFunctors) where
--   heap : genHeapGS Ptr Val GF myHeapF

class MyHeap (hlc : outParam HasLC) (GF : BundledGFunctors) extends genHeapGS Ptr Val GF myHeapF

-- attribute [reducible, instance] MyHeap.heap

variable [MyHeap hlc GF]


-- 2, CN-style ownership predicates.
--  Every ownership predicate reduces to ownership of unsigned
--  bytes and a proof that the value is within bounds for that type.

-- Unsigned C integer types
def Owned_UChar (l : Ptr) (v : Int) : IProp GF := iprop%
    (l ↦ some v) -- Pointsto predicate
  ∗ ⌜l ≠ 0⌝ -- Pointer is non-null
  ∗ ⌜ min_UChar ≤ v ∧ v ≤ max_UChar ⌝ -- Value is within bounds
def Owned_UShort (l : Ptr) (v : Int) : IProp GF :=
  let bytes := UShort_bytes v
    iprop%
    (Owned_UChar l (bytes 0) ∗ Owned_UChar (l + 1) (bytes 1))
  ∗ ⌜ min_UShort ≤ v ∧ v ≤ max_UShort ⌝
def Owned_UInt (l : Ptr) (v : Int) : IProp GF :=
  let bytes := UInt_bytes v
  iprop%
    ([∗list] i ↦ b ∈ List.ofFn bytes, Owned_UChar (l + i) b)
  ∗ ⌜ min_UInt ≤ v ∧ v ≤ max_UInt ⌝
def Owned_ULong (l : Ptr) (v : Int) : IProp GF :=
  let bytes := ULong_bytes v
  iprop%
    ([∗list] i ↦ b ∈ List.ofFn bytes, Owned_UChar (l + i) b)
  ∗ ⌜ min_ULong ≤ v ∧ v ≤ max_ULong ⌝

-- Signed C integer types
def Owned_Char (l : Ptr) (v : Int) : IProp GF :=
  let byte := wrapI min_UChar max_UChar v
  iprop%
    Owned_UChar l byte
  ∗ ⌜ min_Char ≤ v ∧ v ≤ max_Char ⌝
def Owned_Short (l : Ptr) (v : Int) : IProp GF :=
  let bytes := UShort_bytes (wrapI min_UShort max_UShort v)
  iprop%
    (Owned_UChar l (bytes 0) ∗ Owned_UChar (l + 1) (bytes 1))
  ∗ ⌜ min_Short ≤ v ∧ v ≤ max_Short ⌝
def Owned_Int (l : Ptr) (v : Int) : IProp GF :=
  let bytes := UInt_bytes (wrapI min_UInt max_UInt v)
  iprop%
    ([∗list] i ↦ b ∈ List.ofFn bytes, Owned_UChar (l + i) b)
  ∗ ⌜ min_Int ≤ v ∧ v ≤ max_Int ⌝
def Owned_Long (l : Ptr) (v : Int) : IProp GF :=
  let bytes := ULong_bytes (wrapI min_ULong max_ULong v)
  iprop%
    ([∗list] i ↦ b ∈ List.ofFn bytes, Owned_UChar (l + i) b)
  ∗ ⌜ min_Long ≤ v ∧ v ≤ max_Long ⌝

-- 3, Array ownership
def Block (l : Ptr) : IProp GF := iprop% (l ↦ none) ∧ ⌜l ≠ 0⌝

def arrayshift (l : Ptr) (pos : Int) (size : Int) : Ptr := l + pos * size

-- 4, Pointer non-null theorems
theorem Owned_UChar_nonnull {l : Ptr} {v : Int} :
  ⊢@{IProp GF} Owned_UChar l v → ⌜l ≠ 0⌝ :=
by
  unfold Owned_UChar
  iintro ⟨_ , ⟨HOwn, _⟩⟩; iframe

theorem Owned_Char_nonnull {l : Ptr} {v : Int} :
  ⊢@{IProp GF} Owned_Char l v → ⌜l ≠ 0⌝ :=
by
  unfold Owned_Char Owned_UChar
  iintro ⟨⟨_ , ⟨H , _⟩⟩ , _⟩; iframe

theorem Owned_UShort_nonnull {l : Ptr} {v : Int} :
  ⊢@{IProp GF} Owned_UShort l v → ⌜l ≠ 0⌝ :=
by
  unfold Owned_UShort Owned_UChar
  iintro ⟨⟨⟨_ , ⟨H , _⟩⟩ , _⟩ , _⟩; iframe

theorem Owned_Short_nonnull {l : Ptr} {v : Int} :
  ⊢@{IProp GF} Owned_Short l v → ⌜l ≠ 0⌝ :=
by
  unfold Owned_Short Owned_UChar
  iintro ⟨⟨⟨_ , ⟨H , _⟩⟩ , _⟩ , _⟩; iframe

theorem Owned_UInt_nonnull {l : Ptr} {v : Int} :
  ⊢@{IProp GF} Owned_UInt l v → ⌜l ≠ 0⌝ :=
by
  unfold Owned_UInt Owned_UChar
  iintro ⟨H , _⟩; simp
  icases H with ⟨⟨_ , ⟨H , _⟩⟩ , _⟩; iframe

theorem Owned_Int_nonnull {l : Ptr} {v : Int} :
  ⊢@{IProp GF} Owned_Int l v → ⌜l ≠ 0⌝ :=
by
  unfold Owned_Int Owned_UChar
  iintro ⟨H , _⟩; simp
  icases H with ⟨⟨_ , ⟨H , _⟩⟩ , _⟩; iframe

theorem Owned_ULong_nonnull {l : Ptr} {v : Int} :
  ⊢@{IProp GF} Owned_ULong l v → ⌜l ≠ 0⌝ :=
by
  unfold Owned_ULong Owned_UChar
  iintro ⟨H , _⟩; simp
  icases H with ⟨⟨_ , ⟨H , _⟩⟩ , _⟩; iframe

theorem Owned_Long_nonnull {l : Ptr} {v : Int} :
  ⊢@{IProp GF} Owned_Long l v → ⌜l ≠ 0⌝ :=
by
  unfold Owned_Long Owned_UChar
  iintro ⟨H , _⟩; simp
  icases H with ⟨⟨_ , ⟨H , _⟩⟩ , _⟩; iframe

-- 5, Value equality theorems for ownership predicates

theorem Owned_UChar_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UChar l v -∗ Owned_UChar l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_UChar min_UChar max_UChar
  iintro ⟨H1 , ⟨_ , ⟨%h1, %h1'⟩⟩⟩ ⟨H2 , ⟨_ , ⟨%h2 , %h2'⟩⟩⟩
  ihave %H := pointsTo_agree $$ [$]
  ipureintro; injection H with val_eq; lia

theorem Owned_Char_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Char l v -∗ Owned_Char l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_Char min_Char max_Char
  iintro ⟨H1 , %H2⟩ ⟨H3 , %H4⟩
  ihave %HEq := Owned_UChar_eq $$ H1 H3
  ipureintro; apply wrapI_Char_cong H2 H4 HEq

theorem Owned_UShort_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UShort l v -∗ Owned_UShort l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_UShort
  iintro ⟨HOwn, %H⟩ ⟨HOwn', %H'⟩
  solve_owned_eq Owned_UChar_eq (HOwn, HOwn') v v' H H'

theorem Owned_Short_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Short l v -∗ Owned_Short l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_Short
  iintro ⟨HOwn, %H⟩ ⟨HOwn', %H'⟩
  solve_owned_eq Owned_UChar_eq (HOwn, HOwn') v v' H H'

theorem Owned_UInt_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UInt l v -∗ Owned_UInt l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_UInt
  iintro ⟨HOwn, %H⟩ ⟨HOwn', %H'⟩
  simp [-UInt_bytes]
  solve_owned_eq Owned_UChar_eq (HOwn, HOwn') v v' H H'

theorem Owned_Int_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Int l v -∗ Owned_Int l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_Int
  iintro ⟨HOwn, %H⟩ ⟨HOwn', %H'⟩
  simp [-UInt_bytes]
  solve_owned_eq Owned_UChar_eq (HOwn, HOwn') v v' H H'

theorem Owned_ULong_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_ULong l v -∗ Owned_ULong l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_ULong
  iintro ⟨HOwn, %H⟩ ⟨HOwn', %H'⟩
  simp [-ULong_bytes]
  solve_owned_eq Owned_UChar_eq (HOwn, HOwn') v v' H H'

theorem Owned_Long_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Long l v -∗ Owned_Long l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_Long
  iintro ⟨HOwn, %H⟩ ⟨HOwn', %H'⟩
  simp [-ULong_bytes]
  solve_owned_eq Owned_UChar_eq (HOwn, HOwn') v v' H H'

-- 6, Pointer inequality theorems for ownership predicates

theorem Owned_UChar_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UChar l v -∗ Owned_UChar l' v' -∗ ⌜l ≠ l'⌝ :=
by
  unfold Owned_UChar
  iintro ⟨H1 , _⟩ ⟨H2 , _⟩
  iapply pointsTo_ne $$ H1 H2

theorem Owned_Char_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Char l v -∗ Owned_Char l' v' -∗ ⌜l ≠ l'⌝ :=
by
  unfold Owned_Char
  iintro ⟨H1 , _⟩ ⟨H2 , _⟩
  iapply Owned_UChar_neq $$ H1 H2

theorem Owned_UShort_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UShort l v -∗ Owned_UShort l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_UShort Owned_UChar_neq

theorem Owned_Short_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Short l v -∗ Owned_Short l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_Short Owned_UChar_neq

theorem Owned_UInt_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UInt l v -∗ Owned_UInt l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_UInt Owned_UChar_neq

theorem Owned_Int_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Int l v -∗ Owned_Int l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_Int Owned_UChar_neq

theorem Owned_ULong_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_ULong l v -∗ Owned_ULong l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_ULong Owned_UChar_neq

theorem Owned_Long_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Long l v -∗ Owned_Long l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_Long Owned_UChar_neq
