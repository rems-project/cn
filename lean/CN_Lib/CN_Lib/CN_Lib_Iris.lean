import Iris.ProgramLogic.WeakestPre
import Iris.ProgramLogic.Adequacy
import Iris.ProgramLogic.Lifting
import Iris.BI.Lib.GenHeap
import Iris.Std.GenSetsInstances
import Iris.ProofMode
import Std.Data.ExtTreeMap
import CN_Lib.CN_Lib
import CN_Lib.CN_Lib_Tactics

open Iris ProgramLogic Language.Notation Std FromMathlib BI

namespace CN_Lib

-- Library for things needed by CN lemma exports.

-- 1, Resource algebra based on CN-VIP.

abbrev Addr    := Int -- Addresses are integers
abbrev AllocId := Int -- Allocation IDs are integers

structure Loc where -- Locations are address/provenance pairs
  addr         : Addr
  id           : AllocId

abbrev Ptr     := Option Loc -- Pointers are either null or a location
abbrev Val     := Option Int -- Values are option integers for now

/- The allocation history is a map from allocation IDs to the base address and the
  size of the allocation. Let's call the (base address, size) pair "AllocMetaData". -/
structure AllocMetaData where
  base         : Addr
  size          : Nat

abbrev AllocMap := Std.ExtTreeMap AllocId AllocMetaData compare

-- The liveset is a set of allocation IDs and a proof that the set is in dom(alloc history).
abbrev LiveSetR := Auth (LeibnizSet (Std.ExtTreeSet AllocId compare))

/- We stagger the definition of the allocation history resource algebra into two
  classes. The first class defines the liveset resource algebra. -/
class allocHistPreS (GF : BundledGFunctors) where
  livesetR  : ElemG GF (constOF LiveSetR)
attribute [reducible, instance] allocHistPreS.livesetR

/- The second class ensures only one instane of allocHistPreS exists by fixing
  a ghost name for the liveset. -/
class allocHistGS (GF : BundledGFunctors) extends allocHistPreS GF where
  liveset_name  : GName
  allocmap : AllocMap
  --TODO: This doesn't actually enforce the validity condition! Needs to be fixed
  liveset : Std.ExtTreeSet AllocId compare
  liveset_valid : liveset.inner.keys ⊆ allocmap.keys

-- The heap is a map from addresses to values
abbrev VIP_HeapF := fun V => Std.ExtTreeMap Addr V compare

-- The CN-VIP heap is a tuple ((A, L), M) = ((allocation history, liveset), memory)
class VIP_HeapGS (GF : BundledGFunctors) where
  provenance : allocHistGS GF
  memory     : genHeapGS Addr Val GF VIP_HeapF

attribute [reducible, instance] VIP_HeapGS.provenance
attribute [reducible, instance] VIP_HeapGS.memory

variable {GF : BundledGFunctors} [G : VIP_HeapGS GF]

-- Here is some nicer syntax for looking stuff up in the allocation history.
def AllocHist_elem (l : AllocId) (v : AllocMetaData) : IProp GF := iprop%
  ⌜ (allocHistGS.allocmap GF).get? l = some v ⌝

def Live (l : AllocId) : IProp GF :=
  iOwn (E := allocHistPreS.livesetR) (allocHistGS.liveset_name GF) (◯ (.valid { l }))

syntax "AllocHistory[@" term "]" "=" "(" term ("," term)? ")" : term
macro_rules
  | `(AllocHistory[@$l] = ($v , true))   => `(iprop% AllocHist_elem $l $v ∗ Live $l)
  -- TODO: should this just be `(iprop% AllocHist_elem $l $v)?
  | `(AllocHistory[@$l] = ($v , false))  => `(iprop% AllocHist_elem $l $v ∗ ¬ Live $l)
  | `(AllocHistory[@$l] = ($v))          => `(iprop% AllocHist_elem $l $v)

-- Example usage of new syntax
def alloc_entry_example (l : AllocId) (b : Addr) (n : Nat) : IProp GF :=
  AllocHistory[@l] = ({ base := b, size := n }, true)
-- The following is also valid and corresponds to `A[@l] = { (b, n) , _ }`
def alloc_entry_example2 (l : AllocId) (b : Addr) (n : Nat) : IProp GF :=
  AllocHistory[@l] = ({ base := b, size := n })

-- 2, CN-style ownership predicates.
--  Every ownership predicate reduces to the generic 'Owned' predicate:
def Owned (ptr : Ptr) (val : Int)
          (min max : Int)
          (n : Nat) (bytes : Fin n → Int) : IProp GF := iprop%
  -- Assertions about the heap
    ∃ (l : Loc), ⌜ ptr = some l ⌝          -- Pointer is non-null
  ∗ ([∗list] i ↦ v ∈ List.ofFn bytes,
      (l.addr + i) ↦ (some v))             -- Points-to for each byte
  ∗ ⌜ min ≤ val ∧ val ≤ max ⌝              -- Value is within bounds
  -- Assertions about pointer provenance
  ∗ ∃ (b : Addr) (n' : Nat),               -- Allocation history entry exists
      AllocHistory[@l.id] = ({ base := b, size := n' })
  ∗ ⌜ b ≤ l.addr ∧ l.addr ≤ b + n ⌝        -- Address is within bounds

-- Ownership of unsigned integer types
@[simp, reducible]
def Owned_UChar (ptr : Ptr) (val : Int) : IProp GF := iprop%
  Owned ptr val min_UChar max_UChar 1 (fun _ => val)
@[simp, reducible]
def Owned_UShort (ptr : Ptr) (val : Int) : IProp GF := iprop%
  Owned ptr val min_UShort max_UShort 2 (UShort_bytes val)
@[simp, reducible]
def Owned_UInt (ptr : Ptr) (val : Int) : IProp GF := iprop%
  Owned ptr val min_UInt max_UInt 4 (UInt_bytes val)
@[simp, reducible]
def Owned_ULong (ptr : Ptr) (val : Int) : IProp GF := iprop%
  Owned ptr val min_ULong max_ULong 8 (ULong_bytes val)

-- Ownership of signed integer types
@[simp, reducible]
def Owned_Char (ptr : Ptr) (val : Int) : IProp GF := iprop%
  Owned ptr val min_Char max_Char 1 (fun _ => wrapI min_UChar max_UChar val)
@[simp, reducible]
def Owned_Short (ptr : Ptr) (val : Int) : IProp GF := iprop%
  Owned ptr val min_Short max_Short 2
    (UShort_bytes (wrapI min_UShort max_UShort val))
@[simp, reducible]
def Owned_Int (ptr : Ptr) (val : Int) : IProp GF := iprop%
  Owned ptr val min_Int max_Int 4
    (UInt_bytes (wrapI min_UInt max_UInt val))
@[simp, reducible]
def Owned_Long (ptr : Ptr) (val : Int) : IProp GF := iprop%
  Owned ptr val min_Long max_Long 8
    (ULong_bytes (wrapI min_ULong max_ULong val))

-- 3, Array ownership

def Block (ptr : Ptr) (L : List Int) : IProp GF := iprop%
  -- Assertions about the heap
    ∃ (l : Loc), ⌜ ptr = some l ⌝         -- Pointer is non-null
  ∗ ([∗list] i ∈ L, (l.addr + i) ↦ none)  -- Points-to for each byte
  -- Assertions about pointer provenance
  ∗ ∃ (b : Addr) (n' : Nat),              -- Allocation history entry exists
      AllocHistory[@l.id] = ({ base := b, size := n' })
  ∗ ⌜ b ≤ l.addr ∧ l.addr ≤ b + n' ⌝      -- Address is within bounds

-- Ownership of integer type-sized blocks
def Block_Char (l : Ptr) : IProp GF := iprop%
  Block l [0]
def Block_Short (l : Ptr) : IProp GF := iprop%
  Block l [0, 1]
def Block_Int (l : Ptr) : IProp GF := iprop%
  Block l [0, 1, 2, 3]
def Block_Long (l : Ptr) : IProp GF := iprop%
  Block l [0, 1, 2, 3, 4, 5, 6, 7]

-- TODO: Implement
def arrayshift (p : Ptr) (pos : Int) (size : Int) : Ptr := sorry

-- TODO: Implement
def padding (p : Ptr) (n : Nat) : IProp GF := sorry

-- 4, Value equality theorems for Owned
-- Helper theorem 1
theorem points_agree_addr_1 {l : Loc} {v v' : Int} :
  ⊢@{IProp GF} ((l.addr) ↦ some v) -∗ ((l.addr) ↦ some v') -∗ ⌜v = v'⌝ :=
by
  iintro H1 H2
  ihave %H := pointsTo_agree $$ [$]
  ipureintro; injection H with val_eq; lia

-- Helper theorem 2
theorem points_agree_addr_i {l : Loc} {i : Int} {v v' : Int} :
  ⊢@{IProp GF} ((l.addr + i) ↦ some v) -∗ ((l.addr + i) ↦ some v') -∗ ⌜v = v'⌝ :=
by
  iintro H1 H2
  ihave %H := pointsTo_agree $$ [$]
  ipureintro; injection H with val_eq; lia

theorem Owned_UChar_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UChar l v -∗ Owned_UChar l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_UChar Owned
  iintro ⟨%x , ⟨%HEq , ⟨HOwn , _⟩⟩⟩ ⟨%x' , ⟨%HEq' , ⟨HOwn' , _⟩⟩⟩
  simp
  icases HOwn with ⟨H1 , _⟩; icases HOwn' with ⟨H2 , _⟩
  have HEq : x' = x := by simpa [HEq'] using HEq
  rw [HEq]
  ihave %H := pointsTo_agree $$ [$]
  injection H with val_eq
  ipureintro; lia

theorem Owned_UShort_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UShort l v -∗ Owned_UShort l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_UShort Owned
  solve_owned_eq_unsigned UShort_bytes
    points_agree_addr_1 points_agree_addr_i v v'

theorem Owned_UInt_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UInt l v -∗ Owned_UInt l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_UInt Owned
  solve_owned_eq_unsigned UInt_bytes
    points_agree_addr_1 points_agree_addr_i v v'

theorem Owned_ULong_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_ULong l v -∗ Owned_ULong l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_ULong Owned
  solve_owned_eq_unsigned ULong_bytes
    points_agree_addr_1 points_agree_addr_i v v'

theorem Owned_Char_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Char l v -∗ Owned_Char l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_Char Owned
  iintro ⟨%x , ⟨%HEq , ⟨HOwn , ⟨%Hb , _⟩⟩⟩⟩ ⟨%x' , ⟨%HEq' , ⟨HOwn' , ⟨%Hb' , _⟩⟩⟩⟩
  simp
  icases HOwn with ⟨H1 , _⟩; icases HOwn' with ⟨H2 , _⟩
  have HEq : x' = x := by simpa [HEq'] using HEq
  rw [HEq]
  ihave %H := pointsTo_agree $$ [$]
  injection H with val_eq; ipureintro
  exact (wrapI_Char_cong Hb Hb' (val_eq.symm))

theorem Owned_Short_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Short l v -∗ Owned_Short l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_Short Owned
  solve_owned_eq_signed UShort_bytes points_agree_addr_1 points_agree_addr_i
    min_UShort max_UShort v v'

theorem Owned_Int_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Int l v -∗ Owned_Int l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_Int Owned
  solve_owned_eq_signed UInt_bytes points_agree_addr_1 points_agree_addr_i
    min_UInt max_UInt v v'

theorem Owned_Long_eq {l : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Long l v -∗ Owned_Long l v' -∗ ⌜v = v'⌝ :=
by
  unfold Owned_Long Owned
  solve_owned_eq_signed ULong_bytes points_agree_addr_1 points_agree_addr_i
    min_ULong max_ULong v v'

-- 5, Pointer inequality theorems for Owned and Block

theorem Owned_UChar_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UChar l v -∗ Owned_UChar l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_UChar Owned

theorem Owned_Char_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Char l v -∗ Owned_Char l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_Char Owned

theorem Owned_UShort_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UShort l v -∗ Owned_UShort l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_UShort Owned

theorem Owned_Short_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Short l v -∗ Owned_Short l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_Short Owned

theorem Owned_UInt_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_UInt l v -∗ Owned_UInt l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_UInt Owned

theorem Owned_Int_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Int l v -∗ Owned_Int l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_Int Owned

theorem Owned_ULong_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_ULong l v -∗ Owned_ULong l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_ULong Owned

theorem Owned_Long_neq {l l' : Ptr} {v v' : Int} :
  ⊢@{IProp GF} Owned_Long l v -∗ Owned_Long l' v' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Owned_Long Owned

theorem Block_Char_neq {l l' : Ptr} :
  ⊢@{IProp GF} Block_Char l -∗ Block_Char l' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Block_Char Block

theorem Block_Short_neq {l l' : Ptr} :
  ⊢@{IProp GF} Block_Short l -∗ Block_Short l' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Block_Short Block

theorem Block_Int_neq {l l' : Ptr} :
  ⊢@{IProp GF} Block_Int l -∗ Block_Int l' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Block_Int Block

theorem Block_Long_neq {l l' : Ptr} :
  ⊢@{IProp GF} Block_Long l -∗ Block_Long l' -∗ ⌜l ≠ l'⌝ :=
by
  solve_own_neq Block_Long Block
