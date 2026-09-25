Require Import ZArith Bool Lia.
From iris.proofmode Require Import proofmode.
From iris.bi.lib Require Import fractional.
From iris.base_logic.lib Require Export gen_heap.
From iris.algebra Require Import auth gmap gset.
From Coq.Vectors Require Import Vector.
From CN_Lemmas Require Import CN_Lib.
Open Scope Z.

(* Instantiating Iris with a heap *)

Section CN_Lib_Iris.

Notation Addr := Z. (* Addresses are integers *)
Notation AllocId := Z. (* Allocation IDs are integers *)

Record Loc := mkLoc { addr : Addr; id : AllocId }. (* Locations are address/provenance pairs *)

Notation Ptr := (option Loc). (* Pointers are either null or a loc. *)
Notation Val := (option Z). (* Values are either null or an integer. *)

(* Every AllocID is associated with a base address and a size.*)
Record AllocMetaData := mkAllocMetaData {
  alloc_base : Addr;
  alloc_size : Z;
}.

(* TODO: The below does not exactly match the Lean version of CN_Lib, someone should clean it up *)

(* Provenance information consists of two things: 
    1. The metadata associated with each alloc ID 
    2. The liveness of each alloc ID             
  We put this information into a resource algebra. *)
Definition allocmap := gmap AllocId (agreeR (leibnizO AllocMetaData)).
Definition liveset := gsetR AllocId.

Class allocHistPreS (Σ : gFunctors) := AllocHistPreGS {
  #[local] allocmapGS_inG :: inG Σ (authR allocmap);
  #[local] livesetGS_inG :: inG Σ (authR liveset);
}.

Class allocHistGS (Σ : gFunctors) := AllocHistGS {
  #[local] allocHistGS_inG :: allocHistPreS Σ;
  (* This resource algebra should be unital, which we enforce by having setting
    default ghost names. This trick is used by gen_heapGS too. *)
  allocMap_name : gname;
  liveset_name : gname
}.

Global Hint Mode AllocHistGS - - - - : typeclass_instances.

Definition AllocMapContains `{!allocHistGS Σ} (m : allocmap) : iProp Σ :=
  own allocMap_name (◯ m).

Definition LiveSetContains `{!allocHistGS Σ} (s : liveset) : iProp Σ :=
  own liveset_name (◯ s).

(* Now we define a VIP-inspired resource algebra, where gen_heapGS is shipped with 
  Iris and gives us the resource of heaps mapping addresses to values *)
Class vipGS_gen Σ := VipGS {
  #[global] VIP_heap :: gen_heapGS Addr Val Σ;
  #[global] VIP_alloc :: allocHistGS Σ;
}.

Notation vipGS := vipGS_gen.
Context `{!vipGS Σ}.

(* Now let's define some useful notation. *)
Notation "l ↦ v" := (pointsto l (DfracOwn 1) v) (at level 20) : bi_scope.

Notation "AllocHistory[@ id ] = ( metadata )" := 
  (AllocMapContains {[ id := to_agree metadata ]})%I (at level 20) : bi_scope.
Notation "AllocHistory[@ id ] = ( metadata , true )" := 
  (AllocMapContains {[ id := to_agree metadata ]} ∗ LiveSetContains {[ id ]})%I
    (at level 20) : bi_scope.
Notation "AllocHistory[@ id ] = ( metadata , false )" := 
  (AllocMapContains {[ id := to_agree metadata ]} ∗ ¬LiveSetContains {[ id ]})%I 
    (at level 20) : bi_scope.

Definition syntax_example1 : iProp Σ := AllocHistory[@1] = (mkAllocMetaData 0%Z 10%Z).
Definition syntax_example2 : iProp Σ := AllocHistory[@1] = (mkAllocMetaData 0%Z 10%Z, true).

Lemma allocmap_combine: ⊢ ∀ (m m' : allocmap), 
  AllocMapContains m -∗ AllocMapContains m' -∗ AllocMapContains (m ⋅ m').
Proof.
  iIntros (m m') "H1 H2".
  iCombine "H1 H2" as "H"; iFrame.
Qed.

(* Ownership predicates *)

(* The Owned predicate is a generic ownership predicate for a pointer to a value.
  It is parameterized by the pointer, the value, and the allocation metadata. *)

Definition Owned (p: Ptr) (v : Z) (min max : Z) (n : nat) (bytes : vec Z n) : iProp Σ := 
  (* Heap assertions *)
    ∃ (l : Loc), ⌜p = Some l⌝           (* Pointer is not null *)
  ∗ ([∗ list] i ↦ x ∈ (to_list bytes),  (* Points-to for each byte **)
      ((addr l) + (Z.of_nat i)) ↦ (Some x)) 
  ∗ ⌜ min ≤ v ∧ v ≤ max ⌝               (* Value is within bounds *)
  (* Provenance assertions *)
  ∗ ∃ (base : Addr) (size : Z),         (* Allocation history entry exists *)
      AllocHistory[@(id l)] = (mkAllocMetaData base size)
  ∗ ⌜ base ≤ (addr l) ∧ (addr l) ≤ base + size ⌝.   (* Address is within bounds *)

(* We instantiate the generic Owned predicate for each integer type. *)
Definition Owned_UChar (p: Ptr) (v : Z) : iProp Σ := 
  Owned p v min_UChar max_UChar 1 (of_list [v]).
Definition Owned_UShort (p: Ptr) (v : Z) : iProp Σ := 
  Owned p v min_UChar max_UChar 2 (UShort_bytes v).
Definition Owned_UInt (p: Ptr) (v : Z) : iProp Σ := 
  Owned p v min_UChar max_UChar 4 (UInt_bytes v).
Definition Owned_ULong (p: Ptr) (v : Z) : iProp Σ := 
  Owned p v min_UChar max_UChar 8 (ULong_bytes v).

(* TODO: ownership of signed integer types*)

(* TODO: implement block ownership (it should look a lot like Owned) *)
Definition Block (p : Ptr) : iProp Σ := ⌜true⌝.

(* TODO: implement shift correctly *)
Definition shift (p: Ptr) (offset : Z) (size : Z) : Ptr := 
  p.

(* TODO: implement arrayshift correctly *)
Definition arrayshift (p: Ptr) (pos : Z) (size : Z) := p.

(* TODO: implement padding correctly *)
Definition padding (p : Ptr) (n : nat) : iProp Σ := ⌜true⌝.

(* Iterated ownership *)
(* TODO: Implement based on upstream decisions about array representation *)

(* Useful lemmas *)

(* Two owned pointers must not be equal *)

Lemma Owned_UChar_neq : ⊢ ∀ (l l' : Ptr) (v1 v2 : Z), 
  Owned_UChar l v1 -∗ Owned_UChar l' v2 -∗ ⌜l ≠ l'⌝.
Proof.
Admitted.

(* TODO: Define and prove the lemma above for every signed/unsigned integer type 
    The proof should mostly resemble the Lean ones *)

(* two copies of ownership for the same pointer must agree on their values *)
Lemma Owned_UChar_eq : ⊢ ∀ (l : Ptr) (v1 v2 : Z), 
  Owned_UChar l v1 -∗ Owned_UChar l v2 -∗ ⌜v1 = v2⌝.
Proof.
Admitted.

(* TODO: Define and prove the lemma above for every signed/unsigned integer type *)

End CN_Lib_Iris.