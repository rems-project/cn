Require Import ZArith Bool Lia.
From iris.proofmode Require Import proofmode.
From iris.bi.lib Require Import fractional.
From iris.base_logic.lib Require Export gen_heap.

(* Instantiating Iris with a heap *)

Notation Ptr := Z.
Notation Val := (option Z).


Class heapGS_gen Σ := HeapGS {
  heapGS_gen_heapGS :: gen_heapGS Ptr Val Σ;
}.

Notation heapGS := (heapGS_gen).

Notation "l ↦ v" := (pointsto l (DfracOwn 1) v) (at level 20) : bi_scope.

Section CN_Lib_Iris.

Context `{!heapGS Σ}.

(* Own and its various flavours *)

Definition Owned (l: Ptr) (v : Z) : iProp Σ := (l ↦ Some v) ∧ ⌜l ≠ 0⌝.
Definition Block (l :Ptr) : iProp Σ := (l ↦ None) ∧ ⌜l ≠ 0⌝. 

Definition Owned_char (l: Ptr) (v : Z) : iProp Σ := Owned l v.

Definition int_to_bytes (val : Z) : (Z * Z * Z * Z).
Admitted.

Definition Owned_int (l: Ptr) (v : Z) : iProp Σ := 
  let '(b1, b2, b3, b4) := (int_to_bytes v) in
  Owned l b1 ∗
  Owned (l + 1) b2 ∗
  Owned (l + 2) b3 ∗
  Owned (l + 3) b4.

Definition shift (l: Ptr) (offset : Z) (size : Z) := 
  Z.add offset l.

(* padding *)

Definition arrayshift (l: Ptr) (pos : Z) (size : Z) := Z.add (Z.mul pos size) l.

Fixpoint padding (l : Ptr) (n : nat) := 
  match n with
  | 0 => (l ↦ None)%I
  | S n' => (Block l ∗ padding (l + 1) n')%I
  end.
 
(* Iterated ownership *)

Fixpoint each_int (j n : nat) (p : Ptr) (l : list Z) : iProp Σ :=
  (match n with
    | O => ⌜l = []⌝ ∗ emp%I
    | S n' => (match l with
      | cons x xs => (Owned_int (arrayshift p 4 j) x ∗ 
        each_int (S j) n' p xs)%I
      | nil => False%I
      end) 
  end).

(* Useful lemmas *)

(* owned pointers can't be null *)
Lemma ptr_not_null : ⊢ ∀ (l : Ptr) (v : Z), Owned l v -∗ ⌜l ≠ 0⌝.
Proof.
  iIntros (l v) "H".
  iDestruct "H" as "[_ H]"; auto.
Qed.

(* the above, but with owned_int *)
Lemma ptr_not_null_int : ⊢ ∀ (l : Ptr) (v : Z), Owned_int l v -∗ ⌜l ≠ 0⌝.
Proof.
  iIntros (l v) "H".
  unfold Owned_int.
  destruct (int_to_bytes v).
  destruct p; destruct p.
  iDestruct "H" as "[[_ H] _]".
  iFrame.
Qed.

(* two owned pointers must be unequal*)
Lemma owned_neq : ⊢ ∀ (l l' : Ptr) (v1 v2 : Z), 
  Owned l v1 -∗ Owned l' v2 -∗ ⌜l ≠ l'⌝.
Proof.
  iIntros (l l' v1 v2) "[H1 _] [H2 _]".
  iApply (pointsto_ne with "H1 H2").
Qed.

(* the above, but for owned_char *)
Lemma owned_char_neq : ⊢ ∀ (l l' : Ptr) (v1 v2 : Z), 
  Owned_char l v1 -∗ Owned_char l' v2 -∗ ⌜l ≠ l'⌝.
Proof.
  iIntros (l l' v1 v2) "H1 H2".
  unfold Owned_char.
  iApply (owned_neq with "H1 H2").
Qed.

(* the above, but for owned_int *)
Lemma owned_int_neq : ⊢ ∀ (l l' : Ptr) (v1 v2 : Z), 
  Owned_int l v1 -∗ Owned_int l' v2 -∗ ⌜l ≠ l'⌝.
Proof.
  iIntros (l l' v1 v2) "H1 H2". 
  unfold Owned_int in *.
  destruct (int_to_bytes v1); destruct p; destruct p.
  destruct (int_to_bytes v2); destruct p; destruct p.
  iDestruct "H1" as "[H1 _]".
  iDestruct "H2" as "[H2 _]".
  iApply (owned_neq with "H1 H2").
Qed.

(* two copies of ownership for the same pointer must agree on their values *)
Lemma owned_eq : ⊢ ∀ (l : Ptr) (v1 v2 : Z), 
  Owned l v1 -∗ Owned l v2 -∗ ⌜v1 = v2⌝.
Proof.
  iIntros (l v1 v2) "[H1 _] [H2 _]".
  iPoseProof (pointsto_agree with "H1 H2") as "%H".
  iPureIntro; set_solver.
Qed.

(* the above, but for owned_char *)
Lemma owned_eq_char : ⊢ ∀ (l : Ptr) (v1 v2 : Z), 
  Owned_char l v1 -∗ Owned_char l v2 -∗ ⌜v1 = v2⌝.
Proof.
  iIntros (l v1 v2) "H1 H2".
  unfold Owned_char.
  iApply (owned_eq with "H1 H2").
Qed.

(* the above but for owned_int is omitted here because 
  ints_to_bytes is undefined *)

End CN_Lib_Iris.