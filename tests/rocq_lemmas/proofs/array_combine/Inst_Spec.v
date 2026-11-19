(* Instantiation of the CN-exported specification
   using results from the prior theories. *)

Require Import ZArith Bool Lia Coq.ZArith.Znat Coq.Lists.List.
Require Import CN_Lemmas.Gen_Spec.
Require Import CN_Lemmas.CN_Lib_Iris.
Import CN_Lemmas.Gen_Spec.Types.
From iris.proofmode Require Import proofmode.
Require Import iris.base_logic.lib.gen_heap.
Require Import Coq.Classes.RelationClasses.



Module Inst.

  Definition Alloc : (Z * Z) -> Prop := fun x => True.
  
End Inst.

Module Lemma_Defs := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).

Module Proofs.

(* now prove lemmas *)
Import Lemma_Defs Inst.
Open Scope Z.

Module Defs := CN_Lemmas.Gen_Spec.Defs (Inst).
Import Defs.

Section Iris_time.
Context `{!heapGS_gen Σ}.
Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

Lemma each_lemma : ⊢ each_lemma_type.
Proof.
  unfold each_lemma_type.
  unfold each_int.
  iIntros (p n v) "H".
  iIntros (l) "H1 %leq %gte".
  iExists (v :: l).
  iSplitL; auto.
  assert ((Z.to_nat n - Z.to_nat 0)%nat = Z.to_nat n).
  { lia. }
  rewrite H.
  assert (Z.to_nat n ≠ O).
  { lia. }
  destruct (Z.to_nat n); auto.
  iFrame.
  assert ((Z.to_nat 1) = S (Z.to_nat 0)).
  { lia. }
  rewrite H1.
  unfold each_int.
  - assert ((S n0 - S (Z.to_nat 0))%nat = n0).
  { lia. }
  rewrite H2.
  iFrame.
Qed.

Lemma each_concrete2 : ⊢ each_concrete2_type.
Proof.
  unfold each_concrete2_type.
  iIntros (p v) "H".
  iIntros (L) "H1".
  iExists (v :: L).
  simpl.
  iFrame.
  destruct L; auto.
  assert ((arrayshift (arrayshift p 4 1) 4 (Z.to_nat 0)) = (arrayshift p 4 (S (Z.to_nat 0)))).
  { unfold arrayshift.
    lia. }
  rewrite H.
  iDestruct "H1" as "[H H']".
  iFrame.
  assert ((arrayshift (arrayshift p 4 1) 4 (S (Z.to_nat 0))) = arrayshift p 4 (S (S (Z.to_nat 0)))).
  { unfold arrayshift.
    lia. }
  rewrite H0.
  iFrame.
Qed.
  
Lemma n_obv (n : Z) : (Z.to_nat n - Z.to_nat 0)%nat = Z.to_nat n.
Proof.
  lia.
Qed.

Lemma n_zero_obv (z : Z) (n : nat) : (Z.to_nat z + n - 0)%nat = (Z.to_nat z + n)%nat.
Proof.
  lia.
Qed.

Lemma n_add (n m : Z) (Hn : 0 ≤ n) (Hm : 0 ≤ m) :
  (Z.to_nat (n + m)) = (Z.to_nat n + Z.to_nat m)%nat.
Proof.
  destruct n.
  - lia.
  - destruct m.
    + lia.
    + rewrite Z2Nat.inj_add; auto.
    + contradiction.
  - contradiction.
Qed.

Lemma each_emp (m n : nat) (p : Ptr) (L : list Z) :
  each_int m (n - n) p L -∗
  ⌜L = []⌝.
Proof.
  iIntros "H".
  unfold each_int in *.
  induction n.
  - simpl.
    iDestruct "H" as "[%H _]".
    rewrite H; auto.
  - simpl.
    apply IHn.
Qed.

Lemma each_shift_concrete (p : Ptr) (x : Z) (L : list Z) :
  each_int 0 (2%nat) p (L ++ [x]) -∗
  each_int 0 1%nat p L.
Proof.
  iIntros "H".
  destruct L.
  - simpl.
    iDestruct "H" as "[_ %H]".
    auto.
  - simpl.
    iDestruct "H" as "[H' H]".
    iFrame.
    destruct L.
    + simpl; auto.
    + simpl.
      iDestruct "H" as "[_ %H]".
      exfalso.
      inversion H.
      induction L.
      * simpl in H0.
        inversion H0.
      * simpl in H0.
        inversion H0.
Qed.

Lemma each_concrete : ⊢ each_concrete_type.
  Proof.
  unfold each_concrete_type.
  iIntros (p L) "H".
  iIntros (v) "H1".
  iExists (L ++ [v]).
  rewrite (n_obv).
  rewrite (n_obv).
  iSplitL; auto.
  simpl.
  destruct L; auto.
  simpl.
  iDestruct "H" as "[H H']".
  iFrame.
  destruct L; auto.
  simpl.
  iDestruct "H'" as "[H' [%H2 _]]".
  iFrame.
  rewrite H2.
  simpl.
  auto.
Qed.

(*TODO*)
End Iris_time.
End Proofs.

Module InstOK: CN_Lemmas.Gen_Spec.Lemma_Spec(Inst).

  Module L := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).

  Include Proofs.

End InstOK.

