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
  
Lemma n_obv (n : Z) : (Z.to_nat n - Z.to_nat 0)%nat = Z.to_nat n.
Proof.
  lia.
Qed.

Lemma n_obv' (n : Z) : (Z.to_nat n + Z.to_nat 0)%nat = Z.to_nat n.
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

Lemma empty_m (p : Ptr) (m n : nat) :
  each_int n m p [] -∗
  ⌜m = O⌝.
Proof.
  iIntros "H".
  induction m.
  - auto.
  - simpl; auto.
Qed.

Lemma succ_add (m n : nat) :
  m + S n = S m + n.
Proof.
  lia.
Qed.

Lemma succ_n (n : nat) :
  S (Z.to_nat n) = S n.
Proof.
  destruct n; simpl.
  - auto.
  - lia.
Qed.

Lemma to_nat_idem (n : nat):
  Z.to_nat n = n.
Proof.
  lia.
Qed.

Lemma each_int_emp (p : Ptr) (n m : nat) :
  each_int n m p [] -∗
  ⌜m = O⌝.
Proof.
  iIntros "H".
  induction m.
  - auto.
  - simpl; auto.
Qed.

Lemma pers_intro (P Q : iProp Σ) `{!Persistent P} : P ∗ Q ⊢ □ P.
Admitted.

Lemma help (P : list Z -> iProp Σ): 
  (□ P []) -∗ 
  (∀ (x:Z) (l:list Z), P l -∗ P (l ++ [x])) -∗ 
  ∀ (l:list Z), P l.
 Admitted.

Lemma len_lemma (m n : nat) (p : Ptr) (L : list Z) :
  each_int m n p L -∗
  ⌜n = length L⌝.
Proof.
  Admitted.

Lemma owned_each_combine (p : Ptr) (n : nat) (L : list Z) (v : Z) (Hn : 0 ≤ n) :
  each_int O n p L -∗
  Owned_int (arrayshift p 4 n) v -∗
  each_int O (S n) p (L ++ [v]).
Proof.
  iIntros "H1 H2".

  iInduction n as [] "IH" forall (L).
  - simpl.
    iDestruct "H1" as "[%H1 _]".
    rewrite H1; simpl; auto.
  - destruct L.
    + simpl.
      auto.
    + cbn.
      iDestruct "H1" as "[H1 H1']".
      iFrame.
      admit.
Admitted.

Lemma each_combine : ⊢ each_combine_type.
Proof.
  unfold each_combine_type.
  iIntros (p n m L1) "H1".
  iIntros (L2) "H2 %Hn1 %Hn2 %Hm1 %Hm2 %Hmn".
  iExists (L1 ++ L2).
  iSplitL; auto. 
  rewrite n_obv.
  rewrite n_obv.
  rewrite CN_Lib.wrapI_idem; last (lia).
  rewrite n_add; auto.
  - assert ((Z.to_nat m + Z.to_nat n - Z.to_nat n) = Z.to_nat m)%nat.
    {  lia. }
    rewrite H.
    clear Hmn H Hn2 Hm1 Hm2.
    generalize (Z.to_nat n).
    intro n'.
    iInduction (Z.to_nat m) as [] "IH" forall (L1 L2 n').
    + admit.
    + destruct L2.
     * admit.
     * iSpecialize ("IH" $! (L1 ++ [z]) L2 (S n')).
     assert ((L1 ++ z :: L2) = ((L1 ++ [z]) ++ L2)).
      { rewrite <- app_assoc. 
        rewrite <- app_comm_cons.
        simpl; auto. }
      rewrite H.
      assert ((n0 + S n')%nat = (S n0 + n')%nat).
      admit.
      rewrite <- H0.
      iEval (cbn) in "H2".
      iDestruct "H2" as "[H2 H2']".
      iPoseProof(owned_each_combine with "H1 H2" ) as "H4".
      lia.
      iSpecialize ("IH" with "H4 H2'").
      auto.

    (*iInduction L2 as [] "IH1" forall (p m n L1 Hn1).
    + iPoseProof (each_int_emp with "H2") as "%H2".
      rewrite H2; rewrite app_nil_r; auto.
    + destruct (Z.to_nat m); auto.
    { simpl.
      iDestruct "H2" as "[%H2 _]".
      rewrite H2; simpl; rewrite app_nil_r; auto. }
      iSpecialize ("IH1" $! p n0 (n + 1) (L1 ++ [a])).
      assert ((L1 ++ a :: L2) = ((L1 ++ [a]) ++ L2)).
      { rewrite <- app_assoc. 
        rewrite <- app_comm_cons.
        simpl; auto. }
      rewrite H.
      assert ((S n0 + Z.to_nat n)%nat = (Z.to_nat n0 + Z.to_nat (n + 1))%nat).
      rewrite n_add; last (lia); auto.
      assert ((Z.to_nat n + Z.to_nat 1)%nat = S (Z.to_nat n)).
        { lia. }
      rewrite H0.
      rewrite to_nat_idem; auto.
      rewrite H0.
      iDestruct "H2" as "[H2 H2']".
      fold each_int.
      iPoseProof (owned_each_combine with "H1 H2") as "H3".
      lia.
      iAssert (⌜0 ≤ n + 1⌝%I) as "H4".
      { iPureIntro; lia. }
      assert (Z.to_nat (Z.to_nat n + 1) = Z.to_nat (n + 1)).
      { rewrite n_add.
        rewrite to_nat_idem.
        rewrite n_add; auto.
        lia.
        lia.
        lia. }
      rewrite H1.
      rewrite to_nat_idem.
      assert (S (Z.to_nat n) = Z.to_nat (n + 1)).
      { rewrite n_add; auto.
        lia.
        lia. }
      rewrite H2.
      iSpecialize ("IH1" with "H4 H3 H2'").
      auto.
  - admit.*)
Admitted.

(*TODO*)
End Iris_time.
End Proofs.

Module InstOK: CN_Lemmas.Gen_Spec.Lemma_Spec(Inst).

  Module L := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).

  Include Proofs.

End InstOK.

