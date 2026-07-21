(* Instantiation of the CN-exported specification
   using results from the prior theories. *)

Require Import ZArith Bool Lia.
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


Lemma queue_aux_nil : ⊢ ∀ (f : Ptr) (L : List),
  D.QueueAux f f L -∗ ⌜ L = Nil⌝ .
Proof.
  iIntros (f L) "H".
  destruct L. 
  - iPureIntro; auto.
  - unfold D.QueueAux.
    iDestruct "H" as "[H1 | H2]".
    + iDestruct "H1" as %H1.
      iPureIntro.
      exfalso.
      destruct H1; discriminate.
    + iExFalso.
      iDestruct "H2" as "[_ [ H _ ]]".
      iDestruct "H" as %H; easy.
Admitted.

Lemma queue_aux_nil_2 : ⊢ ∀ (f f' : Ptr) (L : List),
D.QueueAux f f' L -∗ ⌜ L = Nil -> f = f'⌝ .
Proof.
  iIntros (f f' L) "H".
  destruct L.
  - unfold D.QueueAux.
    iDestruct "H" as "[H1 | H2]".
    + iDestruct "H1" as %H1.
      iPureIntro.
      destruct H1; auto.
    + iExFalso.
      iDestruct "H2" as "[_ [ _ [%x [ _ [_ [_ [%y [_ HP]]]]]]]]".
      iDestruct "HP" as %H2.
      discriminate.
  - iPureIntro.
    intros P.
    discriminate.
Admitted.

Lemma queue_aux_nil_3 : ⊢ ∀ (f f' : Ptr),
  ⌜f = f'⌝ -∗
  D.QueueAux f f' Nil.
Proof.
  iIntros (f f') "H".
  iLeft; auto.
Admitted.
 
Lemma queue_aux_false : ⊢ ∀ (f f' : Ptr) (L : List),
D.QueueAux f f' L -∗ ⌜ L = Nil -> (f ≠ f' -> False) ⌝.
  iIntros (f f' L) "H".
  induction L.
  - iEval (iApply queue_aux_nil_2) in "H".
    iDestruct "H" as %H.
    iPureIntro.
    destruct H; auto.
  - iPureIntro.
    intros H2.
    discriminate.
Qed.

Lemma owned_queue_cell_unique : ⊢ ∀ (f f' : Ptr) (C C' : D.queue_cell),
  D.Owned_queue_cell f C -∗ D.Owned_queue_cell f' C' -∗ ⌜ f ≠ f'⌝ .
Proof.
  unfold D.Owned_queue_cell.
  iIntros (f f' C C') "[H1 _] [H2 _]".
  iApply (owned_int_neq with "H1 H2").
Qed.

Lemma front_last_neq : ⊢ ∀ (f f' f'' : Ptr) (c : D.queue_cell) (H : f ≠ f') (L : List) ,
  D.QueueAux f f' L -∗ D.Owned_queue_cell f'' c
-∗ ⌜ f ≠ f''⌝ .
Proof.
  iIntros (f f' f'' c H L) "H1 H2".
  unfold D.QueueAux.
  induction L.
  - iDestruct "H1" as "[H1 | H1]".
    + iDestruct "H1" as %H1.
      destruct H1.
      contradiction.
    + iDestruct "H1" as "[_  [_ [%x [_ [_ [_ [%y [_ H6]]]]]]]]".
      iDestruct "H6" as %H6.
      discriminate.
  - iDestruct "H1" as "[H1 | H1]".
    + iDestruct "H1" as %H1.
      destruct H1.
      contradiction.
    + iDestruct "H1" as "[_ [_ H8]]".
      iDestruct "H8" as "[%x [H8 [H9 [H10 [%y [H11 %pat]]]]]]".
      iApply (owned_queue_cell_unique $! f f'' x c with "H8 H2").
Admitted.

Lemma QueueAux_Snoc (front second_last last : Ptr) (Second_last Last : D.queue_cell)(Q : List) (H : D.next Second_last = last) (H2: second_last ≠ last) (H3: front ≠ last)(H4: front ≠ second_last) (HNext: D.next Second_last ≠ 0):
   ⊢ D.QueueAux front second_last Q -∗ D.Owned_queue_cell second_last Second_last -∗
   D.Owned_queue_cell last Last -∗
    D.QueueAux front last (D.Snoc Q (D.first Second_last)) ∗
   ∃ Last2 : D.queue_cell,
     D.Owned_queue_cell last Last2
     ∧ ⌜D.Snoc Q (D.first Second_last) =
        D.Snoc Q (D.first Second_last)⌝ ∧ ⌜Last = Last2⌝ ∧ emp.
Proof.
  iIntros "H1 H2 H3".
  iInduction Q as [] "IH" forall (front H3 H4).
  - iEval (iApply queue_aux_false) in "H1".
    iDestruct "H1" as %H1.
    destruct H1; auto.
  - cbn.
    iDestruct "H1" as "[H1 | H1]".
    + iDestruct "H1" as %H1.
      destruct H1; contradiction.
    + iDestruct "H1" as "[_ [_ H8]]".
      iDestruct "H8" as "[%Front [H8 [H9 [H10 [%y [H11 %pat]]]]]]".
      inversion pat.
      iAssert (⌜D.next Front ≠ last⌝)%I as %S.
      { iDestruct "H10" as "[%H10 | %H10]".
        - inversion H10; auto.
          iPureIntro; lia.
        - iPoseProof (front_last_neq $! (D.next Front) second_last last Last H10 with "H11 H3") as "H12"; auto.
        } 
      assert (D.next Front = last \/ D.next Front ≠ last).
      lia.
      iDestruct "H10" as "[%H10 | %H10]".
      * inversion H10; auto.
        iSplitR "H3"; last (iExists Last; iFrame; auto).
        iRight.
        iSplitR; auto.
        iSplitR; auto.
        iExists Front; iFrame.
        iSplitR; first (iPureIntro; lia).
        iExists (D.Snoc y (D.first Second_last)).
        iClear "IH".
        iPoseProof (queue_aux_nil $! (D.next Front) y with "H11") as "%HNil".
        rewrite HNil.
        iSplitL; last (auto); iSimpl.
        iRight.
        iSplitR; auto.
        iSplitR; auto.
        iExists Second_last; iFrame.
        iSplitR; first (iPureIntro; auto).
        iSplitR; first (iPureIntro; lia).
        iExists Nil.
        iPoseProof (queue_aux_nil_3 $! (D.next Second_last) last H) as "HNil2".
        iSplitR; last (auto).
        iApply "HNil2".
      * destruct H0. inversion H0.
        ** contradiction.
        ** iDestruct ("IH" $! (D.next Front) S H10 with "H11 H2 H3") as "[V [%Last' (C & %C')]]".
          iSplitR "C".
          ++ iRight.
          iSplitR; auto.
          iSplitR; auto.
          iExists Front.
          iFrame.
          iPureIntro; set_solver.
          ++ iExists Last'.
            iFrame.
            iPureIntro; set_solver.
Admitted.

Lemma push_induction : ⊢ push_induction_type.
  unfold push_induction_type.
  iIntros (front second_last last [H1 | H2] Q).
  - iIntros "H3" (Second_last) "H4 H5".
    iIntros (Last) "H6".
    iDestruct "H5" as %H5.
    iAssert (⌜D.next Second_last ≠ 0⌝)%I as %H.
        { rewrite H5.
          iDestruct "H6" as "[HNeqLast _]".
          iApply (ptr_not_null_int with "HNeqLast").
        }
    iSplitR.
    + iPureIntro.
      lia.
    + rewrite H1.
      iExists (D.Snoc Q (D.first Second_last)).
      iEval (iApply queue_aux_nil) in "H3".
      iDestruct "H3" as %H3.
      iAssert (⌜second_last ≠ last⌝)%I as %P.
      iPoseProof (owned_queue_cell_unique $! second_last last Second_last Last with "H4 H6") as "H3"; auto.
      rewrite H3.
      iSplitR "H6"; auto.
      unfold D.Snoc.
      unfold D.QueueAux.
      iRight.
      iSplitR; iSimpl; auto.
      iSplitR.
      * iPureIntro.
        auto.
      * iExists Second_last.
        iSplitL "H4"; auto.
        iSplitR; auto.
        -- iSplitR.
          ++ iPureIntro.
             lia.
          ++ iExists Nil.
            iSplitL; auto.
  - iIntros "H3" (Second_last) "H4 H5".
    iIntros (Last) "H6".
    iDestruct "H5" as %H5.
    iAssert (⌜D.next Second_last ≠ 0⌝)%I as %H.
        { rewrite H5.
          iDestruct "H6" as "[HNeqLast _]".
          iApply (ptr_not_null_int with "HNeqLast").
        }
    iSplitR.
    + iPureIntro.
      lia.
    + iExists (D.Snoc Q (D.first Second_last)).
      iAssert (⌜second_last ≠ last⌝)%I as %P.
      iPoseProof (owned_queue_cell_unique $! second_last last Second_last Last with "H4 H6") as "H7"; auto.
      iAssert (⌜front ≠ last⌝)%I as %R.
      iPoseProof (front_last_neq $! front second_last last Last H2 with "H3 H6") as "%H7"; auto.
      iApply (QueueAux_Snoc with "H3 H4 H6"); auto.
Admitted.
          
End Iris_time.
End Proofs.

Module InstOK: CN_Lemmas.Gen_Spec.Lemma_Spec(Inst).

  Module L := Lemma_Defs.

  Include Proofs.

End InstOK.

