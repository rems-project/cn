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

(* This isn't proveable! *)

Lemma push_lemma : ⊢ push_lemma_type.
Proof.
  unfold push_lemma_type.
  iIntros (front last [H1 | H2] Q).
  - iIntros "H3" (P) "H4 H5".
    iSplitR.
    + iPureIntro.
      lia.
    + iExists (D.Snoc Q (D.first P)).
      iSplitL; auto.
      rewrite H1.
      iEval (iApply queue_aux_nil) in "H3".
      iDestruct "H3" as %H3.
      rewrite H3.
      unfold D.Snoc.
      unfold D.QueueAux.
      iRight.
      iSplitR; iSimpl; auto.
      iSplitR.
      * admit.
      * admit.
Admitted.


(*TODO*)
End Iris_time.
End Proofs.

Module InstOK: CN_Lemmas.Gen_Spec.Lemma_Spec(Inst).

  Module L := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).

  Include Proofs.

End InstOK.

