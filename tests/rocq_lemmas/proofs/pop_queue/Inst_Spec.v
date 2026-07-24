(* Instantiation of the CN-exported specification
   using results from the prior theories. *)

Require Import ZArith Bool Lia.
Require Import CN_Lemmas.Gen_Spec.
Require Import CN_Lemmas.CN_Lib_Iris.
Import CN_Lemmas.Gen_Spec.Types.
From iris.proofmode Require Import proofmode.



Module Inst.

  Definition Alloc : (Z * Z) -> Prop := fun x => True.
  
End Inst.

Module Lemma_Defs := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).

Module Proofs.

(* now prove lemmas *)
Import Lemma_Defs Inst.
Open Scope Z.

Section Iris_time.
Context `{!heapGS_gen Σ}.
Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

Lemma pop_lemma : ⊢ pop_lemma_type.
Proof.
  iIntros (front back x Q).
  induction Q.
  - iIntros "H" (cell) "Q".
    iExists Nil.
    iSplitL "H".
    + iApply "H".
    + iExists cell.
      iSplitL "Q".
      * iApply "Q".
      * iSplitR.
        -- iPureIntro.
          auto.
        -- iPureIntro.
          auto.
  - iIntros "H" (cell) "Q".
    iExists (Cons z Q).
    iSplitL "H".
    + iApply "H".
    + iExists cell.
      iSplitL "Q".
      * iApply "Q".
      * iSplitR.
        -- iPureIntro.
          auto.
        -- iPureIntro.
          auto.
Qed.
    

(*TODO*)
End Iris_time.
End Proofs.

Module InstOK: CN_Lemmas.Gen_Spec.Lemma_Spec(Inst).

  Module L := Lemma_Defs.

  Include Proofs.

End InstOK.

