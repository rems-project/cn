Require Import ZArith Bool.
Require Import CN_Lemmas.Gen_Spec CN_Lemmas.CN_Lib_Iris.
From iris.proofmode Require Import proofmode.

Module Types := CN_Lemmas.Gen_Spec.Types.
Import Types.

Module Inst.
End Inst.

Module InstOK : CN_Lemmas.Gen_Spec.Lemma_Spec (Inst).
  Module L := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).
  Module Preds := L.R.
  Import Preds L.
  Open Scope Z.

  Section Proof.
  Context `{!heapGS_gen Σ}.

  Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

  Lemma ListSeg_List : ⊢ ListSeg_List_type.
  Proof.
    iIntros (p q segment) "Hsegment".
    iIntros (suffix) "Hsuffix".
    iPoseProof
      (ListSeg_induction
         (fun p q _ => ∀ suffix, List q suffix -∗ ∃ whole, List p whole)%I
         with "[]") as "#Hind".
    { iIntros "!>" (p' q' segment') "Hbody".
      iIntros (suffix') "Hsuffix'".
      iDestruct "Hbody" as "[Hnil | Hcons]".
      - iDestruct "Hnil" as "[%Hpq _]".
        unfold Preds.D.ptr_eq in Hpq.
        apply Is_true_eq_true in Hpq.
        apply Z.eqb_eq in Hpq.
        subst q'.
        iExists suffix'.
        iFrame.
      - iDestruct "Hcons" as "[_ Hcons]".
        iDestruct "Hcons" as "[_ Hcons]".
        iDestruct "Hcons" as "[%Hp_nonnull Hnodes]".
        iDestruct "Hnodes" as (node) "[Hnode Htail]".
        iDestruct "Htail" as (tail) "[IH _]".
        iDestruct ("IH" with "Hsuffix'") as (whole_tail) "Htail".
        iExists (Cons (Types.value node) whole_tail).
        iApply List_unfold.
        iRight.
        iSplit; first done.
        iSplit; first done.
        iExists node.
        iSplitL "Hnode"; first iExact "Hnode".
        iExists whole_tail.
        iFrame.
        done. }
    iSpecialize ("Hind" $! p q segment).
    iSpecialize ("Hind" with "Hsegment").
    iDestruct ("Hind" $! suffix with "Hsuffix") as (whole) "Hwhole".
    iExists whole.
    iFrame.
  Qed.

  End Proof.
End InstOK.
