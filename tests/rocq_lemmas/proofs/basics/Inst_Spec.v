Require Import CN_Lemmas.Gen_Spec.
Require CN_Lemmas.CN_Lib.
Require Import CN_Lemmas.CN_Lib_Iris.
Import CN_Lemmas.Gen_Spec.Types.
From iris.proofmode Require Import proofmode.

Module Inst.
End Inst.

Module InstOK: CN_Lemmas.Gen_Spec.Lemma_Spec(Inst).

  Module L := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).

  Section Proof.
    Import L.
    Context `{!heapGS_gen Σ}.
    Local Notation "⊢ P" := (⊢@{iPropI Σ} P).
    Lemma data_option_trivial : ⊢ data_option_trivial_type.
    Proof.
      iIntros (p).
      done.
    Qed.
  End Proof.
End InstOK.