(* Instantiation of the CN-exported specification
   using results from the prior theories. *)

Require Import ZArith Bool Lia.
Require Import CN_Lemmas.Gen_Spec.
Require CN_Lemmas.CN_Lib.
Require Import CN_Lemmas.CN_Lib_Iris.
Import CN_Lemmas.Gen_Spec.Types.
Require Import List.
Import ListNotations.
From iris.proofmode Require Import proofmode.


Module Inst.
(* nothing!! *)
Definition Alloc : (Z * Z) -> Prop := fun _ => True.

End Inst.

Module Lemma_Defs := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).

Module Proofs.

(* now prove lemmas *)
Import Lemma_Defs Inst.
Open Scope Z.

Section Lemma_Defs.
Context `{!heapGS_gen Σ}.

Local Notation "⊢ P" := (⊢@{iPropI Σ} P).

Lemma Append_Nil_RList : ⊢ Append_Nil_RList_type.
Proof.
  iPureIntro.
  intros L1 _. 
  induction L1.
  + split; reflexivity.
  + destruct IHL1. 
    split; auto; simpl.
    rewrite H; auto.
Qed.

Lemma Append_Cons_RList : ⊢ Append_Cons_RList_type.
Proof.
  iPureIntro.
  intros L1 z L2 t.
  induction L1; auto.
  destruct IHL1. 
  split; auto; simpl.
  rewrite H; auto.
Qed.

End Lemma_Defs.
End Proofs.

Module InstOK: CN_Lemmas.Gen_Spec.Lemma_Spec(Inst).

  Module L := CN_Lemmas.Gen_Spec.Lemma_Defs (Inst).

  Include Proofs.

End InstOK.