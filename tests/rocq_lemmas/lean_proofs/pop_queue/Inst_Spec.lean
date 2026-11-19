import CN_Lib.CN_Lib
import CN_Lib.CN_Lib_Iris
import CN_Lib.CN_Lib_Iris_Fixpoint
import Gen_Spec.Gen_Spec

open Iris CN_Lib ProofMode Gen_Spec
variable [h : MyHeap hlc GF]

theorem pop_lemma_proof :
  ⊢@{IProp GF} pop_lemma_type :=
by
  unfold pop_lemma_type
  iintro %front %back %x %Q
  iinduction Q with
  | Nil =>
    iintro H %cell Q
    iexists List.Nil; isplitl [H]
    · iapply H
    · iexists cell; isplitl [Q]
      · iapply Q
      · simp; ipureintro; simp
        constructor; cbv; cbv
  | Cons x Q ih =>
    iintro H %cell Q
    iexists (List.Cons x Q); isplitl [H]
    · iapply H
    · iexists cell ; isplitl [Q]; iframe
      isplitr; ipureintro; rfl
      isplitr; ipureintro; rfl
      simp; ipureintro; simp
      constructor; cbv; cbv

instance pop_lemma_inst : @pop_lemma _ _ h where
  pop_lemma_inst := pop_lemma_proof
