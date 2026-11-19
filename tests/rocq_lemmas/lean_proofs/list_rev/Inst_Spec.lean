import CN_Lib.CN_Lib_Iris
import CN_Lib.CN_Lib_Iris_Fixpoint
import Gen_Spec.Gen_Spec

open Iris CN_Lib ProofMode Gen_Spec
variable [h : MyHeap hlc GF]

theorem Append_Nil_RList_proof :
  ⊢@{IProp GF} Append_Nil_RList_type :=
by
  unfold Append_Nil_RList_type
  ipureintro
  intros L1 _
  induction L1 with
  | Nil =>
    constructor; rfl; trivial
  | Cons H T ih =>
    rcases ih with ⟨ih , _⟩
    constructor
    · simp [Gen_Spec.Append]; exact ih
    · trivial

theorem Append_Cons_RList_proof :
  ⊢@{IProp GF} Append_Cons_RList_type :=
by
  unfold Append_Cons_RList_type
  ipureintro
  intros L1 n L2 _
  induction L1 with
  | Nil => simp [Gen_Spec.Append, Snoc]
  | Cons H T ih =>
    rcases ih with ⟨ih , _⟩
    simp [Gen_Spec.Append, Snoc]; exact ih

instance Append_Nil_RList_inst : @Append_Nil_RList _ _ h where
  Append_Nil_RList_inst := Append_Nil_RList_proof

instance Append_Cons_RList_inst : @Append_Cons_RList _ _ h where
  Append_Cons_RList_inst := Append_Cons_RList_proof
