import CN_Lib.CN_Lib_Iris
import CN_Lib.CN_Lib_Iris_Fixpoint
import Gen_Spec.Gen_Spec

open Iris CN_Lib ProofMode Gen_Spec
variable [h : MyHeap hlc GF]

theorem ListSeg_List_proof :
  ⊢@{IProp GF} ListSeg_List_type :=
by
  unfold ListSeg_List_type
  iintro %p %q %segment Hsegment %suffix Hsuffix
  ihave Hind := (ListSeg_induction (fun p q _ => iprop%
    ∀ suffix, List q suffix -∗ ∃ whole, List p whole)) $$ []
  · imodintro
    iintro %p' %q' %segment' Hbody
    iintro %suffix' Hsuffix'
    unfold ListSeg_body
    icases Hbody with (Hnil | Hcons)
    · icases Hnil with ⟨%Hpq , _⟩
      unfold ptr_eq at Hpq
      simp at Hpq
      subst q'
      iexists suffix'
      iframe
    · icases Hcons with ⟨_ , ⟨_ , ⟨%Hp_nonnull , %node, ⟨Hnode ,  %tail, ⟨IH , _⟩⟩⟩⟩⟩
      icases IH $$ Hsuffix' with ⟨%whole_tail , Htail⟩
      iexists (list_cn.Cons node.value whole_tail)
      iapply List_unfold
      unfold List_body
      iright
      isplit; itrivial
      isplit; itrivial
      iexists node
      isplitl [Hnode]; iexact Hnode
      iexists whole_tail
      iframe
      ipureintro; rfl
  ispecialize Hind $$ %p %q %segment Hsegment
  icases Hind $$ %suffix Hsuffix with ⟨%whole , Hwhole⟩
  iexists whole; iframe

instance ListSeg_List_inst : @ListSeg_List _ _ h where
  ListSeg_List_inst := ListSeg_List_proof
