Require Import Coq.Structures.DecidableType.
Require Import Coq.Structures.DecidableTypeEx.
Require Import Coq.FSets.FSetWeakList.
Require Import Coq.FSets.FSetDecide.

Require Import Request.
Require Import IndexTerms.

Inductive output : Type := | O: IndexTerms.t -> output.

Definition predicate : Type := Request.Predicate.t * output.

Definition qpredicate : Type := Request.QPredicate.t * output.

Definition t : Type := Request.t * output.

Module Output_as_MiniDecidableType <: MiniDecidableType.
  Definition t := output.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    unfold eq.
    intros [o1] [o2].
    decide equality.
    apply IndexTerm_as_MiniDecidableType.eq_dec.
  Defined.
End Output_as_MiniDecidableType.

Module Resource_as_MiniDecidableType <: MiniDecidableType.
  Definition t := t.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    intros r1 r2.
    apply BaseTypes.prod_eq_dec.
    - apply Request_as_MiniDecidableType.eq_dec.
    - apply Output_as_MiniDecidableType.eq_dec.
  Defined.
End Resource_as_MiniDecidableType.

Module Output_as_DecidableType := Make_UDT Output_as_MiniDecidableType.
Module Resource_as_DecidableType := Make_UDT Resource_as_MiniDecidableType.
Module ResSet := FSetWeakList.Make Resource_as_DecidableType.
Module ResSetDecide := FSetDecide.WDecide(ResSet).

Definition set_from_list (l : list t) : ResSet.t :=
  List.fold_right (fun c s => ResSet.add c s) ResSet.empty l.

Lemma ResSet_In_List_In_eq: forall r rs,
  ResSet.In r (Resource.set_from_list rs) <-> List.In r rs.
Proof.
  intros r rs.
  induction rs.
  { split; intros H; inversion H. }
  cbn.
  split; intros H.
  - apply ResSetDecide.F.add_iff in H.
    destruct H as [H | H].
    + left; apply H.
    + right; apply IHrs, H.
  - apply ResSetDecide.F.add_iff.
    destruct H as [H | H].
    + left; apply H.
    + right; apply IHrs, H.
Qed.
