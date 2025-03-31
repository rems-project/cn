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
  List.fold_left (fun s c => ResSet.add c s) l ResSet.empty.

Definition set_from_list_r (l : list t) : ResSet.t :=
  List.fold_right (fun c s => ResSet.add c s) ResSet.empty l.

Lemma ResSet_In_List_In_eq_r: forall r rs,
  ResSet.In r (Resource.set_from_list_r rs) <-> List.In r rs.
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

Lemma set_from_list_r_eq: forall l,
  ResSet.Equal (set_from_list l) (set_from_list_r l).
Proof.
  intros l.
  unfold set_from_list, set_from_list_r.
  rewrite <- fold_left_rev_right.
  unfold ResSet.Equal.
  intros a; split; intros H.
  - apply ResSet_In_List_In_eq_r in H.
    apply ResSet_In_List_In_eq_r, in_rev, H.
  - apply ResSet_In_List_In_eq_r, in_rev in H.
    apply ResSet_In_List_In_eq_r, H.
Qed.

Lemma ResSet_In_List_In_eq: forall r rs,
  ResSet.In r (Resource.set_from_list rs) <-> List.In r rs.
Proof.
  intros r rs.
  split; intros H.
  - apply set_from_list_r_eq in H.
    apply ResSet_In_List_In_eq_r, H.
  - apply set_from_list_r_eq.
    apply ResSet_In_List_In_eq_r, H.
Qed.
