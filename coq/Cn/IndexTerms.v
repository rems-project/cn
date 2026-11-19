Require Import List.
Require Import Coq.Structures.DecidableType.
Require Import Coq.Structures.DecidableTypeEx.

Require Import BaseTypes.
Require Import Terms.
Require Import Locations.

Import ListNotations.

(* Type aliases *)
Definition t' := term BaseTypes.t.
Definition t := annot BaseTypes.t.

(* Pattern-related types *)
Definition bindings (bt: Type) := list (pattern bt * option (annot bt)).
Definition t_bindings := bindings BaseTypes.t. 

Module IndexTerm_as_MiniDecidableType <: MiniDecidableType :=
  Annot_as_MiniDecidableType BasetTypes_t_as_MiniDecidableType.

Module IndexTerm_as_DecidableType := Make_UDT IndexTerm_as_MiniDecidableType.

(* Defines when a term represents the allocation ID of another term *)
Inductive eq_ (loc:Locations.t): t -> t -> t -> Prop :=
| eq_LC: forall t t' bt bt' l l',
  bt = bt' ->
    eq_ 
      loc 
      (IT _ t bt l) 
      (IT _ t' bt' l') 
      (IT _ (Terms.Binop _ Terms.EQ (IT _ t bt l) (IT _ t' bt' l')) (BaseTypes.Bool _) loc).

(* Logical relation that defines when a list of terms represents equality between two lists *)
Inductive eq_list_rel (loc: Locations.t) : list t -> list t -> list t -> Prop :=
| eq_list_nil: 
    eq_list_rel loc [] [] []
| eq_list_cons: forall x y xs ys eq_terms eq_term,
    eq_ loc x y eq_term ->
    eq_list_rel loc xs ys eq_terms ->
    eq_list_rel loc (x :: xs) (y :: ys) (eq_term :: eq_terms).

Definition and2_ (loc: Locations.t) (t1 t2: t) :=
  IT _ (Terms.Binop _ Terms.And t1 t2) (BaseTypes.Bool _) loc.

Inductive and_ (loc: Locations.t) : list t -> t -> Prop :=
| and_nil: and_ loc [] (IT _ (Terms.Const _ (Terms.Bool true)) (BaseTypes.Bool _) loc)
| and_singleton: forall t, and_ loc [t] t
| and_cons: forall t ts result_t,
    and_ loc ts result_t ->
    and_ loc (t :: ts) (and2_ loc t result_t).

(* Logical relation that defines when a term represents the conjunction of equalities between two lists *)
Inductive eq_and_list_rel (loc: Locations.t) : list t -> list t -> t -> Prop :=
| eq_and_list_rel_intro: forall xs ys eq_terms conj_term,
    eq_list_rel loc xs ys eq_terms ->
    and_ loc eq_terms conj_term ->
    eq_and_list_rel loc xs ys conj_term. 
