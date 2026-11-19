Require Import List.
Require Import String.

Require Import Coq.Structures.DecidableType.
Require Import Coq.Structures.DecidableTypeEx.

Require Import BaseTypes.
Require Import IndexTerms.
Require Import Cerberus.Symbol.
Require Import Locations.
Require Import SCtypes.
Require Import Sym.
Inductive init : Type :=
  | Init
  | Uninit.

Inductive name : Type :=
  | Owned : SCtypes.t -> init -> name 
  | PName : sym -> name.

Module Predicate.
  Record t : Type := mk {
    name : name;
    pointer : IndexTerms.t;
    iargs : list IndexTerms.t
  }.
End Predicate.

Module QPredicate.
  Record t : Type := mk {
    name : name;
    pointer : IndexTerms.t;
    q : Symbol.t * BaseTypes.t;
    q_loc : Locations.t;
    step : SCtypes.ctype;
    permission : IndexTerms.t;
    iargs : list IndexTerms.t
  }.
End QPredicate.

Inductive request_t : Type :=
  | P : Predicate.t -> request_t
  | Q : QPredicate.t -> request_t.

Definition t := request_t. 

Inductive subsumed : name -> name -> Prop :=
  | Subsumed_equal : forall n1 n2,
    n1 = n2 ->
    subsumed n1 n2
  | Subsumed_owned : forall ct ct',
    ct = ct' -> subsumed (Owned ct Uninit) (Owned ct' Init).

Module Init_as_MiniDecidableType <: MiniDecidableType.
  Definition t := init.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    unfold eq.
    decide equality. 
  Defined.
End Init_as_MiniDecidableType.

Module Name_as_MiniDecidableType <: MiniDecidableType.
  Definition t := name.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    unfold eq.
    intros [o1 | p1] [o2 | p2].
    all: decide equality.
    all: try apply SCtypes_as_MiniDecidableType.eq_dec.
    all: try apply Init_as_MiniDecidableType.eq_dec.
    all: try apply Sym_t_as_MiniDecidableType.eq_dec.
  Defined.
End Name_as_MiniDecidableType.

Module Predicate_as_MiniDecidableType <: MiniDecidableType.
  Definition t := Predicate.t.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    unfold eq.
    intros [n1 p1 i1] [n2 p2 i2].
    decide equality.
    - apply List.list_eq_dec.
      apply IndexTerm_as_MiniDecidableType.eq_dec.
    - apply IndexTerm_as_MiniDecidableType.eq_dec.
    - apply Name_as_MiniDecidableType.eq_dec.
  Defined. 
End Predicate_as_MiniDecidableType.

Module QPredicate_as_MiniDecidableType <: MiniDecidableType.
  Definition t := QPredicate.t.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    unfold eq.
    intros x y.
    destruct x, y.
    decide equality.
    - apply List.list_eq_dec.
      apply IndexTerm_as_MiniDecidableType.eq_dec.
    - apply IndexTerm_as_MiniDecidableType.eq_dec.
    - apply SCtypes_as_MiniDecidableType.eq_dec.
    - apply Locations_t_as_MiniDecidableType.eq_dec.
    - apply BaseTypes.prod_eq_dec.
      + apply Sym_t_as_MiniDecidableType.eq_dec.
      + apply BasetTypes_t_as_MiniDecidableType.eq_dec.
    - apply IndexTerm_as_MiniDecidableType.eq_dec.
    - apply Name_as_MiniDecidableType.eq_dec.
  Defined.
End QPredicate_as_MiniDecidableType.

Module Request_as_MiniDecidableType <: MiniDecidableType.
  Definition t := t.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    unfold eq.
    intros [p1 | q1] [p2 | q2].
    all: decide equality.
    all: try apply Predicate_as_MiniDecidableType.eq_dec.
    all: try apply QPredicate_as_MiniDecidableType.eq_dec.
  Defined.
End Request_as_MiniDecidableType.
