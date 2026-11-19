Require Import ZArith.
Require Import String.
Require Import List.
Import ListNotations.

From Cerberus Require Import Location.
Require Import ErrorCommon.
Require Import Request.
Require Import Resource.
Require Import Context.

Inductive unpack_result : Type :=
  | UnpackLRT : LogicalReturnTypes.t -> unpack_result
  | UnpackRES : list Resource.t -> unpack_result.

Definition unfold_changed := list (Resource.t*unpack_result).
Definition extract_changed := list Resource.t.
Definition unfold_step := (unfold_changed*extract_changed)%type.

Inductive log_entry : Type :=
  | PredicateRequest : Context.t ->
                       ErrorCommon.situation ->
                       Request.Predicate.t ->
                       Resource.predicate ->
                       list log_entry ->
                       Context.t ->
                       log_entry
  | UnfoldResources: Context.t ->
                     Location.t ->
                     list unfold_step ->
                     Context.t ->
                     log_entry.

Definition log : Type := list log_entry.

