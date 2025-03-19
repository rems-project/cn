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

Definition unfold_changed := list (Z*Resource.t*unpack_result).
Definition extract_changed := list Resource.t.
Definition unfold_step := (unfold_changed*extract_changed)%type.

Inductive resource_inference_type : Type :=
  | PredicateRequest : ErrorCommon.situation ->
                       Request.Predicate.t ->
                       option (Location.t * string) ->
                       (Resource.predicate * list Z) ->
                       resource_inference_type
  | UnfoldResources: Location.t -> list unfold_step -> resource_inference_type.

Inductive log_entry : Type :=
  | ResourceInferenceStep : Context.t -> resource_inference_type -> Context.t -> log_entry.

Definition log : Type := list log_entry.

