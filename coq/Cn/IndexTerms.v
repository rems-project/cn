Require Import List.
Require Import Coq.Structures.DecidableType.
Require Import Coq.Structures.DecidableTypeEx.

Require Import BaseTypes.
Require Import Terms.
Require Import Locations.

(* Type aliases *)
Definition t' := term BaseTypes.t.
Definition t := annot BaseTypes.t.

(* Pattern-related types *)
Definition bindings (bt: Type) := list (pattern bt * option (annot bt)).
Definition t_bindings := bindings BaseTypes.t. 

Module IndexTerm_as_MiniDecidableType <: MiniDecidableType :=
  Annot_as_MiniDecidableType BasetTypes_t_as_MiniDecidableType.

Module IndexTerm_as_DecidableType := Make_UDT IndexTerm_as_MiniDecidableType.

