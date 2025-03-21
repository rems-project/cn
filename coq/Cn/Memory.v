Require Import Coq.ZArith.ZArith.

From Cerberus Require Import Implementation Utils.
Require Import Sym.
Require Import SCtypes.
Require Import Id.
Require Import CNImpl.

Record struct_piece := mk_struct_piece {
  piece_offset : Z;
  piece_size : Z;
  piece_member_or_padding : option (Id.t * SCtypes.ctype)
}.

Record struct_member := mk_struct_member {
  member_offset : Z;
  member_size : Z;
  memory_struct_member : Id.t * SCtypes.ctype (* rnamed from `struct_member` to avoid name clash with `struct_member` in MuCore.v*)
}.

Definition struct_layout := list struct_piece.
Definition struct_decl := struct_layout.
Definition struct_decls := SymMap.t struct_decl.


Definition is_signed_integer_type ity := (CNImpl.get).(is_signed_ity) ity.

Definition sizeof_ity ity := option_get 0 ((CNImpl.get).(sizeof_ity) ity).


