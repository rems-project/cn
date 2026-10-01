Require List.
Require Import ZArith Bool.
Require Import Lia.
Require NArith.
Require Fin.
Require Import Vector.

Open Scope Z.

(* Bounds on the unsigned integer types. *)
Notation min_UChar  := 0%Z.
Notation max_UChar  := (2 ^ 8 - 1)%Z.
Notation min_UShort := 0%Z.
Notation max_UShort := (2 ^ 16 - 1)%Z.
Notation min_UInt   := 0%Z.
Notation max_UInt   := (2 ^ 32 - 1)%Z.
Notation min_ULong  := 0%Z.
Notation max_ULong  := (2 ^ 64 - 1)%Z.

(* Bounds on the signed integer types. *)
Notation min_Char  := (- (2 ^ 7))%Z.
Notation max_Char  := (2 ^ 7 - 1)%Z.
Notation min_Short := (- (2 ^ 15))%Z.
Notation max_Short := (2 ^ 15 - 1)%Z.
Notation min_Int   := (- (2 ^ 31))%Z.
Notation max_Int   := (2 ^ 31 - 1)%Z.
Notation min_Long  := (- (2 ^ 63))%Z.
Notation max_Long  := (2 ^ 63 - 1)%Z.

Fixpoint list_of_fin (n : nat) : list (Fin.t n) :=
  match n with
  | 0%nat => List.nil
  | S n' => Fin.F1 :: List.map Fin.FS (list_of_fin n')
  end.

Fixpoint to_CN_Bytes (n : nat) (v : Z) : (Vector.t Z n) :=
  match n with
  | O => Vector.nil Z
  | S n' => Vector.cons Z (v mod 256) n' (to_CN_Bytes n' (v / 256))
  end.

Definition from_CN_Bytes {n : nat} (v : Vector.t Z n) : Z :=
List.fold_left
  (fun acc i =>
      acc + nth v i * 2 ^ (8 * Z.of_nat (proj1_sig(Fin.to_nat i))))
  (list_of_fin n)
  0.


Definition UShort_bytes (v : Z) : Vector.t Z 2 := to_CN_Bytes 2 v.
Definition UInt_bytes   (v : Z) : Vector.t Z 4 := to_CN_Bytes 4 v.
Definition ULong_bytes  (v : Z) : Vector.t Z 8 := to_CN_Bytes 8 v.

Theorem UShort_idem {v : Z} :
   min_UShort <= v /\ v <= max_UShort
  -> v = from_CN_Bytes (UShort_bytes v).
Proof.
  intros (H1, H2); unfold max_UShort in H2; simpl in H2.
  unfold from_CN_Bytes, UShort_bytes; cbn.
  assert (Hv : v = 256 * (v / 256) + v mod 256) by (apply Z.div_mod; lia).
  assert (Hv'' : 0 <= v / 256 < 256).
    { constructor.
    - apply Z.div_le_lower_bound; lia.
    - apply Z.div_lt_upper_bound; lia. }
  rewrite Z.mul_1_r, Z.mul_comm, Z.add_comm, Hv at 1.
  do 2 f_equal.
  rewrite Z.mod_small; auto.
Qed.

Theorem UInt_idem {v : Z} :
   min_UInt <= v /\ v <= max_UInt
  -> v = from_CN_Bytes (UInt_bytes v).
Proof.
  intros (H1, H2); unfold max_UInt in H2; simpl in H2.
  unfold from_CN_Bytes, UInt_bytes; cbn.
  rewrite Z.mul_1_r, Z.mul_comm, Z.add_comm. 
  (* First division *)
  assert (H3 : v = 256 * (v / 256) + v mod 256) by (apply Z.div_mod; lia).
  (* Second division *)
  assert (H4 : v / 256 = 256 * (v / 256 / 256) + (v / 256) mod 256) by (apply Z.div_mod; lia).
  (* Third division *)
  assert (H5 : v / 256 / 256 = 256 * (v / 256 / 256 / 256) +
      (v / 256 / 256) mod 256)  by (apply Z.div_mod; lia).
  (* max bound *)
  assert (H6 :
    v / 256 / 256 / 256 < 256).
  { repeat (apply Z.div_lt_upper_bound; [lia |]).
    lia. }
  (* min bound *)
  assert (H7 : 
    0 <= v / 256 / 256 / 256 < 256).
  { constructor.
    - repeat (apply Z.div_le_lower_bound; [lia; simpl |]); lia.
    - repeat (apply Z.div_lt_upper_bound; [lia; simpl |]); lia. }
  assert (H4mod :
    (v / 256 / 256 / 256) mod 256 = v / 256 / 256 / 256) by (apply Z.mod_small; lia).
  rewrite H4mod.
  rewrite H3 at 1.
  rewrite H4 at 1.
  rewrite H5 at 1.
  lia.
Qed.

(* TODO: Prove the above theorem for ULong *)

Theorem UShort_eq {v v' : Z} :
    min_UShort <= v /\ v <= max_UShort
  -> min_UShort <= v' /\ v' <= max_UShort
  -> UShort_bytes v = UShort_bytes v'
  -> v = v'.
Proof.
  intros Hv Hv' H.
  unfold UShort_bytes in H.
  assert (HEq : v = from_CN_Bytes (UShort_bytes v)) by (apply UShort_idem; assumption).
  assert (HEq' : v' = from_CN_Bytes (UShort_bytes v')) by (apply UShort_idem; assumption).
  unfold UShort_bytes in HEq, HEq'.
  rewrite H in HEq.
  rewrite <- HEq' in HEq.
  assumption.
Qed.

Theorem UInt_eq {v v' : Z} :
    min_UInt <= v /\ v <= max_UInt
  -> min_UInt <= v' /\ v' <= max_UInt
  -> UInt_bytes v = UInt_bytes v'
  -> v = v'.
Proof.
  intros Hv Hv' H.
  unfold UInt_bytes in H.
  assert (HEq : v = from_CN_Bytes (UInt_bytes v)) by (apply UInt_idem; assumption).
  assert (HEq' : v' = from_CN_Bytes (UInt_bytes v')) by (apply UInt_idem; assumption).
  unfold UInt_bytes in HEq, HEq'.
  rewrite H in HEq.
  rewrite <- HEq' in HEq. 
  assumption.
Qed.

(* TODO: Prove the above theorem for ULong *)

Definition wrapI (minInt : Z) (maxInt : Z) x :=
  let delta := ((maxInt - minInt) + 1)%Z in
  let r := Z.modulo x delta in
  (if (r <=? maxInt) then r else r - delta)%Z.

Lemma wrapI_idem:
  forall (minInt maxInt x : Z),
  (minInt <= x <= maxInt)%Z ->
  (minInt <= 0 < maxInt)%Z ->
  wrapI minInt maxInt x = x.
Proof.
  Open Scope Z.
  intros.
  unfold wrapI.
  pose (delta := ((maxInt - minInt) + 1)).
  destruct (0 <=? x) eqn: x_neg.
  - rewrite Z.mod_small by lia.
    rewrite Zle_imp_le_bool by lia.
    reflexivity.
  - rewrite (Znumtheory.Zdivide_mod_minus _ _ (x + delta)).
    + destruct (x + delta <=? maxInt) eqn: leb; lia.
    + lia.
    + exists (-1); lia.
Qed.

(* TODO: Prove this theorem *)
Theorem wrapI_within_bounds :
  forall {min max x : Z}, (min <= 0 /\ 0 < max) ->
  min <= wrapI min max x /\ wrapI min max x <= max.
Proof.
  Admitted.

(* TODO: Prove the rest of the `wrapI_*` theorems from the Lean library *)