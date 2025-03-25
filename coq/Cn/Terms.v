Require Import ZArith.
Require Import String.
Require Import List.
Require Import QArith.Qcanon.
Require Import List.
Require Import Coq.Structures.DecidableType.
Require Import Coq.Structures.DecidableTypeEx.
Require Import Cerberus.Symbol.
Require Import Sym.
Require Import BaseTypes.
Require Import Cerberus.Location.
Require Import SCtypes.
Require Import Locations.
Require Import Cerberus.IntegerType.

(* Constants *)
Inductive const : Type :=
  | Z : Z -> const
  | Bits : (BaseTypes.sign * nat) * Z -> const
  | Q : Qc -> const
  | MemByte : Z * Z -> const  (* alloc_id * value *)
  | Pointer : Z * Z -> const  (* alloc_id * addr *)
  | Alloc_id : Z -> const
  | Bool : bool -> const
  | Unit : const
  | Null : const
  | CType_const : SCtypes.ctype -> const
  | Default : BaseTypes.t -> const.

(* Unary operators *)
Inductive unop : Type :=
  | Not : unop
  | Negate : unop
  | BW_CLZ_NoSMT : unop
  | BW_CTZ_NoSMT : unop
  | BW_FFS_NoSMT : unop
  | BW_FLS_NoSMT : unop
  | BW_Compl : unop.

(* Binary operators *)
Inductive binop : Type :=
  | And : binop
  | Or : binop
  | Implies : binop
  | Add : binop
  | Sub : binop
  | Mul : binop
  | MulNoSMT : binop
  | Div : binop
  | DivNoSMT : binop
  | Exp : binop
  | ExpNoSMT : binop
  | Rem : binop
  | RemNoSMT : binop
  | Mod : binop
  | ModNoSMT : binop
  | BW_Xor : binop
  | BW_And : binop
  | BW_Or : binop
  | ShiftLeft : binop
  | ShiftRight : binop
  | LT : binop
  | LE : binop
  | Min : binop
  | Max : binop
  | EQ : binop
  | LTPointer : binop
  | LEPointer : binop
  | SetUnion : binop
  | SetIntersection : binop
  | SetDifference : binop
  | SetMember : binop
  | Subset : binop.

Unset Elimination Schemes.

(* Patterns *)
Inductive pattern_ (bt : Type) : Type :=
  | PSym : sym -> pattern_ bt
  | PWild : pattern_ bt
  | PConstructor : sym -> list (Symbol.identifier * pattern bt) -> pattern_ bt

with pattern (bt : Type) : Type :=
  | Pat : pattern_ bt -> bt -> Locations.t -> pattern bt.

(* Terms *)
Inductive term (bt : Type) : Type :=
  | Const : const -> term bt
  | Sym : sym -> term bt
  | Unop : unop -> annot bt -> term bt
  | Binop : binop -> annot bt -> annot bt -> term bt
  | ITE : annot bt -> annot bt -> annot bt -> term bt
  | EachI : (nat * (sym * BaseTypes.t) * nat) -> annot bt -> term bt
  | Tuple : list (annot bt) -> term bt
  | NthTuple : nat -> annot bt -> term bt
  | Struct : sym -> list (Symbol.identifier * annot bt) -> term bt
  | StructMember : annot bt -> Symbol.identifier -> term bt
  | StructUpdate :  (annot bt * Symbol.identifier) -> annot bt -> term bt
  | TRecord : list (Symbol.identifier * annot bt) -> term bt
  | RecordMember : annot bt -> Symbol.identifier -> term bt
  | RecordUpdate :  (annot bt * Symbol.identifier) -> annot bt -> term bt
  | Constructor : sym -> list (Symbol.identifier * annot bt) -> term bt
  | MemberShift : annot bt -> sym -> Symbol.identifier -> term bt
  | ArrayShift : annot bt -> SCtypes.t -> annot bt -> term bt  (* base * ct * index *)
  | CopyAllocId : annot bt -> annot bt -> term bt  (* addr * loc *)
  | HasAllocId : annot bt -> term bt
  | SizeOf : SCtypes.t -> term bt 
  | OffsetOf : sym -> Symbol.identifier -> term bt
  | Nil : BaseTypes.t -> term bt
  | Cons : annot bt -> annot bt -> term bt
  | Head : annot bt -> term bt
  | Tail : annot bt -> term bt
  | NthList : annot bt -> annot bt -> annot bt -> term bt
  | ArrayToList : annot bt -> annot bt -> annot bt -> term bt
  | Representable : SCtypes.t -> annot bt -> term bt  
  | Good : SCtypes.t -> annot bt -> term bt  
  | Aligned : annot bt -> annot bt -> term bt  (* t * align *)
  | WrapI : IntegerType.t  -> annot bt -> term bt 
  | MapConst : BaseTypes.t -> annot bt -> term bt
  | MapSet : annot bt -> annot bt -> annot bt -> term bt
  | MapGet : annot bt -> annot bt -> term bt
  | MapDef : (sym * BaseTypes.t) -> annot bt -> term bt
  | Apply : sym -> (list  (annot bt)) -> term bt
  | TLet : (sym * annot bt) -> annot bt -> term bt
  | TMatch : annot bt -> list (pattern bt * annot bt) -> term bt
  | Cast : BaseTypes.t -> annot bt -> term bt

with annot (bt : Type) : Type :=
  | IT : term bt -> bt -> Locations.t -> annot bt.

Set Elimination Schemes.

(* We define a custom induction principle for [pattern_] to properly handle
   mutual induction and constructors with hidden recursive cases. *)
Theorem pattern__ind_set (ty : Type) (P : pattern_ ty -> Type) (P' : pattern ty -> Type) :
  (forall (s : sym), P (PSym ty s)) ->
  (P (PWild ty)) ->
  (forall (s : sym) (l : list (Symbol.identifier * pattern ty)),
    Forall_type (fun '(_, p) => P' p) l -> P (PConstructor ty s l)) ->
  (forall (p : pattern_ ty) (tt : ty) (lc : Locations.t), P p -> P' (Pat ty p tt lc)) ->
  forall p : pattern_ ty, P p.
Proof.
  intros HPSym HPWild HPConstructor HPPat.
  fix IH 1.
  intros p.
  destruct p.
  - apply HPSym.
  - apply HPWild.
  - clear - HPConstructor HPPat IH.
    apply HPConstructor.
    induction l.
    + apply Forall_nil.
    + destruct a as [i p].
      destruct p.
      apply Forall_cons.
      * apply HPPat, IH.
      * apply IHl.
Qed.

(* We define a custom induction principle for [term] to properly handle
   mutual induction and constructors with hidden recursive cases. *)
Theorem term_ind_set (ty : Type) (P : term ty -> Type) (P' : annot ty -> Type) :
  (forall (c : const), P (Const ty c)) ->
  (forall (s : sym), P (Sym ty s)) ->
  (forall (u : unop) (a : annot ty), P' a -> P (Unop ty u a)) ->
  (forall (b : binop) (a1 a2 : annot ty), P' a1 -> P' a2 -> P (Binop ty b a1 a2)) ->
  (forall (a1 a2 a3 : annot ty), P' a1 -> P' a2 -> P' a3 -> P (ITE ty a1 a2 a3)) ->
  (forall (p : nat * (sym * BaseTypes.t) * nat) (a : annot ty), P' a -> P (EachI ty p a)) ->
  (forall (l : list (annot ty)), Forall_type (fun a => P' a) l -> P (Tuple ty l)) ->
  (forall (n : nat) (a : annot ty), P' a -> P (NthTuple ty n a)) ->
  (forall (s : sym) (l : list (identifier * annot ty)), Forall_type (fun '(_, a) => P' a) l -> P (Struct ty s l)) ->
  (forall (a : annot ty) (i : identifier), P' a -> P (StructMember ty a i)) ->
  (forall (p : annot ty * identifier) (a : annot ty), P' (fst p) -> P' a -> P (StructUpdate ty p a)) ->
  (forall (l : list (identifier * annot ty)), Forall_type (fun '(_, a) => P' a) l -> P (TRecord ty l)) ->
  (forall (a : annot ty) (i : identifier), P' a -> P (RecordMember ty a i)) ->
  (forall (p : annot ty * identifier) (a : annot ty), P' (fst p) -> P' a -> P (RecordUpdate ty p a)) ->
  (forall (s : sym) (l : list (identifier * annot ty)), Forall_type (fun '(_, a) => P' a) l -> P (Constructor ty s l)) ->
  (forall (a : annot ty) (s : sym) (i : identifier), P' a -> P (MemberShift ty a s i)) ->
  (forall (a1 : annot ty) (ct : SCtypes.t) (a2 : annot ty), P' a1 -> P' a2 -> P (ArrayShift ty a1 ct a2)) ->
  (forall (a1 a2 : annot ty), P' a1 -> P' a2 -> P (CopyAllocId ty a1 a2)) ->
  (forall (a : annot ty), P' a -> P (HasAllocId ty a)) ->
  (forall (ct : SCtypes.t), P (SizeOf ty ct)) ->
  (forall (s : sym) (i : identifier), P (OffsetOf ty s i)) ->
  (forall (bt : BaseTypes.t), P (Nil ty bt)) ->
  (forall (a1 a2 : annot ty), P' a1 -> P' a2 -> P (Cons ty a1 a2)) ->
  (forall (a : annot ty), P' a -> P (Head ty a)) ->
  (forall (a : annot ty), P' a -> P (Tail ty a)) ->
  (forall (a1 a2 a3 : annot ty), P' a1 -> P' a2 -> P' a3 -> P (NthList ty a1 a2 a3)) ->
  (forall (a1 a2 a3: annot ty), P' a1 -> P' a2 -> P' a3 -> P (ArrayToList ty a1 a2 a3)) ->
  (forall (ct : SCtypes.t) (a : annot ty) , P' a -> P (Representable ty ct a)) ->
  (forall (ct : SCtypes.t) (a : annot ty) , P' a -> P (Good ty ct a)) ->
  (forall (a1 a2 : annot ty), P' a1 -> P' a2 -> P (Aligned ty a1 a2)) ->
  (forall (i : IntegerType.t) (a : annot ty), P' a -> P (WrapI ty i a)) ->
  (forall (bt : BaseTypes.t) (a : annot ty), P' a -> P (MapConst ty bt a)) ->
  (forall (a1 a2 a3 : annot ty), P' a1 -> P' a2 -> P' a3 -> P (MapSet ty a1 a2 a3)) ->
  (forall (a1 a2 : annot ty), P' a1 -> P' a2 -> P (MapGet ty a1 a2)) ->
  (forall (p : sym * BaseTypes.t) (a : annot ty), P' a -> P (MapDef ty p a)) ->
  (forall (s : sym) (l : list (annot ty)), Forall_type (fun a => P' a) l -> P (Apply ty s l)) ->
  (forall (p : sym * annot ty) (a : annot ty), P' (snd p) -> P' a -> P (TLet ty p a)) ->
  (forall (a : annot ty) (l : list (pattern ty * annot ty)), P' a -> Forall_type (fun '(_, a) => P' a) l -> P (TMatch ty a l)) ->
  (forall (bt : BaseTypes.t) (a : annot ty), P' a -> P (Cast ty bt a)) ->
  (forall (t : term ty) (tt : ty) (lc : Locations.t), P t -> P' (IT ty t tt lc)) ->
  forall t : term ty, P t.
Proof.
  intros HConst HSym HUnop HBinop HITE HEachI HTuple HNthTuple HStruct
         HStructMember HStructUpdate HTRecord HRecordMember HRecordUpdate
         HConstructor HMemberShift HArrayShift HCopyAllocId HHasAllocId
         HSizeOf HOffsetOf HNil HCons HHead HTail HNthList HArrayToList
         HRepresentable HGood HAligned HWrapI HMapConst HMapSet HMapGet
         HMapDef HApply HTLet HTMatch HCast HIT.
  fix IH 1.
  intros t.
  destruct t.
  - apply HConst.
  - apply HSym.
  - clear - HUnop HIT IH.
    destruct a.
    apply HUnop, HIT, IH.
  - clear - HBinop HIT IH.
    destruct a, a0.
    apply HBinop; apply HIT, IH.
  - clear - HITE HIT IH.
    destruct a, a0, a1.
    apply HITE; apply HIT, IH.
  - clear - HEachI HIT IH.
    destruct a.
    apply HEachI, HIT, IH.
  - clear - HTuple HIT IH.
    apply HTuple.
    induction l.
    + apply Forall_nil.
    + destruct a.
      apply Forall_cons.
      * apply HIT, IH.
      * apply IHl.
  - clear - HNthTuple HIT IH.
    destruct a.
    apply HNthTuple, HIT, IH.
  - clear - HStruct HIT IH.
    apply HStruct.
    induction l.
    + apply Forall_nil.
    + destruct a as [? a], a.
      apply Forall_cons.
      * apply HIT, IH.
      * apply IHl.
  - clear - HStructMember HIT IH.
    destruct a.
    apply HStructMember, HIT, IH.
  - clear - HStructUpdate HIT IH.
    destruct a, p as [a1 ?], a1.
    apply HStructUpdate; apply HIT, IH.
  - clear - HTRecord HIT IH.
    apply HTRecord.
    induction l.
    + apply Forall_nil.
    + destruct a as [? a], a.
      apply Forall_cons.
      * apply HIT, IH.
      * apply IHl.
  - clear - HRecordMember HIT IH.
    destruct a.
    apply HRecordMember, HIT, IH.
  - clear - HRecordUpdate HIT IH.
    destruct a, p as [a1 ?], a1.
    apply HRecordUpdate; apply HIT, IH.
  - clear - HConstructor HIT IH.
    apply HConstructor.
    induction l.
    + apply Forall_nil.
    + destruct a as [? a], a.
      apply Forall_cons.
      * apply HIT, IH.
      * apply IHl.
  - clear - HMemberShift HIT IH.
    destruct a.
    apply HMemberShift; apply HIT, IH.
  - clear - HArrayShift HIT IH.
    destruct a, a0.
    apply HArrayShift; apply HIT, IH.
  - clear - HCopyAllocId HIT IH.
    destruct a, a0.
    apply HCopyAllocId; apply HIT, IH.
  - clear - HHasAllocId HIT IH.
    destruct a.
    apply HHasAllocId; apply HIT, IH.
  - apply HSizeOf.
  - apply HOffsetOf.
  - apply HNil.
  - clear - HCons HIT IH.
    destruct a, a0.
    apply HCons; apply HIT, IH.
  - clear - HHead HIT IH.
    destruct a.
    apply HHead; apply HIT, IH.
  - clear - HTail HIT IH.
    destruct a.
    apply HTail; apply HIT, IH.
  - clear - HNthList HIT IH.
    destruct a, a0, a1.
    apply HNthList; apply HIT, IH.
  - clear - HArrayToList HIT IH.
    destruct a, a0, a1.
    apply HArrayToList; apply HIT, IH.
  - clear - HRepresentable HIT IH.
    destruct a.
    apply HRepresentable, HIT, IH.
  - clear - HGood HIT IH.
    destruct a.
    apply HGood, HIT, IH.
  - clear - HAligned HIT IH.
    destruct a, a0.
    apply HAligned; apply HIT, IH.
  - clear - HWrapI HIT IH.
    destruct a.
    apply HWrapI, HIT, IH.
  - clear - HMapConst HIT IH.
    destruct a.
    apply HMapConst, HIT, IH.
  - clear - HMapSet HIT IH.
    destruct a, a0, a1.
    apply HMapSet; apply HIT, IH.
  - clear - HMapGet HIT IH.
    destruct a, a0.
    apply HMapGet; apply HIT, IH.
  - clear - HMapDef HIT IH.
    destruct a.
    apply HMapDef, HIT, IH.
  - clear - HApply HIT IH.
    apply HApply.
    induction l.
    + apply Forall_nil.
    + destruct a.
      apply Forall_cons.
      * apply HIT, IH.
      * apply IHl.
  - clear - HTLet HIT IH.
    destruct a, p as [? a], a.
    apply HTLet; apply HIT, IH.
  - clear - HTMatch HIT IH.
    destruct a.
    apply HTMatch.
    + apply HIT, IH.
    + induction l.
      * apply Forall_nil.
      * destruct a as [? a], a.
        apply Forall_cons.
        -- apply HIT, IH.
        -- apply IHl.
  - clear - HCast HIT IH.
    destruct a.
    apply HCast; apply HIT, IH.
Defined.

Module Const_as_MiniDecidableType <: MiniDecidableType.
  Definition t := const.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    unfold eq.
    intros x y.
    destruct x, y; try (right; discriminate).
    - destruct (Z_as_DT.eq_dec z z0); try (right; congruence); subst.
      left; reflexivity.
    - destruct p as [[s n] z], p0 as [[s' n'] z'].
      destruct (Nat_as_DT.eq_dec n n'); try (right; congruence); subst.
      destruct (sign_eq_dec s s'); try (right; congruence); subst.
      destruct (Z_as_DT.eq_dec z z'); try (right; congruence); subst.
      left; reflexivity.
    - destruct (Qc_eq_dec q q0); try (right; congruence); subst.
      left; reflexivity.
    - destruct p as [z1 z2], p0 as [z1' z2'].
      destruct (Z_as_DT.eq_dec z1 z1'); try (right; congruence); subst.
      destruct (Z_as_DT.eq_dec z2 z2'); try (right; congruence); subst.
      left; reflexivity.
    - destruct p as [z1 z2], p0 as [z1' z2'].
      destruct (Z_as_DT.eq_dec z1 z1'); try (right; congruence); subst.
      destruct (Z_as_DT.eq_dec z2 z2'); try (right; congruence); subst.
      left; reflexivity.
    - destruct (Z_as_DT.eq_dec z z0); try (right; congruence); subst.
      left; reflexivity.
    - destruct (Bool.bool_dec b b0); try (right; congruence); subst.
      left; reflexivity.
    - left; reflexivity.
    - left; reflexivity.
    - destruct (SCtypes_as_MiniDecidableType.eq_dec c c0) as [E | NE]; try (right; congruence).
      inversion E; subst; left; reflexivity.
    - destruct (BasetTypes_t_as_MiniDecidableType.eq_dec t0 t1) as [E | NE]; try (right; congruence).
      inversion E; subst; left; reflexivity.
  Defined.
End Const_as_MiniDecidableType.

Module Unop_as_MiniDecidableType <: MiniDecidableType.
  Definition t := unop.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    unfold eq.
    decide equality.
  Defined.
End Unop_as_MiniDecidableType.

Module Binop_as_MiniDecidableType <: MiniDecidableType.
  Definition t := binop.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    unfold eq.
    decide equality.
  Defined.
End Binop_as_MiniDecidableType.

Module Pattern__as_MiniDecidableType (Ty_as_MiniDecidableType : MiniDecidableType) <: MiniDecidableType.
  Definition ty := Ty_as_MiniDecidableType.t.

  Lemma eq_dec : forall (x y : pattern_ ty), { eq x y } + { ~ eq x y }.
  Proof.
    apply (pattern__ind_set ty
      (fun p1 => forall p2, { eq p1 p2 } + { ~ eq p1 p2 })
      (fun p1 => forall p2, { eq p1 p2 } + { ~ eq p1 p2 })).
    - intros s p0.
      destruct p0; try (right; discriminate).
      destruct (Sym_t_as_MiniDecidableType.eq_dec s s0) as [E | ?]; try (right; congruence).
      inversion E; subst; left; reflexivity.
    - intros p0.
      destruct p0; try (right; discriminate).
      left; reflexivity.
    - intros s l IHl p0.
      destruct p0; try (right; discriminate).
      destruct (Sym_t_as_MiniDecidableType.eq_dec s s0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      revert l0.
      induction IHl as [| [i p] l IHp _ IHl]; intros l0.
      + destruct l0; try (right; discriminate).
        left; reflexivity.
      + destruct l0; try (right; discriminate).
        destruct p0 as [i0 p0].
        destruct (Symbol_identifier_as_MiniDecidableType.eq_dec i i0) as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        destruct (IHp p0) as [E | ?]; try (right; congruence); subst.
        destruct (IHl l0) as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        left; reflexivity.
    - intros p tt lc IHp p0.
      destruct p0; try (right; discriminate).
      destruct (Locations_t_as_MiniDecidableType.eq_dec lc t0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (Ty_as_MiniDecidableType.eq_dec tt t) as [E | ?]; try (right; congruence); subst.
      destruct (IHp p0); try (right; congruence); subst.
      left; reflexivity.
  Defined.

  Definition t := pattern_ ty.
  Definition eq := @eq t.
End Pattern__as_MiniDecidableType.

Module Pattern_as_MiniDecidableType (Ty_as_MiniDecidableType : MiniDecidableType) <: MiniDecidableType.
  Module Pattern__as_MiniDecidableType := Pattern__as_MiniDecidableType Ty_as_MiniDecidableType.
  Definition ty := Ty_as_MiniDecidableType.t.
  Definition t := pattern ty.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    unfold eq.
    intros [p1 tt1 lc1] [p2 tt2 lc2].
    destruct (Pattern__as_MiniDecidableType.eq_dec p1 p2); try (right; congruence); subst.
    destruct (Ty_as_MiniDecidableType.eq_dec tt1 tt2); try (right; congruence); subst.
    destruct (Locations_t_as_MiniDecidableType.eq_dec lc1 lc2) as [E | NE]; try (right; congruence).
    inversion E; subst; left; reflexivity.
  Defined.
End Pattern_as_MiniDecidableType.

Module Term_as_MiniDecidableType (Ty_as_MiniDecidableType : MiniDecidableType) <: MiniDecidableType.
  Module Pattern_as_MiniDecidableType := Pattern_as_MiniDecidableType Ty_as_MiniDecidableType.
  Definition ty := Ty_as_MiniDecidableType.t.

  Lemma eq_dec : forall (x y : term ty), { eq x y } + { ~ eq x y }.
  Proof.
    apply (term_ind_set ty
            (fun t1 => forall t2, { eq t1 t2 } + { ~ eq t1 t2 })
            (fun a1 => forall a2, { eq a1 a2 } + { ~ eq a1 a2 })).
    - intros c t0.
      destruct t0; try (right; discriminate).
      destruct (Const_as_MiniDecidableType.eq_dec c c0) as [E | ?]; try (right; congruence).
      inversion E; subst; left; reflexivity.
    - intros s t0.
      destruct t0; try (right; discriminate).
      destruct (Sym_t_as_MiniDecidableType.eq_dec s s0) as [E | ?]; try (right; congruence).
      inversion E; subst; left; reflexivity.
    - intros u a IHa t0.
      destruct t0; try (right; discriminate).
      destruct (Unop_as_MiniDecidableType.eq_dec u u0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros b a1 a2 IHa1 IHa2 t0.
      destruct t0; try (right; discriminate).
      destruct (Binop_as_MiniDecidableType.eq_dec b b0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa1 a); try (right; congruence); subst.
      destruct (IHa2 a0); try (right; congruence); subst.
      left; reflexivity.
    - intros a1 a2 a3 IHa1 IHa2 IHa3 t0.
      destruct t0; try (right; discriminate).
      destruct (IHa1 a); try (right; congruence); subst.
      destruct (IHa2 a0); try (right; congruence); subst.
      destruct (IHa3 a4); try (right; congruence); subst.
      left; reflexivity.
    - intros p a IHa t0.
      destruct t0; try (right; discriminate).
      destruct p as [[n1 [s bt]] n2].
      destruct p0 as [[n1' [s' bt']] n2'].
      destruct (Nat_as_DT.eq_dec n1 n1'); try (right; congruence).
      destruct (Sym_t_as_MiniDecidableType.eq_dec s s') as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (BasetTypes_t_as_MiniDecidableType.eq_dec bt bt') as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (Nat_as_DT.eq_dec n2 n2'); try (right; congruence); subst.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros l IHl t0.
      destruct t0; try (right; discriminate).
      revert l0.
      induction IHl as [| [t tt] l IH _ IHl]; intros l0.
      + destruct l0; try (right; discriminate).
        left; reflexivity.
      + destruct l0; try (right; discriminate).
        destruct (IH a) as [E | ?]; try (right; congruence).
        destruct a.
        inversion E; subst; clear E.
        destruct (IHl l0) as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        left; reflexivity.
    - intros n a IHa t0.
      destruct t0; try (right; discriminate).
      destruct (Nat_as_DT.eq_dec n n0); try (right; congruence); subst.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros s l IHl t0.
      destruct t0; try (right; discriminate).
      destruct (Sym_t_as_MiniDecidableType.eq_dec s s0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      revert l0.
      induction IHl as [| [i a] l IHa _ IHl]; intros l0.
      + destruct l0; try (right; discriminate).
        left; reflexivity.
      + destruct l0; try (right; discriminate).
        destruct p as [i0 a0].
        destruct (Symbol_identifier_as_MiniDecidableType.eq_dec i i0) as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        destruct (IHa a0) as [E | ?]; try (right; congruence); subst.
        destruct (IHl l0) as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        left; reflexivity.
    - intros a i IHa t0. 
      destruct t0; try (right; discriminate).
      destruct (Symbol_identifier_as_MiniDecidableType.eq_dec i i0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros p a1 IHa2 IHa1 t0.
      destruct t0; try (right; discriminate).
      destruct p as [a2 i].
      destruct p0 as [a2' i'].
      destruct (Symbol_identifier_as_MiniDecidableType.eq_dec i i') as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      cbn in *.
      destruct (IHa2 a2'); try (right; congruence); subst.
      destruct (IHa1 a); try (right; congruence); subst.
      left; reflexivity.
    - intros l IHl t0.
      destruct t0; try (right; discriminate).
      revert l0.
      induction IHl as [| [i a] l IHa _ IHl]; intros l0.
      + destruct l0; try (right; discriminate).
        left; reflexivity.
      + destruct l0; try (right; discriminate).
        destruct p as [i0 a0].
        destruct (Symbol_identifier_as_MiniDecidableType.eq_dec i i0) as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        destruct (IHa a0) as [E | ?]; try (right; congruence); subst.
        destruct (IHl l0) as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        left; reflexivity.
    - intros a i IHa t0.
      destruct t0; try (right; discriminate).
      destruct (Symbol_identifier_as_MiniDecidableType.eq_dec i i0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros p a1 IHa2 IHa1 t0.
      destruct t0; try (right; discriminate).
      destruct p as [a2 i].
      destruct p0 as [a2' i'].
      destruct (Symbol_identifier_as_MiniDecidableType.eq_dec i i') as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      cbn in *.
      destruct (IHa1 a); try (right; congruence); subst.
      destruct (IHa2 a2'); try (right; congruence); subst.
      left; reflexivity.
    - intros s l IHl t0.
      destruct t0; try (right; discriminate).
      destruct (Sym_t_as_MiniDecidableType.eq_dec s s0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      revert l0.
      induction IHl as [| [i a] l IHa _ IHl]; intros l0.
      + destruct l0; try (right; discriminate).
        left; reflexivity.
      + destruct l0; try (right; discriminate).
        destruct p as [i0 a0].
        destruct (Symbol_identifier_as_MiniDecidableType.eq_dec i i0) as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        destruct (IHa a0) as [E | ?]; try (right; congruence); subst.
        destruct (IHl l0) as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        left; reflexivity.
    - intros a s i IHa t0.
      destruct t0; try (right; discriminate).
      destruct (Symbol_identifier_as_MiniDecidableType.eq_dec i i0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (Sym_t_as_MiniDecidableType.eq_dec s s0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros a1 ct a2 IHa1 IHa2 t0.
      destruct t0; try (right; discriminate).
      destruct (SCtypes_as_MiniDecidableType.eq_dec ct t) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa1 a); try (right; congruence); subst.
      destruct (IHa2 a0); try (right; congruence); subst.
      left; reflexivity.
    - intros a1 a2 IHa1 IHa2 t0.
      destruct t0; try (right; discriminate).
      destruct (IHa1 a); try (right; congruence); subst.
      destruct (IHa2 a0); try (right; congruence); subst.
      left; reflexivity.
    - intros a IHa t0.
      destruct t0; try (right; discriminate).
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros ct t0.
      destruct t0; try (right; discriminate).
      destruct (SCtypes_as_MiniDecidableType.eq_dec ct t) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      left; reflexivity.
    - intros s i t0.
      destruct t0; try (right; discriminate).
      destruct (Symbol_identifier_as_MiniDecidableType.eq_dec i i0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (Sym_t_as_MiniDecidableType.eq_dec s s0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      left; reflexivity.
    - intros bt t0.
      destruct t0; try (right; discriminate).
      destruct (BasetTypes_t_as_MiniDecidableType.eq_dec bt t) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      left; reflexivity.
    - intros a1 a2 IHa1 IHa2 t0.
      destruct t0; try (right; discriminate).
      destruct (IHa1 a); try (right; congruence); subst.
      destruct (IHa2 a0); try (right; congruence); subst.
      left; reflexivity.
    - intros a IHa t0.
      destruct t0; try (right; discriminate).
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros a IHa t0.
      destruct t0; try (right; discriminate).
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros a1 a2 a3 IHa1 IHa2 IHa3 t0.
      destruct t0; try (right; discriminate).
      destruct (IHa1 a); try (right; congruence); subst.
      destruct (IHa2 a0); try (right; congruence); subst.
      destruct (IHa3 a4); try (right; congruence); subst.
      left; reflexivity.
    - intros a1 a2 a3 IHa1 IHa2 IHa3 t0.
      destruct t0; try (right; discriminate).
      destruct (IHa1 a); try (right; congruence); subst.
      destruct (IHa2 a0); try (right; congruence); subst.
      destruct (IHa3 a4); try (right; congruence); subst.
      left; reflexivity.
    - intros ct a IHa t0.
      destruct t0; try (right; discriminate).
      destruct (SCtypes_as_MiniDecidableType.eq_dec ct t) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros ct a IHa t0.
      destruct t0; try (right; discriminate).
      destruct (SCtypes_as_MiniDecidableType.eq_dec ct t) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros a1 a2 IHa1 IHa2 t0.
      destruct t0; try (right; discriminate).
      destruct (IHa1 a); try (right; congruence); subst.
      destruct (IHa2 a0); try (right; congruence); subst.
      left; reflexivity.
    - intros it a IHa t0.
      destruct t0; try (right; discriminate).
      destruct (IntegerType_as_MiniDecidableType.eq_dec it t) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros bt a IHa t0.
      destruct t0; try (right; discriminate).
      destruct (BasetTypes_t_as_MiniDecidableType.eq_dec bt t) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros a1 a2 a3 IHa1 IHa2 IHa3 t0.
      destruct t0; try (right; discriminate).
      destruct (IHa1 a); try (right; congruence); subst.
      destruct (IHa2 a0); try (right; congruence); subst.
      destruct (IHa3 a4); try (right; congruence); subst.
      left; reflexivity.
    - intros a1 a2 IHa1 IHa2 t0.
      destruct t0; try (right; discriminate).
      destruct (IHa1 a); try (right; congruence); subst.
      destruct (IHa2 a0); try (right; congruence); subst.
      left; reflexivity.
    - intros p a IHa t0.
      destruct t0; try (right; discriminate).
      destruct p as [s bt].
      destruct p0 as [s' bt'].
      destruct (Sym_t_as_MiniDecidableType.eq_dec s s') as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (BasetTypes_t_as_MiniDecidableType.eq_dec bt bt') as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros s l IHl t0.
      destruct t0; try (right; discriminate).
      destruct (Sym_t_as_MiniDecidableType.eq_dec s s0) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      revert l0.
      induction IHl as [| [t tt] l IHa _ IHl]; intros l0.
      + destruct l0; try (right; discriminate).
        left; reflexivity.
      + destruct l0; try (right; discriminate).
        destruct (IHa a) as [E | ?]; try (right; congruence).
        destruct a.
        inversion E; subst; clear E.
        destruct (IHl l0) as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        left; reflexivity.
    - intros p a IHa2 IHa1 t0.
      destruct t0; try (right; discriminate).
      destruct p as [s a2].
      destruct p0 as [s' a2'].
      cbn in *.
      destruct (Sym_t_as_MiniDecidableType.eq_dec s s') as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa1 a0); try (right; congruence); subst.
      destruct (IHa2 a2'); try (right; congruence); subst.
      left; reflexivity.
    - intros a1 l IHa IHl t0.
      destruct t0; try (right; discriminate).
      destruct (IHa a); try (right; congruence); subst.
      revert l0.
      induction IHl as [| [p a2] l IHa2 _ IHl]; intros l0.
      + destruct l0; try (right; discriminate).
        left; reflexivity.
      + destruct l0; try (right; discriminate).
        destruct p0 as [p' a2'].
        destruct (Pattern_as_MiniDecidableType.eq_dec p p') as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        destruct (IHa2 a2'); try (right; congruence); subst.
        destruct (IHl l0) as [E | ?]; try (right; congruence).
        inversion E; subst; clear E.
        left; reflexivity.
    - intros bt a IHa t0.
      destruct t0; try (right; discriminate).
      destruct (BasetTypes_t_as_MiniDecidableType.eq_dec bt t) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHa a0); try (right; congruence); subst.
      left; reflexivity.
    - intros t tt lc IHt a.
      destruct a; try (right; discriminate).
      destruct (Ty_as_MiniDecidableType.eq_dec tt t1); try (right; congruence); subst.
      destruct (Locations_t_as_MiniDecidableType.eq_dec lc t2) as [E | ?]; try (right; congruence).
      inversion E; subst; clear E.
      destruct (IHt t0); try (right; congruence); subst.
      left; reflexivity.
  Defined.

  Definition t := term ty.
  Definition eq := @eq t.
End Term_as_MiniDecidableType.

Module Annot_as_MiniDecidableType (Ty_as_MiniDecidableType : MiniDecidableType) <: MiniDecidableType.
  Module Term_as_MiniDecidableType := Term_as_MiniDecidableType Ty_as_MiniDecidableType.
  Definition ty := Ty_as_MiniDecidableType.t.
  Definition t := annot ty.
  Definition eq := @eq t.
  Lemma eq_dec : forall (x y : t), { eq x y } + { ~ eq x y }.
  Proof.
    unfold eq.
    intros [t1 tt1 lc1] [t2 tt2 lc2].
    destruct (Term_as_MiniDecidableType.eq_dec t1 t2); try (right; congruence); subst.
    destruct (Ty_as_MiniDecidableType.eq_dec tt1 tt2); try (right; congruence); subst.
    destruct (Locations_t_as_MiniDecidableType.eq_dec lc1 lc2) as [E | NE]; try (right; congruence).
    inversion E; subst; left; reflexivity.
  Defined.
End Annot_as_MiniDecidableType.
