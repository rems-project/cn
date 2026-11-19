import Iris.BI.Lib.Fixpoint
import Iris.BI.BIBase
import Iris.Algebra.OFE
import Iris.ProofMode
import Iris.HeapLang.ProofMode
import Iris.HeapLang.Notation
import CN_Lib.CN_Lib_Iris_Fixpoint

open Iris HeapLang ProofMode

-- A custom datatype, as generated from a DSL datatype declaration.
inductive shape where
  | SLeaf
  | SNum (n : Int)
  | SFlag (b : Bool)

abbrev shapeO := DiscreteO shape

inductive tri where | TA | TB | TC
abbrev triO := DiscreteO tri

-- For the mutual-recursion test: mutually recursive datatypes, and a
-- custom argument type for the combined fixpoint whose constructor names
-- match the predicate names.
mutual
inductive tree where
  | TNode (n : Int) (ts : forest)
inductive forest where
  | FNil
  | FCons (t : tree) (ts : forest)
end

abbrev treeO := DiscreteO tree
abbrev forestO := DiscreteO forest

inductive tf where
  | IsTree (v : Val) (t : tree)
  | IsForest (v : Val) (ts : forest)

abbrev tfO := DiscreteO tf

abbrev valO := DiscreteO Val

open shape tri tree forest tf

-- Notations for option values in HeapLang
--  (Doesn't seem to be implemented in iris-lean yet?)
abbrev NONE := (Exp.injL (Val.lit BaseLit.unit))
abbrev NONEV := (Val.injL (Val.lit BaseLit.unit))
abbrev SOME (x : Val) := (Exp.injR x)
abbrev SOMEV (x : Val) := (Val.injR x)
deriving instance DecidableEq for DiscreteO

variable {hlc} {GF} [HeapLangGS hlc GF]

-- Test 1: linked list (if / ∃ / ⌜ϕ⌝ / ∗ / ↦ / f(e)).
def is_list_pre (f : valO × (DiscreteO (List Int)) → IProp GF) : valO × (DiscreteO (List Int)) → IProp GF :=
  iprop% fun vl =>
    if (decide (vl.1 = ⟨NONEV⟩)) then ⌜vl.2 = ⟨[]⟩⌝
    else ∃ (p : Loc) (x : Int) (l' : List Int) (w : Val),
      ⌜vl.2 = ⟨x :: l'⟩⌝ ∗ ⌜vl.1 = ⟨SOMEV (Val.lit p)⟩⌝
      ∗ p ↦ Val.pair (Val.lit x) w ∗ f (⟨w⟩, ⟨l'⟩)

instance is_list_pre_mono [HeapLangGS hlc GF] :
    BIMonoPred (is_list_pre (hlc := hlc) (GF := GF)) :=
by solve_bi_mono_pred is_list_pre

--  Test 2: ⊤, ⊥, plain bool if, nested ifs, ∧ with a pure proposition, multiple recursive calls.
def gnarly_pre (f : (DiscreteO Bool) × (valO × DiscreteO (List Int)) → IProp GF) : (DiscreteO Bool) × (valO × DiscreteO (List Int)) → IProp GF :=
  iprop% fun a =>
    if a.1 = ⟨true⟩
      then ⌜a.2.2 = ⟨[]⟩⌝ ∗ True
      else if (a.2.1 = ⟨NONEV⟩)
        then False
        else ∃ (p : Loc) (w : Val) (x : Int) (l' : List Int),
          ⌜a.2.2 = ⟨x :: l'⟩⌝ ∗ p ↦ w ∗
          (⌜x = 0⌝ ∧ f (⟨true⟩, (⟨w⟩, ⟨l'⟩))) ∗
          f (⟨false⟩, (⟨w⟩, ⟨l'⟩))

instance gnarly_pre_mono [HeapLangGS hlc GF] :
    BIMonoPred (gnarly_pre (hlc := hlc) (GF := GF)) :=
by solve_bi_mono_pred gnarly_pre

-- Test 3: pattern-matching lambdas, including a nested pattern
--         (these elaborate to matches on pairs).
def plist_pre (f : valO × (DiscreteO (List Int)) × (DiscreteO Bool) → IProp GF) : valO × (DiscreteO (List Int)) × (DiscreteO Bool) → IProp GF :=
  iprop% fun (v, (l, strict)) =>
    if (v = ⟨NONEV⟩) then (if strict = ⟨true⟩ then ⌜l = ⟨[]⟩⌝
      else True)
        else ∃ (p : Loc) (x : Int) (l' : List Int) (w : Val),
          ⌜l = ⟨x :: l'⟩⌝ ∗ ⌜v = ⟨SOMEV (Val.lit p)⟩⌝ ∗ p ↦ Val.pair (Val.lit x) w ∗ f (⟨w⟩, (⟨l'⟩, strict))

instance plist_pre_mono [HeapLangGS hlc GF] :
    BIMonoPred (plist_pre (hlc := hlc) (GF := GF)) :=
by solve_bi_mono_pred plist_pre

-- Test 4: match over a custom datatype, with a projection as scrutinee
--         (a shape [f_equiv]'s match rules cannot decompose structurally — the
--         non-expansiveness proof goes through only because the domain is
--         discrete), recursive calls in some branches only, and a nested if
--         inside a branch.

def shape_pre (f : valO × shapeO → IProp GF) : valO × shapeO → IProp GF :=
  iprop% fun a =>
    match a.2 with
    | ⟨SLeaf⟩ => ⌜a.1 = ⟨NONEV⟩⌝
    | ⟨SNum n⟩ => ∃ (p : Loc) (w : Val),
        ⌜a.1 = ⟨SOMEV (Val.lit p)⟩⌝ ∗ p ↦ Val.pair (Val.lit n) w ∗ f (⟨w⟩, ⟨SLeaf⟩)
    | ⟨SFlag b⟩ => if b then True else f (a.1, ⟨SLeaf⟩) ∗ False

instance shape_pre_mono [HeapLangGS hlc GF] :
    BIMonoPred (shape_pre (hlc := hlc) (GF := GF)) :=
by solve_bi_mono_pred shape_pre

-- Test 5: compound patterns — deep patterns ([x], x :: y :: rest),
--         an or-pattern (TA | TB), and a wildcard default. These elaborate to
--         nested single-level matches, handled by repeated destructs.

@[simp]
def deep_pre (f : valO × (DiscreteO (List Int) × triO) → IProp GF) : valO × (DiscreteO (List Int) × triO) → IProp GF :=
  iprop% fun a =>
    match a.2.1 with
    | ⟨[]⟩ => ⌜a.1 = ⟨NONEV⟩⌝
    | ⟨[x]⟩ => ∃ (p : Loc), ⌜a.1 = ⟨SOMEV (Val.lit p)⟩⌝ ∗ p ↦ Val.lit x
    | ⟨x :: y :: rest⟩ =>
        match a.2.2 with
        | ⟨TA⟩ | ⟨TB⟩ => ⌜x = y⌝ ∗ f (a.1, (⟨rest⟩, ⟨TC⟩))
        | _ => f (a.1, (⟨y :: rest⟩, ⟨TA⟩))

instance deep_pre_mono [HeapLangGS hlc GF] :
    BIMonoPred (deep_pre (hlc := hlc) (GF := GF)) :=
by solve_bi_mono_pred deep_pre


/-
Test 6: mutual recursion via a single fixpoint over a custom
        dispatch type.

          is_tree v (TNode n ts) = ∃ p w, ⌜v = #p⌝ ∗ p ↦ (#n, w) ∗ is_forest w ts
          is_forest v FNil       = ⌜v = NONE⌝
          is_forest v (FCons t ts) = ∃ p w1 w2, ⌜v = SOME #p⌝ ∗ p ↦ (w1, w2) ∗
                                     is_tree w1 t ∗ is_forest w2 ts

        (This particular pair is structurally decreasing on tree/forest, so a
        mutual [Fixpoint] would also work; it is used here to demonstrate the
        general encoding, which applies even when recursion is not
        structural.)
       Note: the above is a Neel comment from the Rocq version of this file.
           I think it might not be applicable here. -/

@[simp]
def tf_pre (f : tfO → IProp GF) : tfO → IProp GF := iprop% fun a =>
  match a with
  | ⟨IsTree v t⟩ =>
      match t with
      | TNode n ts => ∃ (p : Loc) (w : Val),
          ⌜v = Val.lit p⌝ ∗ p ↦ Val.pair (Val.lit n) w ∗ f (⟨IsForest w ts⟩)
  | ⟨IsForest v ts⟩ =>
      match ts with
      | FNil => ⌜v = NONEV⌝
      | FCons t ts' => ∃ (p : Loc) (w1 w2 : Val),
          ⌜v = SOMEV (Val.lit p)⌝ ∗ p ↦ Val.pair w1 w2 ∗
          f (⟨IsTree w1 t⟩) ∗ f (⟨IsForest w2 ts'⟩)

instance tf_pre_mono [HeapLangGS hlc GF] :
    BIMonoPred (tf_pre (hlc := hlc) (GF := GF)) :=
by solve_bi_mono_pred tf_pre

--  Wrapper layer: per-predicate definitions and unfold lemmas that hide
--        the dispatch type from users.
@[simp]
def is_tree (v : Val) (t : tree) : IProp GF :=
  bi_least_fixpoint tf_pre ⟨IsTree v t⟩
@[simp]
def is_forest (v : Val) (ts : forest) : IProp GF :=
  bi_least_fixpoint tf_pre ⟨IsForest v ts⟩

theorem is_tree_unfold v n ts :
  is_tree v (TNode n ts) ⊣⊢
    ∃ (p : Loc) (w : Val), ⌜v = Val.lit p⌝ ∗ p ↦ Val.pair (Val.lit n) w ∗ is_forest w ts :=
by
  -- Todo: why do I have to apply this manually??
  apply Iris.BI.BiEntails.of_eq
  rw [is_tree, least_fixpoint_unfold]
  rfl

theorem is_tree_unfold2 v n ts :
  is_tree v (TNode n ts) = iprop(∃ (p : Loc) (w : Val), ⌜v = Val.lit p⌝ ∗ p ↦ some hl_val((#n, &w)) ∗ is_forest w ts) :=
by
  rw [is_tree, least_fixpoint_unfold]
  rfl

theorem is_forest_unfold v ts :
      is_forest v ts ⊣⊢
        match ts with
        | FNil => ⌜v = NONEV⌝
        | FCons t ts' => ∃ (p : Loc) (w1 w2 : Val),
            ⌜v = SOMEV (Val.lit p)⌝ ∗ p ↦ Val.pair w1 w2 ∗ is_tree w1 t ∗ is_forest w2 ts' :=
by
  apply Iris.BI.BiEntails.of_eq
  rw [is_forest, least_fixpoint_unfold]
  rfl

-- Derived mutual induction principle, from [least_fixpoint_iter] at the
--         combined motive [match a with IsTree v t => Φt v t | ... ].
theorem is_tree_forest_ind (Φt : Val → tree → IProp GF)
    (Φf : Val → forest → IProp GF) :
  □ (∀ v (n : Int) ts, (∃ (p : Loc) (w : Val),
        ⌜v = Val.lit p⌝ ∗ p ↦ Val.pair (Val.lit n) w ∗ Φf w ts) -∗ Φt v (TNode n ts)) -∗
  □ (∀ v, ⌜v = NONEV⌝ -∗ Φf v FNil) -∗
  □ (∀ v t ts, (∃ (p : Loc) (w1 w2 : Val),
        ⌜v = SOMEV (Val.lit p)⌝ ∗ p ↦ Val.pair w1 w2 ∗ Φt w1 t ∗ Φf w2 ts) -∗
      Φf v (FCons t ts)) -∗
  (∀ v t, is_tree v t -∗ Φt v t) ∧ (∀ v ts, is_forest v ts -∗ Φf v ts) :=
by
  iintro #Ht #Hfnil #Hfcons
  simp
  let Φ : tfO → IProp GF := iprop% fun a =>
    match a with
    | ⟨IsTree v t⟩ => Φt v t
    | ⟨IsForest v ts⟩ => Φf v ts
  have HΦne : OFE.NonExpansive Φ := by
    constructor
    intro n x1 x2 Hx
    have Hx := OFE.Discrete.discrete Hx
    subst Hx; rfl
  ihave H : ∀ a, bi_least_fixpoint tf_pre a -∗ Φ a $$ []
  · iapply least_fixpoint_iter
    iintro !> %a ⟨Hpre⟩
    rcases a with ⟨v, t⟩ | ⟨v, ts⟩
    · rcases t with ⟨n, ts⟩
      simp; iapply Ht; itrivial
    · rcases ts with _ | ⟨t, ts'⟩
      · simp; iapply Hfnil; itrivial
      · simp; iapply Hfcons; itrivial
  · isplit
    · iintro %v %t ⟨HP⟩
      iapply H $$ %⟨IsTree v t⟩; itrivial
    · iintro %v %ts ⟨HP⟩
      iapply H $$ %⟨IsForest v ts⟩; itrivial

-- The same public mutual induction principle, proved by the solver used
--         by generated resource-predicate groups.
theorem is_tree_forest_ind_solver (Φt : Val → tree → IProp GF)
    (Φf : Val → forest → IProp GF) :
  □ (∀ v t,
      (match t with
        | TNode n ts => ∃ (p : Loc) (w : Val),
            ⌜v = Val.lit p⌝ ∗ p ↦ Val.pair (Val.lit n) w ∗ Φf w ts
        ) -∗ Φt v t) -∗
  □ (∀ v ts,
      (match ts with
        | FNil => ⌜v = NONEV⌝
        | FCons t ts' => ∃ (p : Loc) (w1 w2 : Val),
            ⌜v = SOMEV (Val.lit p)⌝ ∗ p ↦ Val.pair w1 w2 ∗
            Φt w1 t ∗ Φf w2 ts'
        ) -∗ Φf v ts) -∗
  (∀ v t, is_tree v t -∗ Φt v t) ∧
  (∀ v ts, is_forest v ts -∗ Φf v ts) :=
  by
    iintro #Ht #Hf
    solve_cn_predicate_induction
      tf_pre,
      ((fun a => match a with
        | ⟨IsTree v t⟩ => Φt v t
        | ⟨IsForest v ts⟩ => Φf v ts) : tfO → IProp GF),
      [Ht, Hf]

-- Test 7: the non-structural variant of Test 6. [is_forest'] tests
--         whether the value is null: if so the forest is asserted to be [FNil];
--         otherwise its components are existentially quantified and pinned by a
--         pure equation ([∃ t' ts', ⌜ts = FCons t' ts'⌝ ∗ …]). The recursive
--         calls are on the existentially bound [t'], [ts'] — related to [ts]
--         only through the pure equation — so no structural-decrease guard can
--         accept this system as a mutual [Fixpoint]; it genuinely needs the
--         least fixpoint. (The [IsTree] branch is unchanged from Test 6,
--         demonstrating that the two styles mix freely.)
-- Note: Don't have to worry about [Fixpoint]s in Lean, I think
@[simp]
def tf_pre' (f : tfO → IProp GF) : tfO → IProp GF := iprop% fun a =>
  match a with
    | ⟨IsTree v t⟩ =>
        match t with
        | TNode n ts => ∃ (p : Loc) (w : Val),
            ⌜v = Val.lit p⌝ ∗ p ↦ Val.pair (Val.lit n) w ∗ f ⟨IsForest w ts⟩
    | ⟨IsForest v ts⟩ =>
        if v = NONEV then ⌜ts = FNil⌝
        else ∃ (t' : tree) (ts' : forest), ⌜ts = FCons t' ts'⌝ ∗
          ∃ (p : Loc) (w1 w2 : Val),
            ⌜v = SOMEV (Val.lit p)⌝ ∗ p ↦ Val.pair w1 w2 ∗
            f ⟨IsTree w1 t'⟩ ∗ f ⟨IsForest w2 ts'⟩

instance tf_pre'_mono [HeapLangGS hlc GF] :
    BIMonoPred (tf_pre' (hlc := hlc) (GF := GF)) :=
by solve_bi_mono_pred tf_pre'

@[reducible]
def is_tree' (v : Val) (t : tree) : IProp GF :=
  bi_least_fixpoint tf_pre' ⟨IsTree v t⟩
@[reducible]
def is_forest' (v : Val) (ts : forest) : IProp GF :=
  bi_least_fixpoint tf_pre' ⟨IsForest v ts⟩

theorem is_tree'_unfold v n ts :
  is_tree' v (TNode n ts) ⊣⊢
    ∃ (p : Loc) (w : Val), ⌜v = Val.lit p⌝ ∗
      p ↦ Val.pair (Val.lit n) w ∗ is_forest' w ts :=
by
  apply Iris.BI.BiEntails.of_eq
  rw [is_tree', least_fixpoint_unfold]
  rfl

theorem is_forest'_unfold v ts :
  is_forest' v ts ⊣⊢
    if (v = NONEV) then ⌜ts = FNil⌝
    else ∃ (t' : tree) (ts' : forest), ⌜ts = FCons t' ts'⌝ ∗
      ∃ (p : Loc) (w1 w2 : Val),
        ⌜v = SOMEV (Val.lit p)⌝ ∗ p ↦ Val.pair w1 w2 ∗
        is_tree' w1 t' ∗ is_forest' w2 ts' :=
by
  apply Iris.BI.BiEntails.of_eq
  rw [is_forest', least_fixpoint_unfold]
  rfl

theorem is_tree_forest'_ind (Φt : Val → tree → IProp GF)
    (Φf : Val → forest → IProp GF) :
  □ (∀ v (n : Int) ts, (∃ (p : Loc) (w : Val),
        ⌜v = Val.lit p⌝ ∗ p ↦ Val.pair (Val.lit n) w ∗ Φf w ts) -∗ Φt v (TNode n ts)) -∗
  □ (∀ v ts,
        (if v = NONEV then ⌜ts = FNil⌝
        else ∃ (t' : tree) (ts' : forest), ⌜ts = FCons t' ts'⌝ ∗
          ∃ (p : Loc) (w1 w2 : Val),
            ⌜v = SOMEV (Val.lit p)⌝ ∗ p ↦ Val.pair w1 w2 ∗ Φt w1 t' ∗ Φf w2 ts') -∗
        Φf v ts) -∗
  (∀ v t, is_tree' v t -∗ Φt v t) ∧ (∀ v ts, is_forest' v ts -∗ Φf v ts) :=
by
  iintro #Ht #Hf
  let Φ : tfO → IProp GF := fun a =>
    match a with
      | ⟨IsTree v t⟩ => Φt v t
      | ⟨IsForest v ts⟩ => Φf v ts
  have HΦne : OFE.NonExpansive Φ := by
    constructor
    intro n x1 x2 Hx
    have Hx := OFE.Discrete.discrete Hx
    subst Hx; rfl
  ihave H : ∀ a, bi_least_fixpoint tf_pre' a -∗ Φ a $$ []
  · iapply least_fixpoint_iter
    iintro !> %a ⟨Hpre⟩
    rcases a with ⟨v, t⟩ | ⟨v, ts⟩
    · rcases t with ⟨n, ts⟩
      simp; iapply Ht ; itrivial
    · simp; iapply Hf ; itrivial
  · isplit
    · iintro %v %t ⟨HP⟩
      iapply H $$ %⟨IsTree v t⟩; itrivial
    · iintro %v %ts ⟨HP⟩
      iapply H $$ %⟨IsForest v ts⟩; itrivial

-- The fixpoints and their unfolding lemmas now come for free.'
@[simp]
def is_list (v : Val) (l : List Int) : IProp GF :=
  bi_least_fixpoint is_list_pre (⟨v⟩, ⟨l⟩)

-- Singleton-group variant of the generated induction principle.
theorem is_list_ind_solver (Φ : Val → List Int → IProp GF) :
  □ (∀ v l,
    is_list_pre (fun (⟨v1⟩, ⟨v2⟩) => Φ v1 v2) (⟨v⟩, ⟨l⟩) -∗ Φ v l) -∗
  ∀ v l, is_list v l -∗ Φ v l :=
by
  -- TODO: Surely the iintro can happen in the tactic.
  iintro #Hstep
  solve_cn_predicate_induction
    is_list_pre,
    ((fun (⟨v1⟩ , ⟨v2⟩) => Φ v1 v2) :
        valO × (DiscreteO (List Int)) → IProp GF),
    [Hstep]

theorem is_list_unfold v l :
  is_list (hlc := hlc) (GF := GF) v l
    ⊣⊢
  is_list_pre (fun (⟨v1⟩, ⟨v2⟩) => is_list v1 v2) (⟨v⟩, ⟨l⟩) :=
by
  apply Iris.BI.BiEntails.of_eq
  rw [is_list, least_fixpoint_unfold]
  simp

@[simp]
def is_shape (v : Val) (s : shape) : IProp GF :=
  bi_least_fixpoint shape_pre (⟨v⟩, ⟨s⟩)

theorem is_shape_unfold v s :
  is_shape (hlc := hlc) (GF := GF) v s
    ⊣⊢
  shape_pre (fun (⟨a1⟩, ⟨a2⟩) => is_shape a1 a2) (⟨v⟩, ⟨s⟩) :=
by
  apply Iris.BI.BiEntails.of_eq
  rw [is_shape, least_fixpoint_unfold]
  simp
