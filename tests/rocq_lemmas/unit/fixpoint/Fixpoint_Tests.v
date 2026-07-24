From iris.heap_lang Require Import proofmode notation.
From iris.bi.lib Require Import fixpoint_mono.
Require Import CN_Lemmas.CN_Lib_Iris_Fixpoint.

(** * Tests *)

Module FixpointTests.

  (** A custom datatype, as generated from a DSL datatype declaration. *)
  Inductive shape :=
    | SLeaf
    | SNum (n : Z)
    | SFlag (b : bool).
  Canonical Structure shapeO := leibnizO shape.

  Inductive tri := TA | TB | TC.
  Canonical Structure triO := leibnizO tri.

  (** For the mutual-recursion test: mutually recursive datatypes, and a
      custom argument type for the combined fixpoint whose constructor names
      match the predicate names. Each constructor carries that predicate's
      arguments directly, so no [prodO] uncurrying is needed. *)
  Inductive tree := TNode (n : Z) (ts : forest)
  with forest := FNil | FCons (t : tree) (ts : forest).
  Canonical Structure treeO := leibnizO tree.
  Canonical Structure forestO := leibnizO forest.

  Inductive tf :=
    | IsTree (v : val) (t : tree)
    | IsForest (v : val) (ts : forest).
  Canonical Structure tfO := leibnizO tf.

  Section tests.
    Context `{!heapGS Σ}.

    (** Test 1: linked list (if / ∃ / ⌜ϕ⌝ / ∗ / ↦ / f(e)). *)
    Definition is_list_pre (rec : prodO valO (leibnizO (list Z)) → iProp Σ)
        : prodO valO (leibnizO (list Z)) → iProp Σ := λ vl,
      (if bool_decide (vl.1 = NONEV) then ⌜vl.2 = []⌝
       else ∃ (p : loc) (x : Z) (l' : list Z) (w : val),
         ⌜vl.2 = x :: l'⌝ ∗ ⌜vl.1 = SOMEV #p⌝ ∗ p ↦ (#x, w)%V ∗ rec (w, l'))%I.

    Local Instance is_list_pre_mono : BiMonoPred is_list_pre.
    Proof. solve_bi_mono_pred is_list_pre. Qed.

    (** Test 2: ⊤, ⊥, plain bool if, nested ifs, ∧ with a pure proposition,
        multiple recursive calls. *)
    Definition gnarly_pre
        (rec : prodO (leibnizO bool) (prodO valO (leibnizO (list Z))) → iProp Σ)
        : prodO (leibnizO bool) (prodO valO (leibnizO (list Z))) → iProp Σ := λ a,
      (if a.1
       then ⌜a.2.2 = []⌝ ∗ True
       else if bool_decide (a.2.1 = NONEV)
            then False
            else ∃ (p : loc) (w : val) (x : Z) (l' : list Z),
              ⌜a.2.2 = x :: l'⌝ ∗ p ↦ w ∗
              (⌜x = 0%Z⌝ ∧ rec (true, (w, l'))) ∗
              rec (false, (w, l')))%I.

    Local Instance gnarly_pre_mono : BiMonoPred gnarly_pre.
    Proof. solve_bi_mono_pred gnarly_pre. Qed.

    (** Test 3: pattern-matching lambdas, including a nested pattern
        (these elaborate to matches on pairs). *)
    Definition plist_pre
        (rec : prodO valO (prodO (leibnizO (list Z)) (leibnizO bool)) → iProp Σ)
        : prodO valO (prodO (leibnizO (list Z)) (leibnizO bool)) → iProp Σ :=
      λ '(v, (l, strict)),
      (if bool_decide (v = NONEV) then (if strict then ⌜l = []⌝ else True)
       else ∃ (p : loc) (x : Z) (l' : list Z) (w : val),
         ⌜l = x :: l'⌝ ∗ ⌜v = SOMEV #p⌝ ∗ p ↦ (#x, w)%V ∗ rec (w, (l', strict)))%I.

    Local Instance plist_pre_mono : BiMonoPred plist_pre.
    Proof. solve_bi_mono_pred plist_pre. Qed.

    (** Test 4: match over a custom datatype, with a projection as scrutinee
        (a shape [f_equiv]'s match rules cannot decompose structurally — the
        non-expansiveness proof goes through only because the domain is
        discrete), recursive calls in some branches only, and a nested if
        inside a branch. *)
    Definition shape_pre (rec : prodO valO shapeO → iProp Σ)
        : prodO valO shapeO → iProp Σ := λ a,
      (match a.2 with
       | SLeaf => ⌜a.1 = NONEV⌝
       | SNum n => ∃ (p : loc) (w : val),
           ⌜a.1 = SOMEV #p⌝ ∗ p ↦ (#n, w)%V ∗ rec (w, SLeaf)
       | SFlag b => if b then True else rec (a.1, SLeaf) ∗ False
       end)%I.

    Local Instance shape_pre_mono : BiMonoPred shape_pre.
    Proof. solve_bi_mono_pred shape_pre. Qed.

    (** Test 5: compound patterns — deep patterns ([x], x :: y :: rest),
        an or-pattern (TA | TB), and a wildcard default. These elaborate to
        nested single-level matches, handled by repeated destructs. *)
    Definition deep_pre
        (rec : prodO valO (prodO (leibnizO (list Z)) triO) → iProp Σ)
        : prodO valO (prodO (leibnizO (list Z)) triO) → iProp Σ := λ a,
      (match a.2.1 with
       | [] => ⌜a.1 = NONEV⌝
       | [x] => ∃ (p : loc), ⌜a.1 = SOMEV #p⌝ ∗ p ↦ #x
       | x :: y :: rest =>
           (match a.2.2 with
            | TA | TB => ⌜x = y⌝ ∗ rec (a.1, (rest, TC))
            | _ => rec (a.1, (y :: rest, TA))
            end)
       end)%I.

    Local Instance deep_pre_mono : BiMonoPred deep_pre.
    Proof. solve_bi_mono_pred deep_pre. Qed.

    (** Test 6: mutual recursion via a single fixpoint over a custom
        dispatch type.

          is_tree v (TNode n ts) = ∃ p w, ⌜v = #p⌝ ∗ p ↦ (#n, w) ∗ is_forest w ts
          is_forest v FNil       = ⌜v = NONE⌝
          is_forest v (FCons t ts) = ∃ p w1 w2, ⌜v = SOME #p⌝ ∗ p ↦ (w1, w2) ∗
                                     is_tree w1 t ∗ is_forest w2 ts

        (This particular pair is structurally decreasing on tree/forest, so a
        mutual [Fixpoint] would also work; it is used here to demonstrate the
        general encoding, which applies even when recursion is not
        structural.) *)
    Definition tf_pre (rec : tfO → iProp Σ) : tfO → iProp Σ := λ a,
      (match a with
       | IsTree v t =>
           match t with
           | TNode n ts => ∃ (p : loc) (w : val),
               ⌜v = #p⌝ ∗ p ↦ (#n, w)%V ∗ rec (IsForest w ts)
           end
       | IsForest v ts =>
           match ts with
           | FNil => ⌜v = NONEV⌝
           | FCons t ts' => ∃ (p : loc) (w1 w2 : val),
               ⌜v = SOMEV #p⌝ ∗ p ↦ (w1, w2)%V ∗
               rec (IsTree w1 t) ∗ rec (IsForest w2 ts')
           end
       end)%I.

    Local Instance tf_pre_mono : BiMonoPred tf_pre.
    Proof. solve_bi_mono_pred tf_pre. Qed.

    (** Wrapper layer: per-predicate definitions and unfold lemmas that hide
        the dispatch type from users. *)
    Definition is_tree (v : val) (t : tree) : iProp Σ :=
      bi_least_fixpoint tf_pre (IsTree v t).
    Definition is_forest (v : val) (ts : forest) : iProp Σ :=
      bi_least_fixpoint tf_pre (IsForest v ts).

    Lemma is_tree_unfold v n ts :
      is_tree v (TNode n ts) ⊣⊢
        ∃ (p : loc) (w : val), ⌜v = #p⌝ ∗ p ↦ (#n, w)%V ∗ is_forest w ts.
    Proof. rewrite /is_tree least_fixpoint_unfold //. Qed.

    Lemma is_forest_unfold v ts :
      is_forest v ts ⊣⊢
        match ts with
        | FNil => ⌜v = NONEV⌝
        | FCons t ts' => ∃ (p : loc) (w1 w2 : val),
            ⌜v = SOMEV #p⌝ ∗ p ↦ (w1, w2)%V ∗ is_tree w1 t ∗ is_forest w2 ts'
        end.
    Proof. rewrite /is_forest least_fixpoint_unfold //. Qed.

    (** Derived mutual induction principle, from [least_fixpoint_iter] at the
        combined motive [match a with IsTree v t => Φt v t | ... end]. *)
    Lemma is_tree_forest_ind (Φt : val → tree → iProp Σ)
        (Φf : val → forest → iProp Σ) :
      □ (∀ v (n : Z) ts, (∃ (p : loc) (w : val),
           ⌜v = #p⌝ ∗ p ↦ (#n, w)%V ∗ Φf w ts) -∗ Φt v (TNode n ts)) -∗
      □ (∀ v, ⌜v = NONEV⌝ -∗ Φf v FNil) -∗
      □ (∀ v t ts, (∃ (p : loc) (w1 w2 : val),
           ⌜v = SOMEV #p⌝ ∗ p ↦ (w1, w2)%V ∗ Φt w1 t ∗ Φf w2 ts) -∗
         Φf v (FCons t ts)) -∗
      (∀ v t, is_tree v t -∗ Φt v t) ∧ (∀ v ts, is_forest v ts -∗ Φf v ts).
    Proof.
      iIntros "#Ht #Hfnil #Hfcons".
      set (Φ := (λ a, match a with
                      | IsTree v t => Φt v t
                      | IsForest v ts => Φf v ts
                      end)%I : tfO → iProp Σ).
      assert (NonExpansive Φ) as HΦne.
      { intros n x1 x2 Hx.
        apply discrete in Hx; [ | apply _ ].
        apply leibniz_equiv in Hx; subst; reflexivity. }
      iAssert (∀ a, bi_least_fixpoint tf_pre a -∗ Φ a)%I as "H".
      { iApply least_fixpoint_iter.
        iIntros "!>" (a) "Hpre".
        destruct a as [v t|v ts]; [destruct t as [n ts]|destruct ts as [|t ts']];
          simpl.
        - iApply "Ht". done.
        - by iApply "Hfnil".
        - by iApply "Hfcons". }
      iSplit.
      - iIntros (v t) "HP". iApply ("H" $! (IsTree v t) with "HP").
      - iIntros (v ts) "HP". iApply ("H" $! (IsForest v ts) with "HP").
    Qed.

    (** Test 7: the non-structural variant of Test 6. [is_forest'] tests
        whether the value is null: if so the forest is asserted to be [FNil];
        otherwise its components are existentially quantified and pinned by a
        pure equation ([∃ t' ts', ⌜ts = FCons t' ts'⌝ ∗ …]). The recursive
        calls are on the existentially bound [t'], [ts'] — related to [ts]
        only through the pure equation — so no structural-decrease guard can
        accept this system as a mutual [Fixpoint]; it genuinely needs the
        least fixpoint. (The [IsTree] branch is unchanged from Test 6,
        demonstrating that the two styles mix freely.) *)
    Definition tf_pre' (rec : tfO → iProp Σ) : tfO → iProp Σ := λ a,
      (match a with
       | IsTree v t =>
           match t with
           | TNode n ts => ∃ (p : loc) (w : val),
               ⌜v = #p⌝ ∗ p ↦ (#n, w)%V ∗ rec (IsForest w ts)
           end
       | IsForest v ts =>
           if bool_decide (v = NONEV) then ⌜ts = FNil⌝
           else ∃ (t' : tree) (ts' : forest), ⌜ts = FCons t' ts'⌝ ∗
             ∃ (p : loc) (w1 w2 : val),
               ⌜v = SOMEV #p⌝ ∗ p ↦ (w1, w2)%V ∗
               rec (IsTree w1 t') ∗ rec (IsForest w2 ts')
       end)%I.

    Local Instance tf_pre'_mono : BiMonoPred tf_pre'.
    Proof. solve_bi_mono_pred tf_pre'. Qed.

    Definition is_tree' (v : val) (t : tree) : iProp Σ :=
      bi_least_fixpoint tf_pre' (IsTree v t).
    Definition is_forest' (v : val) (ts : forest) : iProp Σ :=
      bi_least_fixpoint tf_pre' (IsForest v ts).

    Lemma is_tree'_unfold v n ts :
      is_tree' v (TNode n ts) ⊣⊢
        ∃ (p : loc) (w : val), ⌜v = #p⌝ ∗ p ↦ (#n, w)%V ∗ is_forest' w ts.
    Proof. rewrite /is_tree' least_fixpoint_unfold //. Qed.

    Lemma is_forest'_unfold v ts :
      is_forest' v ts ⊣⊢
        if bool_decide (v = NONEV) then ⌜ts = FNil⌝
        else ∃ (t' : tree) (ts' : forest), ⌜ts = FCons t' ts'⌝ ∗
          ∃ (p : loc) (w1 w2 : val),
            ⌜v = SOMEV #p⌝ ∗ p ↦ (w1, w2)%V ∗
            is_tree' w1 t' ∗ is_forest' w2 ts'.
    Proof. rewrite /is_forest' least_fixpoint_unfold //. Qed.

    Lemma is_tree_forest'_ind (Φt : val → tree → iProp Σ)
        (Φf : val → forest → iProp Σ) :
      □ (∀ v (n : Z) ts, (∃ (p : loc) (w : val),
           ⌜v = #p⌝ ∗ p ↦ (#n, w)%V ∗ Φf w ts) -∗ Φt v (TNode n ts)) -∗
      □ (∀ v ts,
           (if bool_decide (v = NONEV) then ⌜ts = FNil⌝
            else ∃ (t' : tree) (ts' : forest), ⌜ts = FCons t' ts'⌝ ∗
              ∃ (p : loc) (w1 w2 : val),
                ⌜v = SOMEV #p⌝ ∗ p ↦ (w1, w2)%V ∗ Φt w1 t' ∗ Φf w2 ts') -∗
           Φf v ts) -∗
      (∀ v t, is_tree' v t -∗ Φt v t) ∧ (∀ v ts, is_forest' v ts -∗ Φf v ts).
    Proof.
      iIntros "#Ht #Hf".
      set (Φ := (λ a, match a with
                      | IsTree v t => Φt v t
                      | IsForest v ts => Φf v ts
                      end)%I : tfO → iProp Σ).
      assert (NonExpansive Φ) as HΦne.
      { intros n x1 x2 Hx.
        apply discrete in Hx; [ | apply _ ].
        apply leibniz_equiv in Hx; subst; reflexivity. }
      iAssert (∀ a, bi_least_fixpoint tf_pre' a -∗ Φ a)%I as "H".
      { iApply least_fixpoint_iter.
        iIntros "!>" (a) "Hpre".
        destruct a as [v t|v ts]; [destruct t as [n ts]|]; simpl.
        - iApply "Ht". done.
        - by iApply "Hf". }
      iSplit.
      - iIntros (v t) "HP". iApply ("H" $! (IsTree v t) with "HP").
      - iIntros (v ts) "HP". iApply ("H" $! (IsForest v ts) with "HP").
    Qed.

    (** The fixpoints and their unfolding lemmas now come for free. *)
    Definition is_list (v : val) (l : list Z) : iProp Σ :=
      bi_least_fixpoint is_list_pre (v, l).

    Lemma is_list_unfold v l :
      is_list v l ⊣⊢ is_list_pre (λ vl, is_list vl.1 vl.2) (v, l).
    Proof. rewrite /is_list least_fixpoint_unfold //. Qed.

    Definition is_shape (v : val) (s : shape) : iProp Σ :=
      bi_least_fixpoint shape_pre (v, s).

    Lemma is_shape_unfold v s :
      is_shape v s ⊣⊢ shape_pre (λ a, is_shape a.1 a.2) (v, s).
    Proof. rewrite /is_shape least_fixpoint_unfold //. Qed.
  End tests.
End FixpointTests.
