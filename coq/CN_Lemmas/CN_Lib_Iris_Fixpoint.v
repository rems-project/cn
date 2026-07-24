  From iris.proofmode Require Import proofmode.
  From iris.bi.lib Require Import fixpoint_mono.

  (** [solve_mono_go] proves goals of the form [body[Φ] -∗ body[Ψ]] (after
    [iIntros "HF"], so: goal [body[Ψ]] with "HF" : [body[Φ]]) for bodies
    generated from the grammar

      P ::= ∃ x:A. P | l ↦ v | ⊤ | ⊥ | P ∗ Q | ⌜ϕ⌝ ∧ P | ⌜ϕ⌝
          | if b then P else Q | match e with C₁ xs₁ => P₁ | … | Cₙ xsₙ => Pₙ end
          | f(e)

    where [match] is over any NON-INDEXED inductive (plain parameters are
    fine; indexed families would need inversion rather than [destruct]) with
    a constant return type. Any number of branches is fine, as are compound
    patterns — nested (x :: y :: rest), or-patterns (C₁ | C₂ => P), and
    wildcards — since Rocq elaborates them to trees of single-level matches,
    which the destruct rule consumes one level at a time. The context must
    contain
      "Hmon" : □ (∀ y, Φ y -∗ Ψ y).

    Invariant: goal and "HF" have identical shape, differing only at
    recursive-call leaves (Ψ vs Φ). At each node:
    - Φ-free subterm (↦, ⊤, ⊥, ⌜ϕ⌝, or any subtree without f): [iExact]
    - recursive call f(e): apply "Hmon"
    - ∃ / ∗ / ∧: mirror the structure and recurse
    - match/if (incl. pattern-matching lambdas, which elaborate to matches):
      destruct the shared scrutinee; both sides reduce in lockstep. *)
Ltac solve_mono_go :=
  cbv beta iota zeta;
  first
    [ iExact "HF"
    | iApply "Hmon"; iExact "HF"
    | lazymatch goal with
      | |- environments.envs_entails _ (bi_exist _) =>
          let x := fresh "x" in
          iDestruct "HF" as (x) "HF"; iExists x; solve_mono_go
      | |- environments.envs_entails _ (bi_sep _ _) =>
          iDestruct "HF" as "[HF1 HF2]";
          iSplitL "HF1";
          [ iRename "HF1" into "HF" | iRename "HF2" into "HF" ];
          solve_mono_go
      | |- environments.envs_entails _ (bi_and _ _) =>
          iSplit;
          [ iDestruct "HF" as "[HF _]" | iDestruct "HF" as "[_ HF]" ];
          solve_mono_go
      | |- environments.envs_entails _ (match ?x with _ => _ end) =>
          destruct x; solve_mono_go
      | |- _ => fail "solve_mono_go: unsupported connective in body"
      end ].

(** Full [BiMonoPred] solver: [solve_bi_mono_pred F_pre] where [F_pre] is the
    pre-fixpoint functional. The non-expansiveness obligation first tries the
    discrete fast-path (valid whenever the domain is built from [leibnizO],
    [valO], [prodO], ...): an [n]-distance on a discrete OFE is an equality,
    so substitute and conclude by reflexivity. [solve_proper] can also handle
    fully discrete domains (via the [SolveProperSubrelation (dist n) (=)]
    instance, iris/algebra/ofe.v), but reaches the same conclusion only after
    backtracking typeclass search through the body; the fast-path is
    deterministic and independent of the body's shape. [solve_proper] remains
    as a fallback for domains with non-discrete components. *)
Ltac solve_bi_mono_pred F :=
  split;
  [ iIntros (Φ Ψ HΦ HΨ) "#Hmon %y HF"; unfold F; solve_mono_go
  | intros ? ?;
    first
      [ intros ? x1 x2 Hx;
        apply discrete in Hx; [ | apply _ ];
        apply leibniz_equiv in Hx; subst; reflexivity
      | solve_proper ] ].
