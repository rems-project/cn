import Iris.Proofmode.Tactics
import Iris.BI.Lib.Fixpoint
import Qq
import Lean
import Batteries.Tactic.SeqFocus

open Lean Elab Tactic Qq
namespace Iris.ProofMode

/-
[solve_mono_go] proves goals of the form [body[Φ] -∗ body[Ψ]] (after
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
- Φ-free subterm (↦, ⊤, ⊥, ⌜ϕ⌝, or any subtree without f): [iexact]
- recursive call f(e): iapply Hmon
- ∃ / ∗ / ∧: mirror the structure and recurse
- match/if (incl. pattern-matching lambdas, which elaborate to matches):
  destruct the shared scrutinee; both sides reduce in lockstep.
-/

def solve_mono (HF : Ident): TacticM Unit := withMainContext do
  let goal ← getMainGoal
  goal.withContext do
  let ty : Q(Prop) ← goal.getType
  let ~q(@ProofMode.Entails' _ $h _ $body) := ty
  | throwError "solve_mono: goal is not an entailment {ty}"
  match body with
  | ~q(@BI.exists ..) =>
    trace[debug] "solve_mono: exists found"
    let x : Ident ← `(x)
    let tac ← `(tactic| icases $HF:term with ⟨%x,$HF:ident⟩; iexists $x)
    let (mvars,_) ← runTactic goal tac
    setGoals mvars
  | ~q(@BI.sep ..) =>
    trace[debug] "solve_mono: sep found"
    let HF₁ : Ident ← `(HF₁)
    let HF₂ : Ident ← `(HF₂)
    let tac ← `(tactic|
      icases $HF:term with ⟨∗($HF₁:ident), ∗($HF₂:ident)⟩;
      isplitl [$HF₁:ident] <;>
      [irename $HF₁:ident => $HF:ident ; irename $HF₂:ident => $HF:ident ]
      )
    let (mvars,_) ← runTactic goal tac
    setGoals mvars
  | ~q(@BI.and ..) =>
    trace[debug] "solve_mono: and found"
    let tac ← `(tactic|
      isplit <;>
      [icases $HF:term with ⟨∗($HF:ident), _⟩;
      icases $HF:term with ⟨_ , ∗($HF:ident)⟩])
    let (mvars,_) ← runTactic goal tac
    setGoals mvars
  | ~q(@BI.or ..) =>
    trace[debug] "solve_mono: or found"
    let tac ← `(tactic|
      icases $HF:term with (∗($HF:ident) | ∗($HF:ident)) <;>
      [ileft ; iright])
    let (mvars,_) ← runTactic goal tac
    setGoals mvars
  | _ => match body.getAppFn with
      | Expr.const (.str _ s) _ =>
        -- Matches sometimes get elaborated to _.match_n for some n,
        -- so we check for a "match_" prefix here.
        trace[debug] "solve_mono: s found {s}"
        if (s.startsWith "match_") || (s.startsWith "ite")  then
          trace[debug] "solve_mono: match or ite found"
          let tac ← `(tactic| split <;> try simp)
          let (mvars,_) ← runTactic goal tac
          setGoals mvars
        -- TODO: This might be missing some cases for elaborated match statements.
        else throwError "solve_mono: unrecognized elaboration suffix {s}"
      | _ => throwError "solve_mono_go: unsupported connective in body"

elab "solve_mono" HF:ident : tactic => do
   solve_mono HF

macro "solve_mono_go" HF:ident Hmon:ident : tactic => do
  `(tactic|repeat' first | iexact $HF | (iapply $Hmon:ident; iexact $HF) | solve_mono $HF)

elab "solve_bi_mono_pred_with_prepare" F:ident "["defs_to_unfold:ident,* "]" : tactic => do
  let defs_to_unfold := defs_to_unfold.getElems

  evalTactic (← `(tactic|
    constructor;
    iintro %Φ %Ψ %HΦ %HΨ ⟨#Hmon⟩ %y ⟨HF⟩;
    unfold $F:ident;
    unfold $defs_to_unfold:ident*;
    solve_mono_go HF Hmon;
    intro _ _;
    constructor;
    intros _ x1 x2 Hx;
    have Hx := OFE.Discrete.discrete Hx;
    subst Hx; rfl
  ))

elab "solve_bi_mono_pred" F:ident : tactic => do
  evalTactic (← `(tactic|solve_bi_mono_pred_with_prepare $F:ident []))

/- Proves the standard public induction principle for a generated resource
predicate group.  The generator supplies only the group-specific pieces:

- [F]: the combined pre-fixpoint;
- [motive]: the dispatch-type motive assembled from the public motives;
- [hyps]: list of hypotheses introduced outside the tactic
- [unfold_defs]: list of public predicate wrappers appearing in the
  lemma statement

Everything involving the Iris least fixpoint remains in this library.

TODO: Can all of these arguments be inferred from the goal? -/

-- Recursively solves the conclusions of the induction principle
partial def solve_conclusions (hyp : Ident) : TacticM Unit := do
  trace[debug] "solve_conclusions: here"
  try
    evalTactic (← `(tactic| isplit))
    let goals ← getGoals
    for g in goals do
      setGoals [g]
      trace[debug] "solve_conclusions: split goals"
      solve_conclusions hyp
  catch _ =>
      trace[debug] "solve_conclusions: no more splits"
      evalTactic (← `(tactic| iintro *; iintro ⟨Hfix⟩ ; try simp))
      trace[debug] "solve_conclusions: simped"
      evalTactic (← `(tactic| iapply $hyp:ident $$ Hfix))

elab "solve_cn_predicate_induction"
    F:ident ","
    motive:term ","
    "["hyps:ident,* "]" : tactic => do

  -- Get the type of the induction assertion
  let motiveExpr ← elabTerm motive none
  let motiveTy ← Lean.Meta.inferType motiveExpr
  let .forallE _ fromTy toTy _ := motiveTy
    | throwError "motive must have function type"
  let fromTySyntax ← Lean.PrettyPrinter.delab fromTy
  let toTySyntax ← Lean.PrettyPrinter.delab toTy

  -- Define induction assertion
  evalTactic <| ← `(tactic|
    let induction_assertion : $toTySyntax := iprop% ∀ call : $fromTySyntax, bi_least_fixpoint $F call -∗ $motive call)

  -- Prove motive nonexpansive
  evalTactic (← `(tactic|
    have HCN_ind_motive_ne : OFE.NonExpansive $motive := by
      constructor
      intros _ _ _ Hx
      have Hx := OFE.Discrete.discrete Hx
      subst Hx
      rfl))

  -- Induction hypothesis
  evalTactic (← `(tactic|
    ihave Hind : induction_assertion $$ [];
    iapply least_fixpoint_iter;
    iintro !> %call Hbody;
    split
    ))

  -- Assumes you give the list of hypotheses in the same order you introduced them
  for hyp in hyps.getElems.toList do
    evalTactic (← `(tactic|
      try simp; iapply $hyp:ident $$ Hbody
    ))

  -- Assumes you labelled the right things with @[simp]
  solve_conclusions (← `(ident| Hind))
