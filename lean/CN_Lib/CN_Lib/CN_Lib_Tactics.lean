import Iris.Proofmode.Tactics
import Iris.BI.Lib.Fixpoint
import Qq
import Batteries.Tactic.SeqFocus
import Lean.Elab
import Lean
import CN_Lib.CN_Lib

open Lean Elab Tactic Qq

-- This file contains some useful tactics for proofs involving CN ownership predicates over integer types.

namespace CN_Lib

declare_syntax_cat ident_tuple
scoped syntax "(" ident "," ident ")":ident_tuple

declare_syntax_cat pmTerm_tuple
scoped syntax "(" pmTerm "," pmTerm ")":pmTerm_tuple

scoped syntax "own_eq" ident ident_tuple* : tactic
macro_rules
  | `(tactic| own_eq $_) => `(tactic|skip) --no args, tactic does nothing
  | `(tactic| own_eq $f ($hd,$hd') $[$tl]*) =>
    `(tactic|
        ihave %$hd := $f:ident $$ $hd:ident $hd':ident <;>
        own_eq $f $[$tl]*)

-- Tactic that destructs owned predicates into their component Owned_UChar predicates
scoped syntax "destruct_owned" pmTerm_tuple ident : tactic
macro_rules
  | `(tactic| destruct_owned ($HOwn, $HOwn') $f) =>
      `(tactic| first
        | icases $HOwn with ⟨H1, ⟨H2, ⟨H3, ⟨H4, HOwn⟩⟩⟩⟩ <;>
          icases HOwn with ⟨H5, ⟨H6, ⟨H7, ⟨H8, _⟩⟩⟩⟩ <;>
          icases $HOwn' with ⟨H1', ⟨H2', ⟨H3', ⟨H4', HOwn'⟩⟩⟩⟩ <;>
          icases HOwn' with ⟨H5', ⟨H6', ⟨H7', ⟨H8', _⟩⟩⟩⟩ <;>
          own_eq $f (H1, H1') (H2, H2') (H3, H3') (H4, H4')
            (H5, H5') (H6, H6') (H7, H7') (H8, H8')
        | icases $HOwn with ⟨H1, ⟨H2, ⟨H3, ⟨H4, _⟩⟩⟩⟩ <;>
          icases $HOwn' with ⟨H1', ⟨H2', ⟨H3', ⟨H4', _⟩⟩⟩⟩ <;>
          own_eq $f (H1, H1') (H2, H2') (H3, H3') (H4, H4')
        | icases $HOwn with ⟨H1, H2⟩ <;>
          icases $HOwn' with ⟨H1', H2'⟩ <;>
          own_eq $f (H1, H1') (H2, H2')
          )

-- Macro that proves `f v = f v'` from `f : Fin n → Int` and seperate hypotheses stating
--  `f v i = f v' i` for each `i` in `Fin n`.
scoped syntax "solve_fin_from_cases" : tactic
macro_rules
  | `(tactic| solve_fin_from_cases) =>
    `(tactic| funext i <;>
      (repeat refine Fin.lastCases ?_ ?_ i ; lia ; intro i) <;>
      apply Fin.elim0 <;> assumption)

-- Tactic that solves goals of the form ⌜v = v'⌝ given a pair of owned signed predicates
elab "solve_owned_eq_signed" HOwn:pmTerm_tuple f:ident
  f_bytes:ident min:ident max:ident
  v:ident v':ident H:ident H':ident : tactic => do
  evalTactic <| ← `(tactic|
    destruct_owned $HOwn $f <;> ipureintro <;>
    (have Hbytes : $f_bytes (wrapI $min $max $v) = $f_bytes (wrapI $min $max $v') := by
      solve_fin_from_cases) <;>
    have Hv := wrapI_within_bounds (x := $v) (min := $min) (max := $max) (by lia) <;>
    have Hv' := wrapI_within_bounds (x := $v') (min := $min) (max := $max) (by lia) <;>
    (first
      | have HEq := CN_Lib.UShort_eq Hv Hv' Hbytes <;> apply wrapI_Short_cong $H $H' HEq
      | have HEq := CN_Lib.UInt_eq Hv Hv' Hbytes <;> apply wrapI_Int_cong $H $H' HEq
      | have HEq := CN_Lib.ULong_eq Hv Hv' Hbytes <;> apply wrapI_Long_cong $H $H' HEq)
    )

-- Tactic that solves goals of the form ⌜v = v'⌝ given a pair of owned unsigned predicates
elab "solve_owned_eq_unsigned" HOwn:pmTerm_tuple f:ident
  f_bytes:ident
  v:ident v':ident H:ident H':ident : tactic => do
  evalTactic <| ← `(tactic|
    destruct_owned $HOwn $f <;> ipureintro <;>
    (have Hbytes : $f_bytes $v = $f_bytes $v' := by
      funext i
      repeat refine Fin.lastCases ?_ ?_ i; lia; intro i
      apply Fin.elim0; assumption) <;>
    (first
      | have HEq := CN_Lib.ULong_eq $H $H' Hbytes <;> assumption
      | have HEq := CN_Lib.UInt_eq $H $H' Hbytes <;> assumption
      | have HEq := CN_Lib.UShort_eq $H $H' Hbytes <;> assumption
      )
    )

-- Tactic that tries to apply the above two tactics for each supported C integer type
elab "solve_owned_eq" f:ident HOwn:pmTerm_tuple
  v:ident v':ident
  H:ident H':ident : tactic => do
  evalTactic <| ← `(tactic|
    (first
      | solve_owned_eq_unsigned $HOwn $f ULong_bytes $v $v' $H $H'
      | solve_owned_eq_unsigned $HOwn $f UInt_bytes $v $v' $H $H'
      | solve_owned_eq_unsigned $HOwn $f UShort_bytes $v $v' $H $H'
      | solve_owned_eq_signed $HOwn $f UInt_bytes min_UInt max_UInt $v $v' $H $H'
      | solve_owned_eq_signed $HOwn $f UShort_bytes min_UShort max_UShort $v $v' $H $H'
      | solve_owned_eq_signed $HOwn $f ULong_bytes min_ULong max_ULong $v $v' $H $H'
      )
    )

-- Tactic that solves inequalities of the form ⌜l ≠ l'⌝ given a pair of owned predicates
scoped syntax "solve_own_neq" ident ident: tactic
macro_rules
  | `(tactic| solve_own_neq $f $g) =>
      `(tactic| unfold $f <;>
          iintro ⟨H1 , _⟩ ⟨H2 , _⟩; simp <;>
          icases H1 with ⟨H1 , _⟩; icases H2 with ⟨H2 , _⟩ <;>
          iapply $g:ident $$ H1 H2)
