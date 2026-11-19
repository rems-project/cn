(** Tests for the LiftConstraints Disjunction Stage2 transformation *)

open OUnit2
module BT = Cn.BaseTypes
module MT = Cn.MakeTerm
module T = Cn.Terms.Normal
module LC = Cn.LogicalConstraints
module Sym = Cn.Sym
module Pp = Cn.Pp

(* Use Ownership domain for testing *)
module Domain = Cn.TestGeneration.Private.Bennet.Private.AbstractDomains.Ownership

module Stage2 = Cn.TestGeneration.Private.Bennet.Private.Stage2
module Term = Stage2.Private.Term.Make (Domain)

module Disjunction =
  Stage2.Private.SimplifyGen.Private.LiftConstraints.Private.Disjunction.Make (Domain)

(** Helper: Create test location *)
let test_loc = Cerb_location.unknown

let int_bt = BT.Bits (Signed, 32)

let pp_term gt = Pp.plain (Term.pp gt)

let assert_term_equal ~msg expected actual =
  assert_equal ~msg ~cmp:Term.equal ~printer:pp_term expected actual


let sym_it x = MT.sym_ (x, int_bt, test_loc)

let num n = MT.num_lit_ (Z.of_int n) int_bt test_loc

let eq a b = MT.eq_ (a, b) test_loc

let or_ a b = MT.or2_ (a, b) test_loc

(** Helper: Extract the branches of a `Pick, failing the test otherwise *)
let pick_branches (gt : Term.t) : Term.t list =
  let (Annot (gt_, (), _, _)) = gt in
  match gt_ with
  | `Pick gts -> gts
  | _ -> assert_failure (Printf.sprintf "Expected `Pick, got: %s" (pp_term gt))


(** Helper: Bind [v] to a generator call, making it external for the body.
    The generator symbol is shared so that inputs and expected outputs built
    with this helper compare equal. *)
let external_gen = Sym.fresh "external_gen"

let bind_external (v : Sym.t) (body : Term.t) : Term.t =
  let call = Term.call_ (external_gen, []) () int_bt test_loc in
  Term.let_star_ ((v, call), body) () test_loc


(** Test: A disjunction of liftable constraints becomes a `Pick with one branch
    per disjunct *)
let test_all_split _ =
  let x = Sym.fresh "x" in
  let d1 = eq (sym_it x) (num 0) in
  let d2 = eq (sym_it x) (num 1) in
  let inner = Term.return_ (MT.unit_ test_loc) () test_loc in
  let gt = Term.assert_ (LC.T (or_ d1 d2), inner) () test_loc in
  let expected =
    Term.pick_
      [ Term.assert_ (LC.T d1, inner) () test_loc;
        Term.assert_ (LC.T d2, inner) () test_loc
      ]
      ()
      BT.Unit
      test_loc
  in
  assert_term_equal
    ~msg:"Disjunction should lift to a `Pick with one branch per disjunct"
    expected
    (Disjunction.transform_gt gt)


(** Test (regression for conjunct handling): a leftover disjunct (mentioning an
    external variable) becomes its own `Pick branch, and is NOT conjoined into
    the liftable branches *)
let test_split_and_leftover _ =
  let x = Sym.fresh "x" in
  let v = Sym.fresh "v" in
  let d_split = eq (sym_it x) (num 0) in
  let d_left = eq (sym_it v) (num 7) in
  let inner = Term.return_ (sym_it v) () test_loc in
  let gt =
    bind_external v (Term.assert_ (LC.T (or_ d_split d_left), inner) () test_loc)
  in
  let expected =
    bind_external
      v
      (Term.pick_
         [ Term.assert_ (LC.T d_split, inner) () test_loc;
           Term.assert_ (LC.T d_left, inner) () test_loc
         ]
         ()
         int_bt
         test_loc)
  in
  let result = Disjunction.transform_gt gt in
  assert_term_equal
    ~msg:"Leftover disjunct should be a separate `Pick branch"
    expected
    result;
  (* Explicitly check the liftable branch's body is the unchanged inner
     generator, i.e. the leftover disjunct was not conjoined into it *)
  let (Annot (result_, (), _, _)) = result in
  match result_ with
  | `LetStar (_, gt_rest) ->
    (match pick_branches gt_rest with
     | [ Annot (`Assert (LC.T it_split, gt_body), (), _, _); _ ] ->
       assert_equal
         ~msg:"First branch should assert only the liftable disjunct"
         ~cmp:T.equal
         ~printer:(fun it -> Pp.plain (T.pp it))
         d_split
         it_split;
       assert_term_equal
         ~msg:"Liftable branch body should not contain the leftover disjunct"
         inner
         gt_body
     | gts ->
       assert_failure
         (Printf.sprintf
            "Expected 2 `Assert branches, got: %s"
            (String.concat ", " (List.map pp_term gts))))
  | _ -> assert_failure (Printf.sprintf "Expected `LetStar, got: %s" (pp_term result))


(** Test: Multiple leftover disjuncts are folded into a single disjunction in
    one `Pick branch *)
let test_multiple_leftovers _ =
  let x = Sym.fresh "x" in
  let v = Sym.fresh "v" in
  let d_split = eq (sym_it x) (num 0) in
  let d_left1 = eq (sym_it v) (num 7) in
  let d_left2 = eq (sym_it v) (num 9) in
  let inner = Term.return_ (sym_it v) () test_loc in
  let gt =
    bind_external
      v
      (Term.assert_ (LC.T (or_ (or_ d_split d_left1) d_left2), inner) () test_loc)
  in
  let expected =
    bind_external
      v
      (Term.pick_
         [ Term.assert_ (LC.T d_split, inner) () test_loc;
           Term.assert_ (LC.T (or_ d_left1 d_left2), inner) () test_loc
         ]
         ()
         int_bt
         test_loc)
  in
  assert_term_equal
    ~msg:"Leftover disjuncts should be folded into a single `Pick branch"
    expected
    (Disjunction.transform_gt gt)


(** Test: A disjunction with no liftable disjuncts is left unchanged *)
let test_all_leftover _ =
  let v = Sym.fresh "v" in
  let d_left1 = eq (sym_it v) (num 7) in
  let d_left2 = eq (sym_it v) (num 9) in
  let inner = Term.return_ (sym_it v) () test_loc in
  let gt =
    bind_external v (Term.assert_ (LC.T (or_ d_left1 d_left2), inner) () test_loc)
  in
  assert_term_equal
    ~msg:"Disjunction with no liftable disjuncts should be unchanged"
    gt
    (Disjunction.transform_gt gt)


(** Test: A non-disjunction assertion is left unchanged *)
let test_non_disjunction _ =
  let x = Sym.fresh "x" in
  let inner = Term.return_ (MT.unit_ test_loc) () test_loc in
  let gt = Term.assert_ (LC.T (eq (sym_it x) (num 0)), inner) () test_loc in
  assert_term_equal
    ~msg:"Non-disjunction assertion should be unchanged"
    gt
    (Disjunction.transform_gt gt)


let suite =
  "Disjunction"
  >::: [ "test_all_split" >:: test_all_split;
         "test_split_and_leftover" >:: test_split_and_leftover;
         "test_multiple_leftovers" >:: test_multiple_leftovers;
         "test_all_leftover" >:: test_all_leftover;
         "test_non_disjunction" >:: test_non_disjunction
       ]
