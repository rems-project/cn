(** Stage2 Test Suite *)

open OUnit2

(** Combined test suite for all Stage2 tests *)
let test_suite = "Stage2 Tests" >::: [ Disjunction.suite ]
