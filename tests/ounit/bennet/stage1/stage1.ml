(** Stage1 Test Suite *)

open OUnit2

(** Combined test suite for all Stage1 tests *)
let test_suite =
  "Stage1 Tests" >::: [ DestructProducts.suite; PruneArgs.suite; PruneReturns.suite ]
