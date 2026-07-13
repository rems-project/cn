(** Abstract Domains Test Suite *)

open OUnit2

(** Combined test suite for all abstract domain tests *)
let test_suite =
  "Abstract Domains Tests"
  >::: [ WrappedInterval.suite; Tristate.suite; Congruence.suite ]
