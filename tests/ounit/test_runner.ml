(** CN Test Runner - OUnit2 and QCheck Integration *)

open OUnit2

(** Main test suite combining all test modules *)
let all_tests = "CN Test Suite" >::: [ IndexTerms.suite; Bennet.test_suite ]

(** Run all tests with detailed output *)
let () = run_test_tt_main all_tests
