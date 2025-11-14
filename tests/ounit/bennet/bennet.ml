(** Bennet Test Suite *)

open OUnit2

(** Combined test suite for all Bennet tests *)
let test_suite = "Bennet Tests" >::: [ AbstractDomains.test_suite ]
