open OUnit2

let () =
  Cerb_frontend.Ocaml_implementation.set
    Cerb_frontend.Ocaml_implementation.HafniumImpl.impl;
  run_test_tt_main
    ("CN Random Testing"
     >::: [ SpecExport.suite; AustenBuild.suite; AustenCampaign.suite ])
