(* Lucas: Bennet's randomized pipeline generalized by framing generation as iterative refinement of abstract-domain elements.

   The abstract-domain flags live here and are only accepted by this
   engine. *)

open Cn
open Cmdliner

(* Section for flags specific to [cn test lucas]. *)
let s_absint = "ABSTRACT INTERPRETATION OPTIONS"

module Flags = struct
  (* Lucas is experimental, so its experimental flags stay visible in `--help`
     (under their natural sections) rather than being hidden. [exp_docs section]
     files a flag under [section] here, but would hide it on a stable engine. *)
  let exp_docs = Shared.experimental_docs TestGeneration.Lucas

  let ad_pruning =
    let doc = "Enable abstract domain-based pruning" in
    Arg.(value & flag & info ~docs:s_absint [ "ad-pruning" ] ~doc)


  let static_absint =
    let doc = "(Experimental) Enable static abstract interpretation" in
    Arg.(value & flag & info ~docs:(exp_docs s_absint) [ "static-absint" ] ~doc)


  let domains =
    let doc =
      "Specify abstract domains to use (comma-separated list). Options: 'interval', \
       'wrapped_interval', 'tristate', 'congruence'"
    in
    Arg.(
      value
      & opt
          (list
             (enum
                [ ("interval", "interval");
                  ("wrapped_interval", "wrapped_interval");
                  ("tristate", "tristate");
                  ("congruence", "congruence")
                ]))
          []
      & info ~docs:(exp_docs s_absint) [ "domains" ] ~docv:"DOMAIN" ~doc)


  let local_iterations =
    let doc = "Maximum iterations for local abstract interpretation refinement" in
    Arg.(
      value
      & opt int TestGeneration.default_cfg.local_iterations
      & info ~docs:s_absint [ "local-iterations" ] ~doc)


  let smt_pruning_before_absint =
    let doc =
      "(Experimental) Use SMT solver to prune unsatisfiable branches before abstract \
       interpretation"
    in
    Arg.(
      value
      & opt (enum [ ("none", `None); ("fast", `Fast); ("slow", `Slow) ]) `None
      & info ~docs:(exp_docs s_absint) [ "smt-pruning-before-absint" ] ~doc)


  let smt_pruning_after_absint =
    let doc =
      "(Experimental) Use SMT solver to prune unsatisfiable branches after abstract \
       interpretation"
    in
    Arg.(
      value
      & opt (enum [ ("none", `None); ("fast", `Fast); ("slow", `Slow) ]) `None
      & info ~docs:(exp_docs s_absint) [ "smt-pruning-after-absint" ] ~doc)


  let smt_pruning_keep_redundant_assertions =
    let doc =
      "(Experimental) Keep assertions even if provably redundant during SMT pruning"
    in
    Arg.(
      value
      & flag
      & info ~docs:(exp_docs s_absint) [ "smt-pruning-keep-redundant-assertions" ] ~doc)


  let runtime_assert_domain =
    let doc = "Enable assert_domain checks at runtime (disabled by default)" in
    Arg.(value & flag & info ~docs:s_absint [ "runtime-assert-domain" ] ~doc)


  let dynamic_absint_assign =
    let doc =
      "Enable dynamic abstract interpretation for assign constraints. Modes: 'also' (use \
       dynamic absint in addition to static), 'only' (use dynamic absint only)"
    in
    Arg.(
      value
      & opt (some (enum TestGeneration.Options.dynamic_absint_assign_mode)) None
      & info ~docs:s_absint [ "dynamic-absint-assign" ] ~docv:"MODE" ~doc)


  let dynamic_local_iterations =
    let doc =
      "Fuel for the runtime abstract-interpretation local-iteration loop (re-run assume \
       refinement while the state changes). The default of 1 is single-pass; mirrors the \
       static side's $(b,--local-iterations)."
    in
    Arg.(
      value
      & opt int TestGeneration.default_cfg.dynamic_local_iterations
      & info ~docs:s_absint [ "dynamic-local-iterations" ] ~docv:"N" ~doc)


  let dynamic_arbitrary_domain =
    let doc = "Enable dynamic abstract domain refinement for arbitrary constraints" in
    Arg.(value & flag & info ~docs:s_absint [ "dynamic-arbitrary-domain" ] ~doc)


  let dynamic_arbitrary_propagation =
    let doc = "Enable backward domain propagation for arbitrary constraints" in
    Arg.(value & flag & info ~docs:s_absint [ "dynamic-arbitrary-propagation" ] ~doc)


  let dynamic_assert_domain =
    let doc = "Enable dynamic abstract domain refinement for assert constraints" in
    Arg.(value & flag & info ~docs:s_absint [ "dynamic-assert-domain" ] ~doc)


  let dynamic_return_propagation =
    let doc = "Enable backward domain propagation at let-return sites" in
    Arg.(value & flag & info ~docs:s_absint [ "dynamic-return-propagation" ] ~doc)
end

let term : (TestGeneration.config -> TestGeneration.config) Term.t =
  let make
        ad_pruning
        static_absint
        domains
        local_iterations
        smt_pruning_before_absint
        smt_pruning_after_absint
        smt_pruning_keep_redundant_assertions
        runtime_assert_domain
        dynamic_absint_assign
        dynamic_local_iterations
        dynamic_arbitrary_domain
        dynamic_arbitrary_propagation
        dynamic_assert_domain
        dynamic_return_propagation
        (cfg : TestGeneration.config)
    : TestGeneration.config
    =
    { cfg with
      ad_pruning;
      static_absint;
      domains;
      local_iterations;
      smt_pruning_before_absint;
      smt_pruning_after_absint;
      smt_pruning_remove_redundant_assertions = not smt_pruning_keep_redundant_assertions;
      runtime_assert_domain;
      dynamic_absint_assign;
      dynamic_local_iterations;
      dynamic_arbitrary_domain;
      dynamic_arbitrary_propagation;
      dynamic_assert_domain;
      dynamic_return_propagation
    }
  in
  Term.(
    const make
    $ Flags.ad_pruning
    $ Flags.static_absint
    $ Flags.domains
    $ Flags.local_iterations
    $ Flags.smt_pruning_before_absint
    $ Flags.smt_pruning_after_absint
    $ Flags.smt_pruning_keep_redundant_assertions
    $ Flags.runtime_assert_domain
    $ Flags.dynamic_absint_assign
    $ Flags.dynamic_local_iterations
    $ Flags.dynamic_arbitrary_domain
    $ Flags.dynamic_arbitrary_propagation
    $ Flags.dynamic_assert_domain
    $ Flags.dynamic_return_propagation)


let cmd =
  let doc =
    "(Experimental) Generate tests via randomized refinement of abstract elements."
  in
  Shared.mk_cmd
    ~name:"lucas"
    ~doc
    ~extra:[ s_absint ]
    ~engine:TestGeneration.Lucas
    ~engine_flags:(Shared.compose_flags Bennet.term term)
