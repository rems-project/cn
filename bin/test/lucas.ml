(* Lucas: Bennet's randomized pipeline generalized by framing generation as iterative refinement of abstract-domain elements.

   The abstract-domain flags live here and are only accepted by this
   engine. *)

open Cn
open Cmdliner

module Flags = struct
  let ad_pruning =
    let doc = "Enable abstract domain-based pruning" in
    Arg.(value & flag & info [ "ad-pruning" ] ~doc)


  let static_absint =
    let doc =
      "(Experimental) Use static abstract interpretation with specified domain (or a \
       comma-separated list). (e.g., 'interval', 'wrapped_interval', 'tristate')"
    in
    Arg.(
      value
      & opt
          (list
             (enum
                [ ("interval", "interval");
                  ("wrapped_interval", "wrapped_interval");
                  ("tristate", "tristate")
                ]))
          []
      & info [ "static-absint" ] ~docv:"DOMAIN" ~doc)


  let local_iterations =
    let doc = "Maximum iterations for local abstract interpretation refinement" in
    Arg.(
      value
      & opt int TestGeneration.default_cfg.local_iterations
      & info [ "local-iterations" ] ~doc)


  let smt_pruning_before_absint =
    let doc =
      "(Experimental) Use SMT solver to prune unsatisfiable branches before abstract \
       interpretation"
    in
    Arg.(
      value
      & opt (enum [ ("none", `None); ("fast", `Fast); ("slow", `Slow) ]) `None
      & info [ "smt-pruning-before-absint" ] ~doc)


  let smt_pruning_after_absint =
    let doc =
      "(Experimental) Use SMT solver to prune unsatisfiable branches after abstract \
       interpretation"
    in
    Arg.(
      value
      & opt (enum [ ("none", `None); ("fast", `Fast); ("slow", `Slow) ]) `None
      & info [ "smt-pruning-after-absint" ] ~doc)


  let smt_pruning_keep_redundant_assertions =
    let doc =
      "(Experimental) Keep assertions even if provably redundant during SMT pruning"
    in
    Arg.(value & flag & info [ "smt-pruning-keep-redundant-assertions" ] ~doc)


  let smt_pruning_at_runtime =
    let doc = "(Experimental) Use SMT solver to prune branches at runtime" in
    Arg.(value & flag & info [ "smt-pruning-at-runtime" ] ~doc)


  let runtime_assert_domain =
    let doc = "Enable assert_domain checks at runtime (disabled by default)" in
    Arg.(value & flag & info [ "runtime-assert-domain" ] ~doc)


  let experimental_learning =
    let doc = "Use experimental domain learning" in
    Arg.(value & flag & info [ "experimental-learning" ] ~doc)
end

let term : (TestGeneration.config -> TestGeneration.config) Term.t =
  let make
        ad_pruning
        static_absint
        local_iterations
        smt_pruning_before_absint
        smt_pruning_after_absint
        smt_pruning_keep_redundant_assertions
        smt_pruning_at_runtime
        runtime_assert_domain
        experimental_learning
        (cfg : TestGeneration.config)
    : TestGeneration.config
    =
    { cfg with
      ad_pruning;
      static_absint;
      local_iterations;
      smt_pruning_before_absint;
      smt_pruning_after_absint;
      smt_pruning_remove_redundant_assertions = not smt_pruning_keep_redundant_assertions;
      smt_pruning_at_runtime;
      runtime_assert_domain;
      experimental_learning
    }
  in
  Term.(
    const make
    $ Flags.ad_pruning
    $ Flags.static_absint
    $ Flags.local_iterations
    $ Flags.smt_pruning_before_absint
    $ Flags.smt_pruning_after_absint
    $ Flags.smt_pruning_keep_redundant_assertions
    $ Flags.smt_pruning_at_runtime
    $ Flags.runtime_assert_domain
    $ Flags.experimental_learning)


let cmd =
  let doc =
    "(Experimental) Generate tests via randomized refinement of abstract elements."
  in
  Cmd.v
    (Cmd.info "lucas" ~doc)
    (Shared.mk_term
       ~engine:(Term.const TestGeneration.Lucas)
       ~engine_flags:(Shared.compose_flags Bennet.term term))
