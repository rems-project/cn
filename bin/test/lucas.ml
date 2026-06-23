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
      & info ~docs:(exp_docs s_absint) [ "static-absint" ] ~docv:"DOMAIN" ~doc)


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
end

let term : (TestGeneration.config -> TestGeneration.config) Term.t =
  let make
        ad_pruning
        static_absint
        local_iterations
        smt_pruning_before_absint
        smt_pruning_after_absint
        smt_pruning_keep_redundant_assertions
        runtime_assert_domain
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
      runtime_assert_domain
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
    $ Flags.runtime_assert_domain)


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
