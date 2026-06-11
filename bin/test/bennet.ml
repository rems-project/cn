(* Bennet: randomized test generation (the default engine) *)

open Cn
open Cmdliner

module Flags = struct
  let gen_backtrack_attempts =
    let doc =
      "Set the maximum attempts to satisfy a constraint before backtracking further, \
       during input generation"
    in
    Arg.(
      value
      & opt int TestGeneration.default_cfg.max_backtracks
      & info [ "max-backtrack-attempts" ] ~doc)


  let random_size_splits =
    let doc = "Randomly split sizes between recursive generator calls" in
    Arg.(value & flag & info [ "random-size-splits" ] ~doc)


  let allowed_size_split_backtracks =
    let doc =
      "Set the maximum attempts to split up a generator's size (between recursive calls) \
       before backtracking further, during input generation"
    in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.allowed_size_split_backtracks
      & info [ "allowed-size-split-backtracks" ] ~doc)


  let sized_null =
    let doc = "Does nothing." in
    let deprecated = "Will be removed after July 31." in
    Arg.(value & flag & info [ "sized-null" ] ~deprecated ~doc)


  let only_top_level_ite_lifting =
    let doc = "Only lift top-level ITE expressions" in
    Arg.(value & flag & info [ "only-top-level-ite-lifting" ] ~doc)
end

let term : (TestGeneration.config -> TestGeneration.config) Term.t =
  let make
        max_backtracks
        random_size_splits
        allowed_size_split_backtracks
        _sized_null
        only_top_level_ite_lifting
        (cfg : TestGeneration.config)
    : TestGeneration.config
    =
    { cfg with
      max_backtracks;
      random_size_splits;
      allowed_size_split_backtracks;
      only_top_level_ite_lifting
    }
  in
  Term.(
    const make
    $ Flags.gen_backtrack_attempts
    $ Flags.random_size_splits
    $ Flags.allowed_size_split_backtracks
    $ Flags.sized_null
    $ Flags.only_top_level_ite_lifting)


let cmd =
  let doc = "Generate tests using randomized input generation (the default engine)." in
  Cmd.v
    (Cmd.info "bennet" ~doc)
    (Shared.mk_term ~engine:(Term.const TestGeneration.Bennet) ~engine_flags:term)
