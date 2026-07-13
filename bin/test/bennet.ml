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
      & info ~docs:Shared.s_generation [ "max-backtrack-attempts" ] ~doc)


  let random_size_splits =
    let doc = "Randomly split sizes between recursive generator calls" in
    Arg.(value & flag & info ~docs:Shared.s_generation [ "random-size-splits" ] ~doc)


  let allowed_size_split_backtracks =
    let doc =
      "Set the maximum attempts to split up a generator's size (between recursive calls) \
       before backtracking further, during input generation"
    in
    Arg.(
      value
      & opt (some int) TestGeneration.default_cfg.allowed_size_split_backtracks
      & info ~docs:Shared.s_generation [ "allowed-size-split-backtracks" ] ~doc)


  let only_top_level_ite_lifting =
    let doc = "Only lift top-level ITE expressions" in
    Arg.(
      value & flag & info ~docs:Shared.s_generation [ "only-top-level-ite-lifting" ] ~doc)
end

let term : (TestGeneration.config -> TestGeneration.config) Term.t =
  let make
        max_backtracks
        random_size_splits
        allowed_size_split_backtracks
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
    $ Flags.only_top_level_ite_lifting)


let cmd =
  let doc = "Generate tests using randomized input generation (the default engine)." in
  Shared.mk_cmd
    ~name:"bennet"
    ~doc
    ~extra:[]
    ~engine:TestGeneration.Bennet
    ~engine_flags:term
