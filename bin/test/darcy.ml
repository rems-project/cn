(* Darcy: symbolic (SMT-based) test generation *)

open Cn
open Cmdliner

module Flags = struct
  let symbolic_timeout =
    let doc = "Set timeout for SMT solver in symbolic mode (milliseconds)" in
    Arg.(value & opt (some int) None & info [ "symbolic-timeout" ] ~doc)


  let smt_solver =
    let doc =
      "Choose SMT solver backend for symbolic test generation (z3, cvc5 is unsupported)."
    in
    Arg.(
      value
      & opt (enum TestGeneration.Options.smt_solver) TestGeneration.default_cfg.smt_solver
      & info [ "solver-type" ] ~docv:"SOLVER" ~doc)


  let max_array_length =
    let doc = "Maximum array length for symbolic mode" in
    Arg.(
      value
      & opt int TestGeneration.default_cfg.max_array_length
      & info [ "max-array-length" ] ~doc)


  let smt_logging =
    let doc = "Log SMT solver communication to specified file" in
    Arg.(value & opt (some string) None & info [ "smt-logging" ] ~doc ~docv:"FILE")


  let smt_log_unsat_cores =
    let doc = "Log unsat cores to specified file when constraints are unsatisfiable" in
    Arg.(
      value & opt (some string) None & info [ "smt-log-unsat-cores" ] ~doc ~docv:"FILE")


  let use_solver_eval =
    let doc = "(Experimental) Use solver-based evaluation" in
    Arg.(value & flag & info [ "use-solver-eval" ] ~doc)


  let just_reset_solver =
    let doc =
      "Just reset the SMT solver instead of closing and creating a new one. WARNING: A \
       bunch of stuff breaks."
    in
    Arg.(value & flag & info [ "just-reset-solver" ] ~doc)


  let smt_skewing_mode =
    let doc =
      "Set SMT skewing mode for symbolic test generation. Options: uniform (uniform \
       random values), sized (default, size-based values), none (no skewing)"
    in
    Arg.(
      value
      & opt
          (enum TestGeneration.Options.smt_skewing_mode)
          TestGeneration.default_cfg.smt_skewing_mode
      & info [ "smt-skewing" ] ~docv:"MODE" ~doc)


  let smt_skew_pointer_order =
    let doc = "Enable pointer ordering skewing in SMT solver" in
    Arg.(value & flag & info [ "smt-skew-pointer-order" ] ~doc)
end

let term : (TestGeneration.config -> TestGeneration.config) Term.t =
  let make
        symbolic_timeout
        smt_solver
        max_array_length
        smt_logging
        smt_log_unsat_cores
        use_solver_eval
        just_reset_solver
        smt_skewing_mode
        smt_skew_pointer_order
        (cfg : TestGeneration.config)
    : TestGeneration.config
    =
    { cfg with
      symbolic_timeout;
      smt_solver;
      max_array_length;
      smt_logging;
      smt_log_unsat_cores;
      use_solver_eval;
      just_reset_solver;
      smt_skewing_mode;
      smt_skew_pointer_order
    }
  in
  Term.(
    const make
    $ Flags.symbolic_timeout
    $ Flags.smt_solver
    $ Flags.max_array_length
    $ Flags.smt_logging
    $ Flags.smt_log_unsat_cores
    $ Flags.use_solver_eval
    $ Flags.just_reset_solver
    $ Flags.smt_skewing_mode
    $ Flags.smt_skew_pointer_order)


let cmd =
  let doc =
    "(Experimental) Generate tests using symbolic (SMT-based) input generation."
  in
  Cmd.v
    (Cmd.info "darcy" ~doc)
    (Shared.mk_term ~engine:(Term.const TestGeneration.Darcy) ~engine_flags:term)
