(* Frozen settings for paper artifacts. Full record literals on purpose:
   adding a field to [TestGenConfig.t] must break compilation here so that
   its pinned value is decided explicitly. Never use [default with].

   For options where [None] means "let the runtime decide" (the build script
   only passes the flag when [Some]), the value is pinned to [Some] of the
   runtime default at freeze time, so later changes to the defaults in
   libcn's C sources cannot alter paper runs. [None] remains only where it
   is semantic: a feature that is off ([seed]: fresh randomness per run;
   [max_unfolds]: no unfold pass; [until_timeout]/[symbolic_timeout] are
   pinned to their explicit "disabled" value 0 instead) or that requires an
   output path ([output_tyche], [smt_logging], [smt_log_unsat_cores]).

   Fields backed by the release subcommands' environment flags hold
   placeholders (the CLI defaults); [Shared.mk_release_term] replaces them
   from the command line ([skip_and_only], [cc], [print_steps],
   [num_samples], [dsl_log_dir], the logging/reporting settings
   [logging_level], [trace_granularity], [progress_level], and the boolean
   diagnostic flags [exit_fast], [coverage], [no_replays], [no_replicas],
   [print_size_info], [print_backtrack_info], [print_satisfaction_info],
   [print_discard_info], [print_timing_info], [progress_backtracks]).

   A further set of value-carrying environment fields keeps its frozen
   value as the default but may be overridden from the command line
   ([build_tool], [sanitizers], [seed], [until_timeout], [output_tyche],
   and the host allocator sizes [max_bump_blocks], [bump_block_size],
   [max_input_alloc]). *)

(* PLDI 2026 paper "Code-Specify-Test-Debug-Prove" *)
let pldi26 : TestGenConfig.t =
  { skip_and_only = ([], []);
    cc = "cc";
    print_steps = false;
    num_samples = 100;
    max_backtracks = 25;
    build_tool = Bash;
    sanitizers = (None, None);
    inline = Nothing;
    experimental_struct_asgn_destruction = false;
    experimental_product_arg_destruction = false;
    experimental_arg_pruning = false;
    experimental_return_pruning = false;
    ad_pruning = false;
    static_absint = [];
    local_iterations = 10;
    smt_pruning_before_absint = `None;
    smt_pruning_after_absint = `None;
    smt_pruning_remove_redundant_assertions = true;
    smt_pruning_at_runtime = true;
    runtime_assert_domain = false;
    engine = Darcy;
    symbolic_timeout = Some 0 (* 0 = no solver timeout; cn-smt/solver.c *);
    max_unfolds = None;
    max_array_length = 50;
    use_solver_eval = false;
    smt_solver = Z3;
    disable_specialization = false;
    only_top_level_ite_lifting = false;
    old_style_alloc = true;
    print_seed = false;
    input_timeout = Some 1000 (* ms; cn-testing/test.c *);
    null_in_every = Some 5 (* bennet/dsl/arbitrary.c *);
    seed = None;
    logging_level = None;
    trace_granularity = None;
    progress_level = None;
    until_timeout = Some 0 (* 0 = run num_samples, not a timeout; cn-testing/test.c *);
    exit_fast = false;
    max_stack_depth = Some 255 (* UINT8_MAX; bennet/internals/size.c *);
    max_depth_failures = Some 65535 (* UINT16_MAX; bennet/state/failure.c *);
    max_generator_size = Some 25 (* bennet/internals/size.c *);
    sizing_strategy = Some QuickCheck (* cn-testing/test.c *);
    random_size_splits = false;
    allowed_size_split_backtracks = Some 0 (* bennet/internals/size.c *);
    coverage = false;
    disable_passes = [];
    trap = false;
    no_replays = false;
    no_replicas = false;
    output_tyche = None;
    print_size_info = false;
    print_backtrack_info = false;
    print_satisfaction_info = false;
    print_discard_info = false;
    print_timing_info = false;
    progress_backtracks = false;
    just_reset_solver = false;
    smt_skewing_mode = Sized;
    smt_logging = None;
    smt_log_unsat_cores = None;
    max_bump_blocks = Some 256 (* cn-executable/bump_alloc.c *);
    bump_block_size = Some 8388608 (* 8 MiB; cn-executable/bump_alloc.c *);
    max_input_alloc = Some 33554432 (* 32 MiB; bennet/state/rand_alloc.c *);
    smt_skew_pointer_order = false;
    dsl_log_dir = None;
    disable_extrema_skew = false;
    discard_factor = 10
  }
