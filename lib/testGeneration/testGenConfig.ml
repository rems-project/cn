type engine =
  | Bennet (* Backtracking random search *)
  | Darcy (* Symbolic constraint-based generation *)
  | Lucas (* Randomized refinement of abstract elements *)

let string_of_engine (engine : engine) =
  match engine with Bennet -> "Bennet" | Darcy -> "Darcy" | Lucas -> "Lucas"


(* Subcommand name for an engine (e.g. [Bennet] -> "bennet"). *)
let cli_name (engine : engine) = String.lowercase_ascii (string_of_engine engine)

(* Whether an engine is experimental (not yet ready for general use). *)
let is_experimental (engine : engine) =
  match engine with Bennet | Darcy -> false | Lucas -> true


type logging_level =
  | None
  | Error
  | Info

type trace_granularity =
  | None
  | Ends
  | All

type progress_level =
  | Silent
  | PerFunc
  | PerTestCase

type sizing_strategy =
  | Uniform
  | Constant
  | QuickCheck

type inline_mode =
  | Nothing
  | NonRecursive
  | SemiRecursive
  | Everything

type smt_skewing_mode =
  | Uniform
  | Sized
  | None

type smt_solver =
  | Z3
  | CVC5

type dynamic_absint_assign_mode =
  | DynamicAbsIntAssignAlso
  | DynamicAbsIntAssignOnly

type t =
  { (* Compile time *)
    skip_and_only : string list * string list;
    cc : string;
    print_steps : bool;
    max_backtracks : int;
    sanitizers : string option * string option;
    inline : inline_mode;
    experimental_struct_asgn_destruction : bool;
    experimental_product_arg_destruction : bool;
    experimental_arg_pruning : bool;
    experimental_return_pruning : bool;
    ad_pruning : bool;
    static_absint : bool;
    domains : string list;
    local_iterations : int;
    smt_pruning_before_absint : [ `None | `Fast | `Slow ];
    smt_pruning_after_absint : [ `None | `Fast | `Slow ];
    smt_pruning_remove_redundant_assertions : bool;
    smt_pruning_at_runtime : bool;
    runtime_assert_domain : bool;
    engine : engine;
    symbolic_timeout : int option; (* SMT solver timeout for symbolic solving (ms) *)
    max_unfolds : int option; (* Maximum unfolds for symbolic execution *)
    max_array_length : int; (* For symbolic execution *)
    use_solver_eval : bool; (* Use solver-based evaluation *)
    smt_solver : smt_solver;
    disable_specialization : bool;
    only_top_level_ite_lifting : bool;
    (* Run time *)
    print_seed : bool;
    input_timeout : int option;
    null_in_every : int option;
    seed : string option;
    logging_level : logging_level option;
    trace_granularity : trace_granularity option;
    progress_level : progress_level option;
    until_timeout : int option;
    exit_fast : bool;
    max_stack_depth : int option;
    max_depth_failures : int option;
    max_generator_size : int option;
    sizing_strategy : sizing_strategy option;
    random_size_splits : bool;
    allowed_size_split_backtracks : int option;
    coverage : bool;
    disable_passes : string list;
    trap : bool;
    no_replays : bool;
    no_replicas : bool;
    output_tyche : string option;
    export_spec_json : string option;
    print_size_info : bool;
    print_backtrack_info : bool;
    print_satisfaction_info : bool;
    print_discard_info : bool;
    print_timing_info : bool;
    progress_backtracks : bool;
    just_reset_solver : bool;
    smt_skewing_mode : smt_skewing_mode;
    smt_logging : string option;
    smt_log_unsat_cores : string option;
    max_bump_blocks : int option;
    bump_block_size : int option;
    max_input_alloc : int option;
    smt_skew_pointer_order : bool;
    dsl_log_dir : string option;
    disable_extrema_skew : bool;
    discard_factor : int;
    dynamic_arbitrary_domain : bool;
    dynamic_arbitrary_propagation : bool;
    dynamic_assert_domain : bool;
    dynamic_return_propagation : bool;
    old_style_alloc : bool;
    dynamic_absint_assign : dynamic_absint_assign_mode option;
    dynamic_local_iterations : int
  }

let default =
  { skip_and_only = ([], []);
    cc = "cc";
    print_steps = false;
    max_backtracks = 25;
    sanitizers = (None, None);
    inline = Nothing;
    experimental_struct_asgn_destruction = false;
    experimental_product_arg_destruction = false;
    experimental_arg_pruning = false;
    experimental_return_pruning = false;
    ad_pruning = false;
    static_absint = false;
    domains = [];
    local_iterations = 10;
    smt_pruning_before_absint = `None;
    smt_pruning_after_absint = `None;
    smt_pruning_remove_redundant_assertions = true;
    smt_pruning_at_runtime = false;
    runtime_assert_domain = false;
    engine = Bennet;
    symbolic_timeout = None;
    max_unfolds = None;
    max_array_length = 50;
    use_solver_eval = false;
    smt_solver = Z3;
    disable_specialization = false;
    only_top_level_ite_lifting = false;
    print_seed = false;
    input_timeout = None;
    null_in_every = None;
    seed = None;
    logging_level = None;
    trace_granularity = None;
    progress_level = None;
    until_timeout = None;
    exit_fast = false;
    max_stack_depth = None;
    max_depth_failures = None;
    max_generator_size = None;
    sizing_strategy = None;
    random_size_splits = false;
    allowed_size_split_backtracks = None;
    coverage = false;
    disable_passes = [];
    trap = false;
    no_replays = false;
    no_replicas = false;
    output_tyche = Option.None;
    export_spec_json = Option.None;
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
    max_bump_blocks = None;
    bump_block_size = None;
    max_input_alloc = None;
    smt_skew_pointer_order = false;
    dsl_log_dir = None;
    disable_extrema_skew = false;
    discard_factor = 10;
    dynamic_arbitrary_domain = false;
    dynamic_arbitrary_propagation = false;
    dynamic_assert_domain = false;
    dynamic_return_propagation = false;
    old_style_alloc = false;
    dynamic_absint_assign = None;
    dynamic_local_iterations = 1
  }


let string_of_logging_level (logging_level : logging_level) =
  match logging_level with None -> "none" | Error -> "error" | Info -> "info"


let string_of_trace_granularity (trace_granularity : trace_granularity) =
  match trace_granularity with None -> "none" | Ends -> "ends" | All -> "all"


let string_of_progress_level (progress_level : progress_level) =
  match progress_level with
  | Silent -> "silent"
  | PerFunc -> "function"
  | PerTestCase -> "testcase"


let string_of_sizing_strategy (sizing_strategy : sizing_strategy) =
  match sizing_strategy with
  | Uniform -> "uniform"
  | Constant -> "constant"
  | QuickCheck -> "quickcheck"


let string_of_inline_mode (inline_mode : inline_mode) =
  match inline_mode with
  | Nothing -> "nothing"
  | NonRecursive -> "nonrec"
  | SemiRecursive -> "semirec"
  | Everything -> "everything"


let string_of_smt_skewing_mode (mode : smt_skewing_mode) =
  match mode with Uniform -> "uniform" | Sized -> "sized" | None -> "none"


let string_of_smt_solver (solver : smt_solver) =
  match solver with Z3 -> "z3" | CVC5 -> "cvc5"


let string_of_dynamic_absint_assign_mode (mode : dynamic_absint_assign_mode) =
  match mode with DynamicAbsIntAssignAlso -> "also" | DynamicAbsIntAssignOnly -> "only"


module Options = struct
  let logging_level : (string * logging_level) list =
    List.map (fun lvl -> (string_of_logging_level lvl, lvl)) [ None; Error; Info ]


  let trace_granularity : (string * trace_granularity) list =
    List.map (fun gran -> (string_of_trace_granularity gran, gran)) [ None; Ends; All ]


  let progress_level : (string * progress_level) list =
    List.map
      (fun lvl -> (string_of_progress_level lvl, lvl))
      [ Silent; PerFunc; PerTestCase ]


  let sizing_strategy : (string * sizing_strategy) list =
    List.map
      (fun strat -> (string_of_sizing_strategy strat, strat))
      [ Uniform; Constant; QuickCheck ]


  let inline_mode : (string * inline_mode) list =
    List.map
      (fun mode -> (string_of_inline_mode mode, mode))
      [ Nothing; NonRecursive; SemiRecursive; Everything ]


  let smt_skewing_mode : (string * smt_skewing_mode) list =
    List.map
      (fun mode -> (string_of_smt_skewing_mode mode, mode))
      [ Uniform; Sized; None ]


  let smt_solver : (string * smt_solver) list =
    List.map (fun solver -> (string_of_smt_solver solver, solver)) [ Z3 ]


  let dynamic_absint_assign_mode : (string * dynamic_absint_assign_mode) list =
    List.map
      (fun mode -> (string_of_dynamic_absint_assign_mode mode, mode))
      [ DynamicAbsIntAssignAlso; DynamicAbsIntAssignOnly ]
end
