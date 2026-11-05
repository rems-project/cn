type engine =
  | Bennet (* Backtracking random search *)
  | Darcy (* Symbolic constraint-based generation *)
  | Lucas (* Randomized refinement of abstract elements *)

val string_of_engine : engine -> string

val cli_name : engine -> string

val is_experimental : engine -> bool

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
      (** Dump the specification IR as AustenTest spec-module JSON here. *)
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

val default : t

module Options : sig
  val logging_level : (string * logging_level) list

  val trace_granularity : (string * trace_granularity) list

  val progress_level : (string * progress_level) list

  val sizing_strategy : (string * sizing_strategy) list

  val inline_mode : (string * inline_mode) list

  val smt_skewing_mode : (string * smt_skewing_mode) list

  val smt_solver : (string * smt_solver) list

  val dynamic_absint_assign_mode : (string * dynamic_absint_assign_mode) list
end
