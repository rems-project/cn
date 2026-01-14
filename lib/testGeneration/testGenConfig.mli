type build_tool =
  | Bash
  | Make

type generation_mode =
  | Concrete (* Existing arbitrary/guided fuzzing *)
  | Symbolic (* New symbolic constraint-based generation *)

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
  | Everything

type smt_skewing_mode =
  | Uniform
  | Sized
  | None

type smt_solver =
  | Z3
  | CVC5

type t =
  { (* Compile time *)
    skip_and_only : string list * string list;
    cc : string;
    print_steps : bool;
    num_samples : int;
    max_backtracks : int;
    build_tool : build_tool;
    sanitizers : string option * string option;
    inline : inline_mode;
    experimental_struct_asgn_destruction : bool;
    experimental_product_arg_destruction : bool;
    experimental_learning : bool;
    experimental_arg_pruning : bool;
    experimental_return_pruning : bool;
    static_absint : string list;
    local_iterations : int;
    smt_pruning_before_absinst : [ `None | `Fast | `Slow ];
    smt_pruning_after_absinst : [ `None | `Fast | `Slow ];
    smt_pruning_remove_redundant_assertions : bool;
    smt_pruning_at_runtime : bool;
    runtime_assert_domain : bool;
    symbolic : bool;
    symbolic_timeout : int option; (* SMT solver timeout for symbolic solving *)
    max_unfolds : int option; (* Maximum unfolds for symbolic execution *)
    max_array_length : int; (* For symbolic execution *)
    use_solver_eval : bool; (* Use solver-based evaluation *)
    smt_solver : smt_solver;
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
    print_size_info : bool;
    print_backtrack_info : bool;
    print_satisfaction_info : bool;
    print_discard_info : bool;
    print_timing_info : bool;
    just_reset_solver : bool;
    smt_skewing_mode : smt_skewing_mode;
    smt_logging : string option;
    smt_log_unsat_cores : string option;
    max_bump_blocks : int option;
    bump_block_size : int option;
    max_input_alloc : int option;
    smt_skew_pointer_order : bool;
    dsl_log_dir : string option
  }

val default : t

module Options : sig
  val build_tool : (string * build_tool) list

  val logging_level : (string * logging_level) list

  val trace_granularity : (string * trace_granularity) list

  val progress_level : (string * progress_level) list

  val sizing_strategy : (string * sizing_strategy) list

  val inline_mode : (string * inline_mode) list

  val smt_skewing_mode : (string * smt_skewing_mode) list

  val smt_solver : (string * smt_solver) list
end

val initialize : t -> unit

val get_skip_and_only : unit -> string list * string list

val get_cc : unit -> string

val is_print_steps : unit -> bool

val is_print_seed : unit -> bool

val get_num_samples : unit -> int

val get_max_backtracks : unit -> int

val get_build_tool : unit -> build_tool

val has_sanitizers : unit -> string option * string option

val get_inline_mode : unit -> inline_mode

val is_experimental_struct_asgn_destruction : unit -> bool

val is_experimental_product_arg_destruction : unit -> bool

val is_experimental_learning : unit -> bool

val is_experimental_arg_pruning : unit -> bool

val is_experimental_return_pruning : unit -> bool

val has_static_absint : unit -> string list

val get_local_iterations : unit -> int

val has_smt_pruning_before_absinst : unit -> [ `None | `Fast | `Slow ]

val has_smt_pruning_after_absinst : unit -> [ `None | `Fast | `Slow ]

val is_smt_pruning_remove_redundant_assertions : unit -> bool

val is_smt_pruning_at_runtime : unit -> bool

val is_runtime_assert_domain : unit -> bool

val has_input_timeout : unit -> int option

val has_null_in_every : unit -> int option

val has_seed : unit -> string option

val has_logging_level : unit -> logging_level option

val has_logging_level_str : unit -> string option

val has_trace_granularity : unit -> trace_granularity option

val has_trace_granularity_str : unit -> string option

val has_progress_level : unit -> progress_level option

val has_progress_level_str : unit -> string option

val is_until_timeout : unit -> int option

val is_exit_fast : unit -> bool

val has_max_stack_depth : unit -> int option

val has_max_generator_size : unit -> int option

val has_sizing_strategy : unit -> sizing_strategy option

val has_sizing_strategy_str : unit -> string option

val is_random_size_splits : unit -> bool

val has_allowed_size_split_backtracks : unit -> int option

val is_coverage : unit -> bool

val has_pass : string -> bool

val is_trap : unit -> bool

val has_no_replays : unit -> bool

val has_no_replicas : unit -> bool

val get_output_tyche : unit -> string option

val will_print_size_info : unit -> bool

val will_print_backtrack_info : unit -> bool

val will_print_satisfaction_info : unit -> bool

val will_print_discard_info : unit -> bool

val will_print_timing_info : unit -> bool

val is_symbolic_enabled : unit -> bool

val has_symbolic_timeout : unit -> int option

val get_max_unfolds : unit -> int option

val get_max_array_length : unit -> int

val is_use_solver_eval : unit -> bool

val get_smt_solver : unit -> smt_solver

val is_just_reset_solver : unit -> bool

val get_smt_skewing_mode : unit -> smt_skewing_mode

val get_smt_logging : unit -> string option

val get_smt_log_unsat_cores : unit -> string option

val has_max_bump_blocks : unit -> int option

val has_bump_block_size : unit -> int option

val has_max_input_alloc : unit -> int option

val is_smt_skew_pointer_order : unit -> bool

val get_dsl_log_dir : unit -> string option
