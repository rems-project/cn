type build_tool =
  | Bash
  | Make

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
  | Quartile
  | QuickCheck

type t =
  { (* Compile time *)
    cc : string;
    print_steps : bool;
    num_samples : int;
    max_backtracks : int;
    build_tool : build_tool;
    sanitizers : string option * string option;
    inline_everything : bool;
    experimental_struct_asgn_destruction : bool;
    experimental_product_arg_destruction : bool;
    experimental_learning : bool;
    static_absint : bool;
    smt_pruning : [ `None | `Fast | `Slow ];
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
    print_satisfaction_info : bool
  }

val default : t

module Options : sig
  val build_tool : (string * build_tool) list

  val logging_level : (string * logging_level) list

  val trace_granularity : (string * trace_granularity) list

  val progress_level : (string * progress_level) list

  val sizing_strategy : (string * sizing_strategy) list
end

val initialize : t -> unit

val get_cc : unit -> string

val is_print_steps : unit -> bool

val is_print_seed : unit -> bool

val get_num_samples : unit -> int

val get_max_backtracks : unit -> int

val get_build_tool : unit -> build_tool

val has_sanitizers : unit -> string option * string option

val has_inline_everything : unit -> bool

val is_experimental_struct_asgn_destruction : unit -> bool

val is_experimental_product_arg_destruction : unit -> bool

val is_experimental_learning : unit -> bool

val has_static_absint : unit -> bool

val has_smt_pruning : unit -> [ `None | `Fast | `Slow ]

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
