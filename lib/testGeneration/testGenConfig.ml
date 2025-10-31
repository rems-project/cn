type build_tool =
  | Bash
  | Make

type generation_mode =
  | Concrete (* Backtracking random search *)
  | Symbolic (* Symbolic constraint-based generation *)

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

type t =
  { (* Compile time *)
    skip_and_only : string list * string list;
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
    static_absint : string list;
    smt_pruning_before_absinst : [ `None | `Fast | `Slow ];
    smt_pruning_after_absinst : [ `None | `Fast | `Slow ];
    smt_pruning_at_runtime : bool;
    symbolic : bool;
    symbolic_timeout : int option; (* SMT solver timeout for symbolic solving *)
    max_unfolds : int option; (* Maximum unfolds for symbolic execution *)
    max_array_length : int; (* For symbolic execution *)
    use_solver_eval : bool; (* Use solver-based evaluation *)
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
    smt_logging : string option
  }

let default =
  { skip_and_only = ([], []);
    cc = "cc";
    print_steps = false;
    num_samples = 100;
    max_backtracks = 25;
    build_tool = Bash;
    sanitizers = (None, None);
    inline_everything = false;
    experimental_struct_asgn_destruction = false;
    experimental_product_arg_destruction = false;
    experimental_learning = false;
    static_absint = [];
    smt_pruning_before_absinst = `None;
    smt_pruning_after_absinst = `None;
    smt_pruning_at_runtime = false;
    symbolic = false;
    symbolic_timeout = None;
    max_unfolds = None;
    max_array_length = 50;
    use_solver_eval = false;
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
    print_size_info = false;
    print_backtrack_info = false;
    print_satisfaction_info = false;
    print_discard_info = false;
    print_timing_info = false;
    just_reset_solver = false;
    smt_logging = None
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


module Options = struct
  let build_tool = [ ("bash", Bash); ("make", Make) ]

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
end

let instance : t option ref = ref Option.None

let initialize (cfg : t) = instance := Some cfg

let get_skip_and_only () = (Option.get !instance).skip_and_only

let get_cc () = (Option.get !instance).cc

let is_print_steps () = (Option.get !instance).print_steps

let is_print_seed () = (Option.get !instance).print_seed

let get_num_samples () = (Option.get !instance).num_samples

let get_max_backtracks () = (Option.get !instance).max_backtracks

let get_build_tool () = (Option.get !instance).build_tool

let has_sanitizers () = (Option.get !instance).sanitizers

let is_experimental_struct_asgn_destruction () =
  (Option.get !instance).experimental_struct_asgn_destruction


let is_experimental_product_arg_destruction () =
  (Option.get !instance).experimental_product_arg_destruction


let is_experimental_learning () = (Option.get !instance).experimental_learning

let has_static_absint () = (Option.get !instance).static_absint

let has_smt_pruning_before_absinst () = (Option.get !instance).smt_pruning_before_absinst

let has_smt_pruning_after_absinst () = (Option.get !instance).smt_pruning_after_absinst

let is_smt_pruning_at_runtime () = (Option.get !instance).smt_pruning_at_runtime

let has_inline_everything () = (Option.get !instance).inline_everything

let has_input_timeout () = (Option.get !instance).input_timeout

let has_null_in_every () = (Option.get !instance).null_in_every

let has_seed () = (Option.get !instance).seed

let has_logging_level () = (Option.get !instance).logging_level

let has_logging_level_str () =
  Option.map string_of_logging_level (Option.get !instance).logging_level


let has_trace_granularity () = (Option.get !instance).trace_granularity

let has_trace_granularity_str () =
  Option.map string_of_trace_granularity (Option.get !instance).trace_granularity


let has_progress_level () = (Option.get !instance).progress_level

let has_progress_level_str () =
  Option.map string_of_progress_level (Option.get !instance).progress_level


let is_until_timeout () = (Option.get !instance).until_timeout

let is_exit_fast () = (Option.get !instance).exit_fast

let has_max_stack_depth () = (Option.get !instance).max_stack_depth

let has_max_generator_size () = (Option.get !instance).max_generator_size

let has_sizing_strategy () = (Option.get !instance).sizing_strategy

let has_sizing_strategy_str () =
  Option.map string_of_sizing_strategy (Option.get !instance).sizing_strategy


let is_random_size_splits () = (Option.get !instance).random_size_splits

let has_allowed_size_split_backtracks () =
  (Option.get !instance).allowed_size_split_backtracks


let is_coverage () = (Option.get !instance).coverage

let has_pass s = not (List.mem String.equal s (Option.get !instance).disable_passes)

let is_trap () = (Option.get !instance).trap

let has_no_replays () = (Option.get !instance).no_replays

let has_no_replicas () = (Option.get !instance).no_replicas

let get_output_tyche () = (Option.get !instance).output_tyche

let will_print_size_info () = (Option.get !instance).print_size_info

let will_print_backtrack_info () = (Option.get !instance).print_backtrack_info

let will_print_satisfaction_info () = (Option.get !instance).print_satisfaction_info

let will_print_discard_info () = (Option.get !instance).print_discard_info

let will_print_timing_info () = (Option.get !instance).print_timing_info

let is_symbolic_enabled () = (Option.get !instance).symbolic

let has_symbolic_timeout () = (Option.get !instance).symbolic_timeout

let get_max_unfolds () = (Option.get !instance).max_unfolds

let get_max_array_length () = (Option.get !instance).max_array_length

let is_use_solver_eval () = (Option.get !instance).use_solver_eval

let is_just_reset_solver () = (Option.get !instance).just_reset_solver

let get_smt_logging () = (Option.get !instance).smt_logging
