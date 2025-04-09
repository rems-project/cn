type t =
  { (* Compile time *)
    print_steps : bool;
    with_static_hack : bool;
    num_samples : int;
    max_backtracks : int;
    num_resets : int;
    num_parallel : int
  }

val default : t

val initialize : t -> unit

val is_print_steps : unit -> bool

val get_num_samples : unit -> int

val get_max_backtracks : unit -> int

val get_max_resets : unit -> int

val get_num_parallel : unit -> int

val with_static_hack : unit -> bool
