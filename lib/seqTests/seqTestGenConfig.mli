type t =
  { (* Compile time *)
    print_steps : bool;
    num_calls : int;
    max_backtracks : int;
    num_tests : int
  }

val default : t

val initialize : t -> unit

val is_print_steps : unit -> bool

val get_num_calls : unit -> int

val get_max_backtracks : unit -> int

val get_num_tests : unit -> int
