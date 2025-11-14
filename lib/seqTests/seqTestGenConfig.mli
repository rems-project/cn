type t =
  { (* Compile time *)
    cc : string;
    print_steps : bool;
    disable_shrink : bool;
    num_calls : int;
    max_backtracks : int;
    num_tests : int
  }

val default : t

val initialize : t -> unit

val get_cc : unit -> string

val is_print_steps : unit -> bool

val get_num_calls : unit -> int

val get_max_backtracks : unit -> int

val get_num_tests : unit -> int

val is_disable_shrink : unit -> bool
