type t =
  { (* Compile time *)
    print_steps : bool;
    num_calls : int;
    max_backtracks : int;
    num_tests : int
  }

let default = { print_steps = false; num_calls = 30; max_backtracks = 25; num_tests = 10 }

let instance = ref default

let initialize (cfg : t) = instance := cfg

let is_print_steps () = !instance.print_steps

let get_num_calls () = !instance.num_calls

let get_max_backtracks () = !instance.max_backtracks

let get_num_tests () = !instance.num_tests
