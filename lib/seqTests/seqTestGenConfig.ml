type t =
  { (* Compile time *)
    cc : string;
    print_steps : bool;
    disable_shrink : bool;
    num_calls : int;
    max_backtracks : int;
    num_tests : int
  }

let default =
  { cc = "cc";
    print_steps = false;
    disable_shrink = false;
    num_calls = 30;
    max_backtracks = 25;
    num_tests = 10
  }


let instance = ref default

let initialize (cfg : t) = instance := cfg

let get_cc () = !instance.cc

let is_print_steps () = !instance.print_steps

let get_num_calls () = !instance.num_calls

let get_max_backtracks () = !instance.max_backtracks

let get_num_tests () = !instance.num_tests

let is_disable_shrink () = !instance.disable_shrink
