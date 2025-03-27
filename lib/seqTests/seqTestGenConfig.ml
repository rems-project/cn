type t =
  { (* Compile time *)
    print_steps : bool;
    with_static_hack : bool;
    num_samples : int;
    max_backtracks : int;
    num_resets : int
  }

let default =
  { print_steps = false;
    with_static_hack = false;
    num_samples = 100;
    max_backtracks = 25;
    num_resets = 0
  }


let instance = ref default

let initialize (cfg : t) = instance := cfg

let is_print_steps () = !instance.print_steps

let get_num_samples () = !instance.num_samples

let get_max_backtracks () = !instance.max_backtracks

let get_max_resets () = !instance.num_resets

let with_static_hack () = !instance.with_static_hack
