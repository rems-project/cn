type kind =
  | Constant (* Run function without arguments nor `accesses` once *)
  | Generator (* Run function with random inputs satisfying the precondition *)

type t =
  { filename : string;
    kind : kind;
    suite : string;
    test : string;
    is_static : bool;
    fn : Sym.t;
    fn_loc : Locations.t;
    internal : Fulminate.Extract.fn_args_and_body
  }

let of_instrumentation is_static kind (inst : Fulminate.Extract.instrumentation) : t =
  let filename = inst.fn_loc |> Cerb_location.get_filename |> Option.get in
  { filename;
    kind;
    suite = filename |> Filename.basename |> String.split_on_char '.' |> List.hd;
    test = Sym.pp_string inst.fn;
    is_static;
    fn = inst.fn;
    fn_loc = inst.fn_loc;
    internal = Option.get inst.internal
  }
