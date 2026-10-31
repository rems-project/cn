module CF = Cerb_frontend
module A = CF.AilSyntax
module FExtract = Fulminate.Extract
module LAT = LogicalArgumentTypes
module AT = ArgumentTypes

type t =
  { filename : string;
    has_inputs : bool; (* Takes C arguments or has free logical variables *)
    suite : string;
    test : string;
    is_static : bool;
    is_trusted : bool;
    contains_function_level_spec : bool;
    fn : Sym.t;
    fn_loc : Locations.t;
    internal : Fulminate.Extract.fn_args_and_body
  }

let of_instrumentation
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (paused : _ Typing.pause)
      (inst : Fulminate.Extract.instrumentation)
  : t
  =
  let filename = inst.fn_loc |> Cerb_location.get_filename |> Option.get in
  let has_inputs =
    let _, _, decl = List.assoc Sym.equal inst.fn sigma.declarations in
    match decl with
    | Decl_function (_, _, args, _, _, _) ->
      not
        (List.is_empty args
         && Sym.Set.is_empty
              (LAT.free_vars
                 (fun _ -> Sym.Set.empty)
                 (AT.get_lat (Option.get inst.internal))))
    | Decl_object _ -> failwith __LOC__
  in
  let suite = filename |> Filename.basename |> String.split_on_char '.' |> List.hd in
  let test = Sym.pp_string inst.fn in
  let is_static = inst.is_static in
  let context =
    Result.get_ok (Typing.run_from_pause (fun _ -> Typing.get_typing_context ()) paused)
  in
  let module WellTyped =
    WellTyped.Lift (struct
      type 'a t = ('a, WellTyped.error) Result.t

      let return = Result.ok

      let bind = Result.bind

      let get_context () = return context

      let lift x = x
    end)
  in
  let internal =
    let at = inst.internal |> Option.get in
    match at |> AT.map fst |> WellTyped.function_type "function" inst.fn_loc with
    | Ok at' -> at' |> AT.map (fun rt -> (rt, snd (AT.get_return at)))
    | Error err ->
      TypeErrors.report_pretty TypeErrors.{ loc = err.loc; msg = WellTyped err.msg };
      exit 1
  in
  { filename;
    has_inputs;
    suite;
    test;
    is_static;
    is_trusted = inst.trusted;
    contains_function_level_spec = inst.contains_function_level_spec;
    fn = inst.fn;
    fn_loc = inst.fn_loc;
    internal
  }


let to_instrumentation (test : t) : FExtract.instrumentation =
  FExtract.
    { fn = test.fn;
      fn_loc = test.fn_loc;
      internal = Some test.internal;
      trusted = test.is_trusted;
      contains_function_level_spec = test.contains_function_level_spec;
      is_static = test.is_static
    }
