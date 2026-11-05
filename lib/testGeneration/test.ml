module AT = ArgumentTypes

type t =
  { fn : Sym.t;
    is_static : bool;
    internal : Fulminate.Extract.fn_args_and_body
  }

let of_instrumentation
      (paused : _ Typing.pause)
      (inst : Fulminate.Extract.instrumentation)
  : t
  =
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
      exit 2
  in
  { fn = inst.fn; is_static = inst.is_static; internal }
