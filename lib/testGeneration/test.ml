module CF = Cerb_frontend
module A = CF.AilSyntax
module FExtract = Fulminate.Extract
module LAT = LogicalArgumentTypes
module AT = ArgumentTypes

type kind =
  | Constant (* Run function without arguments nor `accesses` once *)
  | RandomGenerator (* Run function with random inputs satisfying the precondition *)
  | SymbolicGenerator (* Run function with symbolic inputs satisfying the precondition *)
[@@deriving eq]

type t =
  { filename : string;
    kind : kind;
    suite : string;
    test : string;
    is_static : bool;
    is_trusted : bool;
    fn : Sym.t;
    fn_loc : Locations.t;
    internal : Fulminate.Extract.fn_args_and_body
  }

let of_instrumentation
      (cabs_tunit : CF.Cabs.translation_unit)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (paused : _ Typing.pause)
      (inst : Fulminate.Extract.instrumentation)
  : t
  =
  let filename = inst.fn_loc |> Cerb_location.get_filename |> Option.get in
  let kind =
    let _, _, decl = List.assoc Sym.equal inst.fn sigma.declarations in
    match decl with
    | Decl_function (_, _, args, _, _, _)
      when List.is_empty args
           && Sym.Set.is_empty
                (LAT.free_vars
                   (fun _ -> Sym.Set.empty)
                   (AT.get_lat (Option.get inst.internal))) ->
      Constant
    | Decl_function _ ->
      if TestGenConfig.is_symbolic_enabled () then SymbolicGenerator else RandomGenerator
    | Decl_object _ -> failwith __LOC__
  in
  let suite = filename |> Filename.basename |> String.split_on_char '.' |> List.hd in
  let test = Sym.pp_string inst.fn in
  let is_static =
    let (TUnit decls) = cabs_tunit in
    List.exists
      (fun decl ->
         match decl with
         | CF.Cabs.EDecl_func
             (FunDef
                ( _,
                  _,
                  { storage_classes; _ },
                  Declarator
                    (_, DDecl_function (DDecl_identifier (_, Identifier (_, fn')), _)),
                  _ ))
           when String.equal (Sym.pp_string inst.fn) fn'
                && List.exists
                     (fun scs -> match scs with CF.Cabs.SC_static -> true | _ -> false)
                     storage_classes ->
           true
         | _ -> false)
      decls
  in
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
    kind;
    suite;
    test;
    is_static;
    is_trusted = inst.trusted;
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
      is_static = test.is_static
    }
