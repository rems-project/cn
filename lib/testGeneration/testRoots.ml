(* Selection of the functions that `cn test` tests (testing roots) and of the
   functions whose specifications it exports. Independent of generator and
   harness code, and of the global test-generation configuration. *)

module CF = Cerb_frontend
module A = CF.AilSyntax
module C = CF.Ctype
module CtA = Fulminate.Cn_to_ail
module FExtract = Fulminate.Extract

(** Workaround for https://github.com/rems-project/cerberus/issues/765 *)
let needs_enum_hack
      ~(with_warning : bool)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (inst : FExtract.instrumentation)
  =
  match List.assoc Sym.equal inst.fn sigma.declarations with
  | loc, _, Decl_function (_, (_, ret_ct), cts, _, _, _) ->
    if
      List.exists
        (fun (_, ct, _) ->
           match ct with C.Ctype (_, Basic (Integer (Enum _))) -> true | _ -> false)
        cts
    then (
      if with_warning then
        Cerb_colour.with_colour
          (fun () ->
             Pp.(
               warn
                 loc
                 (!^"Function"
                  ^^^ squotes (Sym.pp inst.fn)
                  ^^^ !^"has enum arguments and so could not be tested."
                  ^/^ !^"See https://github.com/rems-project/cerberus/issues/765.")))
          ();
      true)
    else if match ret_ct with C.Ctype (_, Basic (Integer (Enum _))) -> true | _ -> false
    then (
      if with_warning then
        Cerb_colour.with_colour
          (fun () ->
             Pp.(
               warn
                 loc
                 (!^"Function"
                  ^^^ squotes (Sym.pp inst.fn)
                  ^^^ !^"has an enum return type and so could not be tested."
                  ^/^ !^"See https://github.com/rems-project/cerberus/issues/765.")))
          ();
      true)
    else
      false
  | _ -> false


let functions_under_test
      ~(with_warning : bool)
      ~(skip_and_only : string list * string list)
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (paused : _ Typing.pause)
  : Test.t list
  =
  let insts =
    FExtract.collect_instrumentation prog5
    |> fst
    |> List.filter (fun (inst : FExtract.instrumentation) ->
      CtA.has_cn_spec inst
      && (match prog5.main with
          | Some main_fn -> not (Sym.equal main_fn inst.fn)
          | None -> true)
      && Option.is_some inst.internal
      && not (needs_enum_hack ~with_warning sigma inst))
  in
  let selected_fsyms =
    Check.select_functions
      ~strict:true
      skip_and_only
      (Sym.Set.of_list
         (List.map (fun (inst : FExtract.instrumentation) -> inst.fn) insts))
  in
  insts
  |> List.filter (fun (inst : FExtract.instrumentation) ->
    Sym.Set.mem inst.fn selected_fsyms)
  |> List.map (Test.of_instrumentation paused)


(* Specification export is intentionally broader than test generation. Every
   function definition has an internal argument/return specification, even
   when it has no user-written clauses or CN cannot generate a test harness
   for its C signature. Declarations are excluded because they have no body
   or definition in this translation unit. *)
let functions_for_export (prog5 : unit Mucore.file) (paused : _ Typing.pause)
  : Test.t list
  =
  FExtract.collect_instrumentation prog5
  |> fst
  |> List.filter (fun (inst : FExtract.instrumentation) -> Option.is_some inst.internal)
  |> List.map (Test.of_instrumentation paused)
