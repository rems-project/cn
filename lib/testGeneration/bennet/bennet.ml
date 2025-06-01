module CF = Cerb_frontend
module A = CF.AilSyntax
module GD = GenDefinitions.Make (GenTerms)
module GC = GenContext.Make (GenTerms)

let debug_log_file : out_channel option ref = ref None

let init_debug () =
  if Option.is_none !debug_log_file && !Cerb_debug.debug_level > 0 then
    debug_log_file
    := Some
         (let open Stdlib in
          open_out "generatorCompilation.log")


let debug_log (str : string) : unit =
  init_debug ();
  match !debug_log_file with
  | Some oc ->
    output_string oc str;
    flush oc
  | None -> ()


let debug_stage (stage : string) (str : string) : unit =
  debug_log (stage ^ ":\n");
  debug_log (str ^ "\n\n")


let synthesize
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (tests : Test.t list)
  : Pp.document
  =
  if TestGenConfig.is_experimental () then (
    let ctx = Stage1.transform filename prog5 tests in
    debug_stage "Stage 1" (ctx |> Stage1.Ctx.pp |> Pp.plain ~width:80);
    let ctx = Stage2.transform ctx in
    debug_stage "Stage 2" (ctx |> Stage2.Ctx.pp |> Pp.plain ~width:80);
    let ctx = Stage3.transform ctx in
    debug_stage "Stage 3" (ctx |> Stage3.Ctx.pp |> Pp.plain ~width:80);
    let ctx = Stage4.transform ctx in
    debug_stage "Stage 4" (ctx |> Stage4.Ctx.pp |> Pp.plain ~width:80);
    Stage5.transform sigma ctx)
  else (
    let ctx = GenCompile.compile filename prog5.resource_predicates tests in
    debug_stage "Compile" (ctx |> GC.pp |> Pp.plain ~width:80);
    let ctx = ctx |> GenNormalize.normalize prog5 in
    debug_stage "Normalize" (ctx |> GC.pp |> Pp.plain ~width:80);
    let ctx = ctx |> GenDistribute.distribute in
    debug_stage "Distribute" (ctx |> GC.pp |> Pp.plain ~width:80);
    let ctx = ctx |> GenOptimize.optimize prog5 in
    debug_stage "Optimize" (ctx |> GC.pp |> Pp.plain ~width:80);
    let ctx = ctx |> GenElaboration.elaborate in
    debug_stage "Elaborated" (ctx |> GenElaboration.pp |> Pp.plain ~width:80);
    ctx |> GenCodeGen.compile sigma)
