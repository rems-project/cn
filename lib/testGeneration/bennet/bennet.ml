module CF = Cerb_frontend
module A = CF.AilSyntax

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
  let ctx = GenCompile.compile filename prog5.resource_predicates tests in
  debug_stage "Compile" (ctx |> GenDefinitions.pp_context |> Pp.plain ~width:80);
  let ctx = ctx |> GenNormalize.normalize prog5 in
  debug_stage "Normalize" (ctx |> GenDefinitions.pp_context |> Pp.plain ~width:80);
  let ctx = ctx |> GenDistribute.distribute in
  debug_stage "Distribute" (ctx |> GenDefinitions.pp_context |> Pp.plain ~width:80);
  let ctx = ctx |> GenOptimize.optimize prog5 in
  debug_stage "Optimize" (ctx |> GenDefinitions.pp_context |> Pp.plain ~width:80);
  let ctx = ctx |> GenElaboration.elaborate in
  debug_stage "Elaborated" (ctx |> GenElaboration.pp |> Pp.plain ~width:80);
  ctx |> GenCodeGen.compile sigma
