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
      (paused : _ Typing.pause)
      (tests : Test.t list)
  : Pp.document
  =
  let ctx = Stage1.transform filename prog5 tests in
  debug_stage "Stage 1" (ctx |> Stage1.Ctx.pp |> Pp.plain ~width:80);
  let ctx = Stage2.transform prog5 paused ctx in
  debug_stage "Stage 2" (ctx |> Stage2.Ctx.pp |> Pp.plain ~width:80);
  let ctx = Stage3.transform ctx in
  debug_stage "Stage 3" (ctx |> Stage3.Ctx.pp |> Pp.plain ~width:80);
  let ctx = Stage4.transform ctx in
  debug_stage "Stage 4" (ctx |> Stage4.Ctx.pp |> Pp.plain ~width:80);
  let ctx = Stage5.transform ctx in
  debug_stage "Stage 5" (ctx |> Stage5.Ctx.pp |> Pp.plain ~width:80);
  Stage6.transform sigma ctx
