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
  Cerb_debug.print_debug 2 [] (fun () -> stage);
  debug_log (stage ^ ":\n");
  debug_log (str ^ "\n\n")


let parse_domain (s : string) : (module Domain.T) =
  match s with
  | _ when String.equal s AbstractDomains.Ownership.name ->
    (module AbstractDomains.Ownership)
  | _ ->
    Pp.warn Cerb_location.unknown Pp.(!^"Unknown abstract domain," ^^^ squotes !^s);
    (module AbstractDomains.Ownership)


let test_setup () : Pp.document =
  let domain = "ownership" in
  let open Pp in
  let module AD = (val parse_domain domain) in
  let module CG = Domain.CodeGen (AD.CInt) in
  !^"#include <bennet/internals/domain.h>"
  ^^ hardline
  ^^ !^"#include <bennet/internals/domains/ownership.h>"
  ^/^ hardline
  ^^ CG.setup ()


let synthesize
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (paused : _ Typing.pause)
      (tests : Test.t list)
  : Pp.document
  =
  let module AD = (val parse_domain "ownership") in
  let module Stage1 = Stage1.Make (AD) in
  let ctx = Stage1.transform filename prog5 tests in
  debug_stage "Stage 1" (ctx |> Stage1.Ctx.pp |> Pp.plain ~width:80);
  let module Stage2 = Stage2.Make (AD) in
  let ctx = Stage2.transform prog5 ctx in
  debug_stage "Stage 2" (ctx |> Stage2.Ctx.pp |> Pp.plain ~width:80);
  let module Stage3 = Stage3.Make (AD) in
  let ctx = Stage3.transform paused ctx in
  debug_stage "Stage 3" (ctx |> Stage3.Ctx.pp |> Pp.plain ~width:80);
  let module Stage4 = Stage4.Make (AD) in
  let ctx = Stage4.transform ctx in
  debug_stage "Stage 4" (ctx |> Stage4.Ctx.pp |> Pp.plain ~width:80);
  let module Stage5 = Stage5.Make (AD) in
  let ctx = Stage5.transform ctx in
  debug_stage "Stage 5" (ctx |> Stage5.Ctx.pp |> Pp.plain ~width:80);
  let module Stage6 = Stage6.Make (AD) in
  Stage6.transform sigma ctx
