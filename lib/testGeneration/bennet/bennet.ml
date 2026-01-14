module CF = Cerb_frontend
module A = CF.AilSyntax

module Private = struct
  module AbstractDomains = AbstractDomains
  module Stage1 = Stage1
end

let ensure_dir_exists (dir : string) : unit =
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755


let normalize_stage_name (stage : string) : string =
  stage
  |> String.lowercase_ascii
  |> String.map (fun c -> if Char.equal c ' ' then '_' else c)


let write_stage_file (dir : string) (stage : string) (str : string) : unit =
  ensure_dir_exists dir;
  let normalized_name = normalize_stage_name stage in
  let filename = Filename.concat dir (normalized_name ^ ".log") in
  let oc = open_out filename in
  output_string oc str;
  close_out oc


let debug_stage (stage : string) (str : string) : unit =
  match TestGenConfig.get_dsl_log_dir () with
  | Some dir -> write_stage_file dir stage str
  | None -> ()


let parse_domain (s : string list) : (module Domain.T) =
  if List.is_empty s then (* Default *)
    AbstractDomains.product_domains
      [ (module AbstractDomains.Ownership); (module AbstractDomains.Interval) ]
  else (
    let domain_names =
      s |> List.map String.trim |> List.filter (fun x -> String.length x > 0)
    in
    (* Parse individual domain names *)
    let parse_single_domain (name : string) : (module Domain.T) option =
      match name with
      | _ when String.equal name AbstractDomains.Ownership.name -> None
      | _ when String.equal name AbstractDomains.Interval.name ->
        Some (module AbstractDomains.Interval)
      | _ when String.equal name AbstractDomains.WrappedInterval.name ->
        Some (module AbstractDomains.WrappedInterval)
      | _ ->
        Pp.warn_noloc Pp.(!^"Unknown abstract domain," ^^^ squotes !^name);
        None
    in
    match domain_names with
    | [] ->
      (* No domains specified - return ownership domain *)
      (module AbstractDomains.Ownership)
    | additional_domains ->
      (* Parse additional domains and filter out ownership if already specified *)
      let parsed_additional = List.filter_map parse_single_domain additional_domains in
      let ownership_module = (module AbstractDomains.Ownership : Domain.T) in
      (* Check if ownership is already in the list *)
      let all_domains = ownership_module :: parsed_additional in
      (match all_domains with
       | [ single ] -> single (* If only ownership, return it directly *)
       | multiple -> AbstractDomains.product_domains multiple))


let test_setup () : Pp.document =
  let open Pp in
  let module AD = (val parse_domain (TestGenConfig.has_static_absint ())) in
  let module CG = Domain.CodeGen (AD.CInt) in
  !^"#include <bennet/prelude.h>" ^^ hardline ^^ CG.setup ()


let synthesize
      filename
      (sigma : CF.GenTypes.genTypeCategory A.sigma)
      (prog5 : unit Mucore.file)
      (paused : _ Typing.pause)
      (tests : Test.t list)
  : Pp.document
  =
  let module AD = (val parse_domain (TestGenConfig.has_static_absint ())) in
  let module Stage1 = Stage1.Make (AD) in
  let ctx = Stage1.transform filename sigma prog5 tests in
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
  if TestGenConfig.is_symbolic_enabled () then
    let module Symbolic = Symbolic.Make (AD) in
    Symbolic.transform sigma prog5 ctx
  else
    let module Stage5 = Stage5.Make (AD) in
    let ctx = Stage5.transform ctx in
    debug_stage "Stage 5" (ctx |> Stage5.Ctx.pp |> Pp.plain ~width:80);
    let module Stage6 = Stage6.Make (AD) in
    Stage6.transform sigma prog5 ctx
