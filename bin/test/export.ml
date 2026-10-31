(* `cn test export`: write the JSON specification module for every function
   definition in [FILE], without generating, building, or running tests. *)

open Cmdliner

let run (frontend : Frontend.args) output =
  Frontend.with_checked_program
    frontend
    ~f:(fun ~filename ~cerb_conf:_ ~prog5 ~ail_prog ~statement_locs:_ ~paused ->
      Frontend.export_specs ~path:output ~filename (snd ail_prog) prog5 paused;
      Ok ())


let output =
  let doc = "Write the exported specification JSON to $(docv)." in
  Arg.(
    required
    & opt (some string) None
    & info ~docs:Shared.s_output [ "o"; "output" ] ~docv:"PATH" ~doc)


let term : unit Term.t = Term.(const run $ Frontend.term $ output)

let doc =
  "Exports the CN specifications of every function definition in [FILE] as JSON, without \
   generating or running tests."


let cmd = Cmd.v (Cmd.info "export" ~doc ~man:[ `S Shared.s_output; `S Common.s_cn ]) term
