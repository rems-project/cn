open Cmdliner

(* Cmdliner [default] doesn't work with positional arguments (the first
   positional of a command group is resolved as a subcommand name), so legacy
   `cn test FILE` invocations are routed to a flat, deprecated `test` command
   instead of the engine-subcommand group. *)
let subcommands ~legacy_test =
  [ Wf.cmd;
    Verify.cmd;
    (if legacy_test then Test.legacy_cmd else Test.cmd);
    Instrument.cmd;
    SeqTest.cmd
  ]


let () =
  let version_str = Cn_version.git_version ^ " [" ^ Cn_version.git_version_date ^ "]" in
  let cn_info = Cmd.info "cn" ~version:version_str in
  Test.reject_engine_prefix Sys.argv;
  let legacy_test = Test.wants_legacy Sys.argv in
  Stdlib.exit @@ Cmd.(eval (group cn_info (subcommands ~legacy_test)))
