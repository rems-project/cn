open Cmdliner

let subcommands = [ Wf.cmd; Verify.cmd; Test.cmd; Instrument.instrument_cmd; Instrument.run_existing_cmd; SeqTest.cmd ]

let () =
  let version_str = Cn_version.git_version ^ " [" ^ Cn_version.git_version_date ^ "]" in
  let cn_info = Cmd.info "cn" ~version:version_str in
  Stdlib.exit @@ Cmd.(eval (group cn_info subcommands))
