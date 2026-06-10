open Cmdliner

let subcommands = [ Wf.cmd; Verify.cmd; Test.cmd; Instrument.cmd; SeqTest.cmd ]

let () =
  let version_str = Cn_version.git_version ^ " [" ^ Cn_version.git_version_date ^ "]" in
  let cn_info = Cmd.info "cn" ~version:version_str in
  let argv = Test.argv_with_default_engine Sys.argv in
  Stdlib.exit @@ Cmd.(eval ~argv (group cn_info subcommands))
