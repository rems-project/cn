open Cmdliner

let subcommands = [ Wf.cmd; Verify.cmd; Test.cmd; Instrument.cmd; SeqTest.cmd; Mcp.cmd ]

let () =
  let cn_info = Cmd.info "cn" ~version:Common.version_str in
  Stdlib.exit @@ Cmd.(eval (group cn_info subcommands))
