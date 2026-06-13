(* Paper-release subcommands: an engine run with the settings frozen for a
   specific paper (see lib/testGeneration/releases.ml). Only environment
   flags (file/compiler options, --output-dir, --dsl-log-dir, --only/--skip,
   --no-run, --seed, --until-timeout, --build-tool, sanitizer, reporting
   (--output-tyche, --print-*-info) and host allocator sizing flags, ...)
   are accepted — when absent they fall back to the frozen preset values;
   everything else is a cmdliner unknown-option error. *)

open Cn
open Cmdliner

let mk ~name ~doc ~engine ~preset =
  Cmd.v (Cmd.info name ~doc) (Shared.mk_release_term ~engine ~preset)


let darcy_pldi26 =
  mk
    ~name:"darcy-pldi26"
    ~doc:
      "Darcy with the settings frozen for the PLDI 2026 paper \
       'Code-Specify-Test-Debug-Prove'. Only environment flags (file/compiler options, \
       --output-dir, --dsl-log-dir, --only/--skip, --num-samples, --no-run, --seed, \
       --until-timeout, --build-tool, sanitizer, reporting, and allocator-sizing flags, \
       ...) may be passed."
    ~engine:TestGeneration.Darcy
    ~preset:TestGeneration.Releases.pldi26


let all = [ darcy_pldi26 ]

let names = [ "darcy-pldi26" ]
