module Private = struct
  module SpecExport = SpecExport
end

module Config = TestGenConfig
module Options = Config.Options

type engine = TestGenConfig.engine =
  | Bennet
  | Darcy
  | Lucas

type config = Config.t

let default_cfg : config = Config.default

let functions_under_test = TestRoots.functions_under_test

let export_specs = SpecExport.export
