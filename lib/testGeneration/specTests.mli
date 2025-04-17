module CF = Cerb_frontend
module A = CF.AilSyntax
module FExtract = Fulminate.Extract

val compile_constant_tests
  :  CF.GenTypes.genTypeCategory A.sigma ->
  FExtract.instrumentation list ->
  Test.t list * Pp.document

val compile_generators
  :  CF.GenTypes.genTypeCategory A.sigma ->
  unit Mucore.file ->
  FExtract.instrumentation list ->
  Pp.document

val compile_generator_tests
  :  CF.GenTypes.genTypeCategory A.sigma ->
  unit Mucore.file ->
  FExtract.instrumentation list ->
  Test.t list * Pp.document
