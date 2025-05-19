module CF = Cerb_frontend
module A = CF.AilSyntax
module FExtract = Fulminate.Extract

val compile_constant_tests
  :  string ->
  CF.GenTypes.genTypeCategory A.sigma ->
  (bool * FExtract.instrumentation) list ->
  Test.t list * Pp.document

val compile_generators
  :  string ->
  CF.GenTypes.genTypeCategory A.sigma ->
  unit Mucore.file ->
  (bool * FExtract.instrumentation) list ->
  Pp.document

val compile_random_test_case
  :  string ->
  CF.GenTypes.genTypeCategory A.sigma ->
  unit Mucore.file ->
  Test.t ->
  Pp.document

val compile_generator_tests
  :  string ->
  CF.GenTypes.genTypeCategory A.sigma ->
  unit Mucore.file ->
  (bool * FExtract.instrumentation) list ->
  Test.t list * Pp.document
