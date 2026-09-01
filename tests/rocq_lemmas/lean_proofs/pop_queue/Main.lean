import CN_Lib.CN_Lib
import CN_Lib.CN_Lib_Iris
import CN_Lib.CN_Lib_Iris_Fixpoint
import Inst_Params.Inst_Params
import Gen_Spec.Gen_Spec
import Inst_Spec

open Iris CN_Lib ProofMode Gen_Spec

variable [h : MyHeap hlc GF]

#synth @pop_lemma _ _ h
