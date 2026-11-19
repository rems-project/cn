import CN_Lib.CN_Lib
import CN_Lib.CN_Lib_Iris
import CN_Lib.CN_Lib_Iris_Fixpoint

open Iris CN_Lib ProofMode
variable [h : MyHeap hlc GF]

namespace Inst_Params

def Alloc : Ptr -> (Int  ×  Int) -> IProp GF :=
  fun (_ : Ptr) (_ : Int × Int) => iprop% ⌜ true ⌝
