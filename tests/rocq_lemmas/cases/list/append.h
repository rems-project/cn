/*@
function [rec] (datatype List) Append(datatype List L1, datatype List L2) {
  match L1 {
    Nil {} => {
      L2
    }
    Cons {Head : H, Tail : T}  => {
      Cons {Head: H, Tail: Append(T, L2)}
    }
  }
}
@*/
