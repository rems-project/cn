#include "./snoc.h"

/*@
function [rec] (datatype List) RevList(datatype List L) {
  match L {
    Nil {} => {
      Nil {}
    }
    Cons {Head : H, Tail : T}  => {
      Snoc (RevList(T), H)
    }
  }
}
@*/
