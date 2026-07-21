/*@
function (i32) Hd (datatype List L) {
  match L {
    Nil {} => {
      0i32
    }
    Cons {Head : H, Tail : _} => {
      H
    }
  }
}

function (datatype List) Tl (datatype List L) {
  match L {
    Nil {} => {
      Nil{}
    }
    Cons {Head : _, Tail : T} => {
      T
    }
  }
}
@*/
