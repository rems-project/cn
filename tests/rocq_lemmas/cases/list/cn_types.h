/*@
datatype List {
  Nil {},
  Cons {i32 Head, datatype List Tail}
}

predicate [rec] (datatype List) SLList_At(pointer p) {
  if (is_null(p)) {
    return Nil{};
  } else {
    take H = Owned<struct sllist>(p);
    take T = SLList_At(H.tail);
    return (Cons { Head: H.head, Tail: T });
  }
}
@*/
