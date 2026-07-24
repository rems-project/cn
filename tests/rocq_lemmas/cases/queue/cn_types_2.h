/*@
predicate [rec] (datatype List) QueueFB (pointer front, pointer back) {
  if (is_null(front)) {
    return Nil{};
  } else {
    take B = Owned<struct queue_cell>(back);
    assert (is_null(B.next));
    assert (ptr_eq(front, back) || !addr_eq(front, back));
    take L = QueueAux (front, back);
    return Snoc(L, B.first);
  }
}
@*/
