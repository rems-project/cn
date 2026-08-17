struct list {
  int value;
  struct list *next;
};

/*@
datatype list_cn {
  Nil {},
  Cons {i32 head, datatype list_cn tail}
}

predicate [rec] (datatype list_cn) ListSeg(pointer p, pointer q) {
  if (ptr_eq(p, q)) {
    return Nil {};
  } else {
    assert (!is_null(p));
    take node = RW<struct list>(p);
    take tail = ListSeg(node.next, q);
    return Cons {head: node.value, tail: tail};
  }
}

predicate [rec] (datatype list_cn) List(pointer p) {
  if (is_null(p)) {
    return Nil {};
  } else {
    take node = RW<struct list>(p);
    take tail = List(node.next);
    return Cons {head: node.value, tail: tail};
  }
}

lemma ListSeg_List(pointer p, pointer q)
  requires
    take segment = ListSeg(p, q);
    take suffix = List(q);
  ensures
    take whole = List(p);
@*/
