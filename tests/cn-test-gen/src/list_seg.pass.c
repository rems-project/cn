struct List {
  unsigned int value;
  struct List *next;
};

/*@
datatype IntList {
  Nil {},
  Cons { u32 head, IntList tail }
}

predicate IntList ListSegment(pointer from, pointer to) {
  if (ptr_eq(from,to)) {
    return Nil {};
  } else {
    take head = Owned<struct List>(from);
    take tail = ListSegment(head.next, to);
    return Cons { head: head.value, tail: tail };
  }
}
@*/

unsigned int sum(struct List *xs)
/*@
  requires
    take l1 = ListSegment(xs,NULL);
  ensures
    take l2 = ListSegment(xs,NULL);
    l1 == l2;
@*/
{
  unsigned int result = 0;
  while (xs) {
    result += xs->value;
    xs = xs->next;
  }
  return result;
}
