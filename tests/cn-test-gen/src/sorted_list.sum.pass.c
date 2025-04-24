// Sorted list

struct List {
  unsigned int value;
  struct List *next;
};

/*@
datatype IntList {
  Nil {},
  Cons { u32 head, IntList tail }
}

function (boolean) validCons(u32 head, IntList tail) {
  match tail {
    Nil {} => { true }
    Cons { head: next, tail: _ } => { head <= next }
  }
}

predicate IntList ListSegment(pointer from, pointer to) {
  if (ptr_eq(from,to)) {
    return Nil {};
  } else {
    take head = Owned<struct List>(from);
    take tail = ListSegment(head.next, to);
    assert(validCons(head.value,tail));
    return Cons { head: head.value, tail: tail };
  }
}
@*/

// This is a valid spec, even though to verify with CN we'd need a loop
// invariant.
unsigned int sum(struct List *xs)
/*@
  requires
    take l1 = ListSegment(xs,NULL);
  ensures
    take l2 = ListSegment(xs,NULL);
    l1 == l2;
    true;
@*/
{
  unsigned int result = 0;
  while (xs) {
    result += xs->value;
    xs = xs->next;
  }
  return result;
}
