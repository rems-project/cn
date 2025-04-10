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

predicate IntList ListSegmentAux(pointer from, pointer to, u32 prev) {
  if (ptr_eq(from,to)) {
    return Nil {};
  } else {
    take head = Owned<struct List>(from);
    assert(prev <= head.value);
    take tail = ListSegmentAux(head.next, to, head.value);
    return Cons { head: head.value, tail: tail };
  }
}

predicate IntList ListSegment(pointer from, pointer to) {
  if (ptr_eq(from,to)) {
    return Nil {};
  } else {
    take head = Owned<struct List>(from);
    take tail = ListSegmentAux(head.next, to, head.value);
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

void *cn_malloc(unsigned long size);

/*@
function [rec] (IntList) insertList(boolean dups, u32 x, IntList xs) {
  match xs {
    Nil {} => { Cons { head: x, tail: Nil {} } }
    Cons { head: head, tail: tail } => {
      if (head < x) {
        Cons { head: head, tail: insertList(dups, x,tail) }
      } else {
        if (!dups && head == x) {
          xs
        } else {
          Cons { head: x, tail: xs }
        }
      }
    }
  }
}
@*/

void insert(unsigned int x, struct List **xs)
/*@
  requires
    take list_ptr = Owned<struct List*>(xs);
    take list = ListSegment(list_ptr,NULL);
  ensures
    take new_list_ptr = Owned<struct List*>(xs);
    take new_list = ListSegment(new_list_ptr,NULL);
    new_list == insertList(true,x,list);
@*/
{
  struct List *node = (struct List *)cn_malloc(sizeof(struct List));
  node->value = x;

  struct List *prev = 0;
  struct List *cur = *xs;
  while (cur && cur->value < x) {
    prev = cur;
    cur = cur->next;
  }

  if (prev) {
    prev->next = node;
    node->next = cur;
  } else {
    node->next = *xs;
    *xs = node;
  }
}
