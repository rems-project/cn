#include <stdlib.h>

struct List {
  unsigned int value;
  struct List *next;
};

/*@
datatype IntList {
  Nil {},
  Cons { integer head, IntList tail }
}

function (boolean) validCons(integer head, IntList tail) {
  match tail {
    Nil {} => { true }
    Cons { head: next, tail: _ } => { head <= next }
  }
}

datatype I32Option {
  SomeI32 { integer data },
  NoneI32 {}
}

function (integer) getSome(datatype I32Option opt) {
  match (opt) {
    SomeI32 { data: data } => { data }
    NoneI32 {} => { default<integer> }
  }
}

function (boolean) isNone(datatype I32Option opt) {
  match (opt) {
    SomeI32 { data: _ } => { false }
    NoneI32 {} => { true }
  }
}

predicate [rec] IntList ListSegmentAux(pointer from, pointer to, I32Option prev) {
  if (ptr_eq(from,to)) {
    return Nil {};
  } else {
    take head = Owned<struct List>(from);
    assert(!isNone(prev) implies (getSome(prev) <= head.value));
    take tail = ListSegmentAux(head.next, to, SomeI32{data: head.value});
    return Cons { head: head.value, tail: tail };
  }
}

predicate IntList ListSegment(pointer from, pointer to) {
  take L = ListSegmentAux(from, to, NoneI32{});
  return L;
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


/*@
function [rec] (IntList) insertList(boolean dups, integer x, IntList xs) {
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
  struct List *node = (struct List *)malloc(sizeof(struct List));
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
