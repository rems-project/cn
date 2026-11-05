#include <stdlib.h>

/* Austen tracks the standard allocator; free also accepts the NULL branch. */

struct foo {
  int bar;
};

/*@
predicate void OwnedOrNULL(pointer p) {
  if (is_null(p)) {
    return;
  } else {
    take x = W<struct foo>(p);
    return;
  }
}
@*/

void free_foo(struct foo *p)
/*@ requires take x = OwnedOrNULL(p);
    ensures true;
@*/
{
  free(p);
}
