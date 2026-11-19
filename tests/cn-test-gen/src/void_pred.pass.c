#include <stddef.h>

extern void cn_free_sized(void *ptr, size_t size);

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
  cn_free_sized(p, sizeof(struct foo));
}
