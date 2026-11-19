#include "./headers.h"

struct sllist* slcopy (struct sllist *l)
/*@ requires take L = SLList_At(l);
    ensures take L_ = SLList_At(l);
            take Ret = SLList_At(return);
            L == L_;
            L == Ret;
@*/
{
  if (l == 0) {
    return slnil();
  } else {
    struct sllist *new_tail = slcopy(l->tail);
    return slcons(l->head, new_tail);
  }
}
