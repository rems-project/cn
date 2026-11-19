#include "./headers.h"

unsigned int length (struct sllist *l)
{
  if (l == 0) {
    return 0;
  } else {
    return 1 + length(l->tail);
  }
}
