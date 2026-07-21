#include "./headers.h"

struct queue* empty_queue ()
{
  struct queue *p = malloc_queue();
  p->front = 0;
  p->back = 0;
  return p;
}
