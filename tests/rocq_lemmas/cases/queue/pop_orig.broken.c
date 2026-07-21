#include "./headers.h"

int pop_queue (struct queue *q)
{
  struct queue_cell* h = q->front;
  if (h == q->back) {
    int x = h->first;
    free_queue_cell(h);
    q->front = 0;
    q->back = 0;
    return x;
  } else {
    int x = h->first;
    q->front = h->next;
    free_queue_cell(h);
    return x;
  }
}
