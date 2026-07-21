#include "./headers.h" 

void push_induction(struct queue_cell* front
        , struct queue_cell* second_last
        , struct queue_cell* last)
/*@
  requires
      ptr_eq(front, second_last) || !addr_eq(front, second_last);
      take Q = QueueAux(front, second_last);
      take Second_last = Owned(second_last);
      ptr_eq(Second_last.next, last);
      take Last = Owned(last);
  ensures
      ptr_eq(front, last) || !addr_eq(front, last);
      take Q_post = QueueAux(front, last);
      take Last2 = Owned(last);
      Q_post == Snoc(Q, Second_last.first);
      Last == Last2;
@*/
{
    if (front == second_last) {
        /*@ unfold Snoc(Q, Second_last.first); @*/
        return;
    } else {
        push_induction(front->next, second_last, last);
        /*@ unfold Snoc(Q, Second_last.first); @*/
        return;
    }
}

void push_queue (int x, struct queue *q)
/*@ requires take before = QueuePtr_At(q);
    ensures take after = QueuePtr_At(q);
            after == Snoc (before, x);
@*/
{
  struct queue_cell *c = malloc_queue_cell();
  c->first = x;
  c->next = 0;
  if (q->back == 0) {
    q->front = c;
    q->back = c;
    return;
  } else {
    struct queue_cell *oldback = q->back;
    q->back->next = c;
    q->back = c;
    push_induction(q->front, oldback, c);
    return;
  }
}
