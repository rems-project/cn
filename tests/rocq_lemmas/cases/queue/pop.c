#include "./headers.h"
#include "./pop_lemma.h"

int pop_queue (struct queue *q)
/*@ requires take Q = QueuePtr_At(q);
             Q != Nil{};
    ensures take Q_post = QueuePtr_At(q);
            Q_post == Tl(Q);
            return == Hd(Q);
@*/
{
  /*@ split_case is_null(q->front); @*/
  struct queue_cell* h = q->front;
  if (h == q->back) {
    /*@ assert ((alloc_id) h == (alloc_id) (q->back)); @*/
    int x = h->first;
    free_queue_cell(h);
    q->front = 0;
    q->back = 0;
    /*@ unfold Snoc(Nil{}, x); @*/
    return x;
  } else {
    int x = h->first;
    /*@ apply pop_lemma(h->next, q->back, x); @*/
    q->front = h->next;
    free_queue_cell(h);
    return x;
  }
}
