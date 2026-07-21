#include "./headers.h"

/*@
type_synonym result = { datatype List after, datatype List before }

predicate [rec] (result) Queue_pop_lemma(pointer front, pointer back, i32 popped) {
  if (is_null(front)) {
    return { after: Nil{}, before: Snoc(Nil{}, popped) };
  } else {
    take B = Owned<struct queue_cell>(back);
    assert (is_null(B.next));
    take L = QueueAux (front, back);
    return { after: Snoc(L, B.first), before: Snoc(Cons {Head: popped, Tail: L}, B.first) };
  }
}
@*/

void snoc_fact(struct queue_cell *front, struct queue_cell *back, int x)
/*@
requires
    take Q = QueueAux(front, back);
    take B = Owned<struct queue_cell>(back);
ensures
    take Q_post = QueueAux(front, back);
    take B_post = Owned<struct queue_cell>(back);
    Q == Q_post; B == B_post;
    let L = Snoc (Cons{Head: x, Tail: Q}, B.first);
    Hd(L) == x;
    Tl(L) == Snoc (Q, B.first);
@*/
{
    /*@ unfold Snoc (Cons{Head: x, Tail: Q}, B.first); @*/
}

void snoc_fact_unified(struct queue_cell *front, struct queue_cell *back, int x)
/*@
requires
      take AB = Queue_pop_lemma(front, back, x);
ensures
      take NewAB = Queue_pop_lemma(front, back, x);
      AB == NewAB;
      AB.after == Tl(AB.before);
      x == Hd(AB.before);
@*/
{
    if (!front) {
        /*@ unfold Snoc(Nil{}, x); @*/
    } else {
        snoc_fact(front, back, x);
    }
}

int pop_queue (struct queue *q)
/*@ requires take before = QueuePtr_At(q);
             before != Nil{};
    ensures take after = QueuePtr_At(q);
            after == Tl(before);
            return == Hd(before);
@*/
{
  /*@ split_case is_null(q->front); @*/
  struct queue_cell* h = q->front;
  /*@ split_case ptr_eq(h, q->back); @*/
  int x = h->first;
  q->front = h->next;
  free_queue_cell(h);
  if (!q->front) q->back = 0;
  snoc_fact_unified(q->front, q->back, x);
  return x;
}

