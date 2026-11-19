#include "./headers.h"
#include "./append.h"
#include "./rev.h"
#include "./rev_lemmas.h"

struct sllist* rev_loop(struct sllist *l)
/*@ requires take L = SLList_At(l);
    ensures  take L_post = SLList_At(return);
             L_post == RevList(L);
@*/
{
  struct sllist *last = 0;
  struct sllist *cur = l;
  /*@ apply Append_Nil_RList(RevList(L)); @*/
  while(1)
  /*@ inv take Last = SLList_At(last);
          take Cur = SLList_At(cur);
          Append(RevList(Cur), Last) == RevList(L);
  @*/
  {
    if (cur == 0) {
      /*@ unfold RevList(Nil{}); @*/
      /*@ unfold Append(Nil{}, Last); @*/
      return last;
    }
    struct sllist *tmp = cur->tail;
    cur->tail = last;
    last = cur;
    cur = tmp;
    /*@ unfold RevList(Cur); @*/
    /*@ apply Append_Cons_RList(RevList(Tl(Cur)), Hd(Cur), Last); @*/
  }
}
