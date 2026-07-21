struct sllist* slnil()
/*@ ensures take Ret = SLList_At(return);
            Ret == Nil{};
 @*/
{
  return 0;
}

struct sllist* slcons(int h, struct sllist* t)
/*@ requires take T = SLList_At(t);
    ensures take Ret = SLList_At(return);
            Ret == Cons{ Head: h, Tail: T};
 @*/
{
  struct sllist *p = malloc__sllist();
  p->head = h;
  p->tail = t;
  return p;
}
