/*@
predicate [rec] (datatype List) QueuePtr_At (pointer q) {
  take Q = Owned<struct queue>(q);
  assert (   (is_null(Q.front)  && is_null(Q.back)) 
          || (!is_null(Q.front) && !is_null(Q.back)));
  take L = QueueFB(Q.front, Q.back);
  return L;
}
@*/
