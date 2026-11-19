/*@
lemma push_lemma (pointer front, pointer p)
  requires
      ptr_eq(front, p) || !addr_eq(front, p);
      take Q = QueueAux(front, p);
      take P = Owned<struct queue_cell>(p);
      !is_null(P.next);
  ensures
      ptr_eq(front, P.next) || !addr_eq(front, P.next);
      take Q_post = QueueAux(front, P.next);
      Q_post == Snoc(Q, P.first);
@*/