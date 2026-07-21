struct sllist {
  int head;
  struct sllist* tail;
};

extern struct sllist *malloc__sllist();
/*@ spec malloc__sllist();
    requires true;
    ensures take R = Block<struct sllist>(return);
@*/

extern void free__sllist (struct sllist *p);
/*@ spec free__sllist(pointer p);
    requires take P = Block<struct sllist>(p);
    ensures true;
@*/

