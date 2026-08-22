struct s {
  int x;
  int y;
};

void arrow_access_1()
{
  struct s origin = { .x = 0, .y = 0 };
  /*@ assert (origin.x == 0); @*/ // -- member
  struct s *p = &origin;
  struct s *q = &origin;

  /*@ assert (p->x == 0); @*/   // Arrow access
  /*@ assert ((*p).x == 0); @*/ // ... desugared as this
  (*p).y = 7;
  /*@ assert (q->y == 7); @*/
}

void arrow_access_2 (struct s *origin)
/*@
requires
  take Or = RW<struct s>(origin);
  origin->y == 0;
ensures
  take Or_ = RW<struct s>(origin);
  origin->y == 7;
  (*origin).y == 7;
@*/
{
  origin->y = 7;
}
