int
direct (void)
/*@ ensures return == 3; @*/
{
  int x = 1;
  int y = 2;

  ++x;
  --y;
  if (!--y)
    y = ++x;
  return y;
}

struct has_int {
  int x;
};

int
indirect (int *p, struct has_int *q, int *a)
/*@ requires take P = RW(p);
             take S = RW(q);
             take A = RW(a);
             P < 100;
             S.x > 0;
             A < 100;
    ensures take P2 = RW(p);
            take S2 = RW(q);
            take A2 = RW(a);
            P2 == P + 1;
            S2.x == S.x - 1;
            A2 == A + 1;
            return == P2; @*/
{
  int i = 0;

  --q->x;
  ++a[i];
  return ++*p;
}

int main(void)
/*@ trusted; @*/
{
  struct has_int hs = {.x = 5};
  int p[1] = {1};
  int a = 7;
  direct();
  indirect(p, &hs, &a);
}
