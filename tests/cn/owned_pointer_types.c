// Ownership of several pointer types: each needs its own owned_* function.
// Fulminate used to generate one for the first pointer type only, so the
// instrumented C called undefined functions.

struct s {
  int x;
};

int f(char **in, long *l, struct s *p, int n)
/*@ requires take P = RW<char *>(in); take L = RW<long>(l); take S = RW<struct s>(p);
             0 <= n; n < 10;
    ensures take P2 = RW<char *>(in); ptr_eq(P2, P);
            take L2 = RW<long>(l); L2 == L;
            take S2 = RW<struct s>(p); S2 == S; @*/
{
  int k = 0;
  while (k < n)
    /*@ inv take Q = RW<char *>(in); ptr_eq(Q, P);
            take LQ = RW<long>(l); LQ == L;
            take SQ = RW<struct s>(p); SQ == S;
            {in} unchanged; {l} unchanged; {p} unchanged; {n} unchanged;
            0 <= k; k <= n; @*/
  {
    k++;
  }
  return 0;
}

int two_pointer_types(int **p, char **q)
/*@ requires take A = RW<int *>(p); take B = RW<char *>(q);
    ensures take A2 = RW<int *>(p); take B2 = RW<char *>(q); @*/
{
  return 0;
}

int main(void)
/*@ trusted; @*/
{
  char c = 0;
  char *cp = &c;
  long l = 0;
  struct s v = {0};
  int x = 0;
  int *xp = &x;
  two_pointer_types(&xp, &cp);
  return f(&cp, &l, &v, 3);
}
