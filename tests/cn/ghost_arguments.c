int foo(int p)
/*@
requires
  cn_ghost integer n, integer m, integer k;
  n + m + k == p;
ensures
  return == n + m + k;
@*/
{
  return p;
}

int main()
{
  int x = 3;
  int v = 1;
  int* p = &v;
  int y = foo(6 /*@ 2, x + *p - *p, *p @*/);
  /*@  assert(6 == y); @*/
  return 0;
}
