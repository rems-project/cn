int foo(int p)
/*@
requires
  cn_ghost i32 n, i32 m, i32 k;
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
  return foo(6 /*@ 2i32, x, *p @*/);
}