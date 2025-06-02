int foo(int p)
/*@
requires
  ghost i32 n;
  true;
ensures
  true;
@*/
{
  return p;
}

int main()
{
  int x = 3;
  return foo(6 /*@ x, x @*/);
}