int foo(int p)
/*@
requires
  cn_ghost boolean b;
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
  return foo(6);
}