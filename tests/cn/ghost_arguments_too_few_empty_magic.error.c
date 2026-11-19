int foo(int p)
/*@
requires
  cn_ghost i32 m, i32 n;
  true;
ensures
  true;
@*/
{
  return p;
}

int main()
{
  return foo(6 /*@ @*/);
}