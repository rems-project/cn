/*@
predicate void False(pointer p, integer i) {
  assert (i != 0);
  return;
}
@*/

void f (int *p)
/*@ requires take f1 = each(integer i; 0 <= i && i <= 0) { False(p + i, i) };
    ensures false; @*/
{
  /*@ focus False, 0; @*/
}

int main(void)
/*@ trusted; @*/
{
  int p[5] = {1, 2, 3, 4, 5};
  f(p);
  return 0;
}
