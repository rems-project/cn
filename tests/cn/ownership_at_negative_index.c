int f(int *p)
/*@ requires take vs = each(integer i; i == -1) { RW<int>(array_shift(p,i)) };
    ensures take ws = each(integer i; i == -1) { RW<int>(array_shift(p,i)) };
@*/
{
  /*@ focus RW<int>, -1; @*/
  /*@ instantiate -1; @*/
  return p[-1];
}

int main(void)
/*@ trusted; @*/
{
  int p[5] = {1, 2, 3, 4, 5};
  int r = f(p);
}
