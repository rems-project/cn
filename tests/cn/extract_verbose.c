

int
f (int x, int *p, int *q)
/*@ requires take p_arr = each(integer i; 0 <= i && i < 10) {RW(array_shift(p, i))};
             take q_arr = each(integer i; 0 <= i && i < 12) {W<int>(array_shift(q, i))};
    ensures take p_arr2 = each(integer i; 0 <= i && i < 10) {RW(array_shift(p, i))};
            take q_arr2 = each(integer i; 0 <= i && i < 12) {W<int>(array_shift(q, i))}; @*/
{
  /*@ focus RW<int>, 1; @*/
  /*@ focus W<int>, 1; @*/
  /*@ focus RW<int>, 1u64; @*/
  /*@ focus W<int>, 1u64; @*/
  return 1;
}

int main(void)
/*@ trusted; @*/
{
  int p[10] = {1, 2, 3, 4, 5, 6, 9, 10, 11, 12};
  int q[12] = {1, 2, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5};
  f(42, p, q);
}
