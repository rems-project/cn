// Loops with invariants: single-statement bodies, do-while loops, and loops
// left by return or break or continued (rems-project/cn#652).

int while_single(int x)
/*@ requires 0 <= x; x <= 50;
    ensures return == 0; @*/
{
  while (x)
    /*@ inv 0 <= x; x <= 50; @*/
    x = x - 1;
  return x;
}

int for_single(int n)
/*@ requires 0 <= n; n < 10;
    ensures return == n; @*/
{
  int s = 0;
  int i;
  for (i = 0; i < n; i++)
    /*@ inv 0 <= i; i <= n; s == i; {n} unchanged; @*/
    s++;
  return s;
}

int while_continue(int n)
/*@ requires 0 <= n; n < 10;
    ensures return == n; @*/
{
  int i = 0;
  while (i < n)
    /*@ inv 0 <= i; i <= n; {n} unchanged; @*/
  {
    i++;
    if (i % 2)
      continue;
  }
  return i;
}

int do_return(int x)
/*@ ensures return == 0; @*/
{
  do
    /*@ inv true; @*/
  {
    if (x)
      return 0;
  } while (0);
  return 0;
}

int do_break(int x)
/*@ ensures return == 0; @*/
{
  do
    /*@ inv true; @*/
  {
    if (x)
      break;
  } while (0);
  return 0;
}

int main(void)
/*@ trusted; @*/
{
  while_single(50);
  for_single(9);
  while_continue(9);
  do_return(0);
  do_return(1);
  do_break(0);
  do_break(1);
  return 0;
}
