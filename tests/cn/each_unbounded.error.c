void f(int *p)
/*@
  requires take X = each (integer i; i < 10) { RW<int>(array_shift<int>(p, i)) };
  ensures take Y = each (integer i; i < 10) { RW<int>(array_shift<int>(p, i)) };
@*/
{
}
