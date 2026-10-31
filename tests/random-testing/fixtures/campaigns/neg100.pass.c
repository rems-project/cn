int negative100(int x)
/*@ requires x == 100;
    ensures return == -100;
@*/
{
  return -x;
}
