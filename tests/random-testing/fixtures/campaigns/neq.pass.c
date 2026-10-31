int ret_17_except_good(int x)
/*@ requires MINi32() <= x; x <= MAXi32();
             x != 10;
    ensures return == 17; @*/
{
  if (x == 10) {
    return 23;
  } else {
    return 17;
  }
}
