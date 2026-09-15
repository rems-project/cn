int abs(int x)
/*@ requires MINi32() < x;
    ensures return == ((x >= 0) ? x : (0-x));
@*/
{
  if (x >= 0) {
    return x;
  }
  else {
    return -x;
  }
}
