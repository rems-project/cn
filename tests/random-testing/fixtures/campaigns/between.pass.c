unsigned int between(unsigned int p, unsigned int q)
/*@
  requires
    0 <= p; p <= MAXu32();
    0 <= q; q <= MAXu32();
    p <= q;
  ensures
    return >= p;
    return <= q;
@*/
{
  return q;
}
