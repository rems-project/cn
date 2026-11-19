unsigned int between(unsigned int p, unsigned int q)
/*@
  requires
    p <= q;
  ensures
    return >= p;
    return <= q;
@*/
{
  return q;
}
