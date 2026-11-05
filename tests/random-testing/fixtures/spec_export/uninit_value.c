void reads_uninitialized(int *p)
/*@ requires take before = RW<int>(p);
    ensures take after = W<int>(p);
            after == before; @*/
{
  (void)p;
}

int unaffected(void)
/*@ ensures return == 7; @*/
{
  return 7;
}
