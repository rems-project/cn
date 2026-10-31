/* Integer aliases must reach the exit oracle, including owned pre-state values. */
int wrong_increment(int *p)
/*@ requires take before = RW<int>(p);
             before < MAXi32();
    ensures take after = RW<int>(p);
            after == before + 1;
            return == after; @*/
{
  return *p;
}
