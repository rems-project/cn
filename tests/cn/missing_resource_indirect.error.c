
/* this is a wrong specification. */
/* this function doesn't create an integer in memory */

/*@
predicate (integer) RW_Wrapper (pointer p) {
  take I = RW<int>(p);
  return I;
}
@*/

int
f (int *p, int x)
/*@ requires x < 12;
    ensures return < 12;
            take Resource_From_Nothing = RW_Wrapper(p); @*/
{
  return x;
}



