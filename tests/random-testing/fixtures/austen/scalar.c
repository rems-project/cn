/* CN must preprocess this source as Austen does: with both testing macros, a
   forced include, and a command-line macro. */
#ifndef __CN_TEST
#error "__CN_TEST is not defined"
#endif
#ifndef __AUSTEN_TEST
#error "__AUSTEN_TEST is not defined"
#endif
#ifndef FORCED
#error "the forced include was not read"
#endif

int increment(int x)
/*@ requires x < 100;
    ensures return == x + 1; @*/
{
  return x + STEP;
}

int decrement(int x)
/*@ requires x > 0;
    ensures return == x - 1; @*/
{
  return x - STEP;
}

static int twice(int x)
/*@ requires -100 < x; x < 100;
    ensures return == 2 * x; @*/
{
  return x * STEP * 2;
}
