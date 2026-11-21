/* A resource request that fails on an unreachable path (inconsistent context)
   used to hit an assertion in resource inference instead of being accepted. */

void die(int *p);
/*@ spec die(pointer p);
    requires take X = RW<int>(p);
    ensures false; @*/

/* resource requested by the postcondition after a call whose postcondition is false */
void post_after_call(int *p, int b)
/*@ requires take X = RW<int>(p);
    ensures take X2 = RW<int>(p); @*/
{
  if (b)
    die(p);
}

/* load and store after a call whose postcondition is false */
int load_after_call(int *p)
/*@ requires take X = RW<int>(p); @*/
{
  die(p);
  return *p;
}

void store_after_call(int *p)
/*@ requires take X = RW<int>(p); @*/
{
  die(p);
  *p = 1;
}

/* inconsistent precondition */
int load_pre_false(int *p)
/*@ requires false; @*/
{
  return *p;
}

void post_pre_false(int *p)
/*@ requires false;
    ensures take X = RW<int>(p); @*/
{
}
