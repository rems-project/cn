
/* A test case about modulus and masking, in both integers and pointers.
   Originally introduced as a test case because the pointer variant was causing
   issues for the SMT solver for unclear reasons.

   This variant deliberately passes a misaligned pointer to `foo`, so both
   CN verification and Fulminate's runtime precondition check fail
   deterministically.
*/

typedef unsigned long long u64;

enum {
  SHIFT_AMOUNT = 5
};

u64
foo_integer (u64 y)
/*@ requires mod(y, shift_left(1u64, ((u64) SHIFT_AMOUNT))) == 0u64; @*/
/* y = 42 */
/* shift_left(1u64, 5u64) = 0...100000*/
{
  u64 x = y;
  x &= ~ ((1UL << SHIFT_AMOUNT) - 1);
  /*@ assert (x == y); @*/
  return x;
}



int *
foo (int *p)
/*@ requires let p_u64 = (u64) p;
             mod(p_u64, shift_left(1u64, ((u64) SHIFT_AMOUNT))) == 0u64; @*/
{
  u64 x = ((u64) p);
  int *p2;

  x &= ~ ((1UL << SHIFT_AMOUNT) - 1);

  p2 = ((int *) x);
  /*@ assert (((u64) p2) == ((u64) p)); @*/
  return p2;
}

int main(void)
{
  u64 r1 = foo_integer(128);
  int buf[8];
  int *p = (((u64) &buf[0]) % 32 == 0) ? &buf[1] : &buf[0];
  int *r2 = foo(p);
}
