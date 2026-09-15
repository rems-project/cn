
/* A test case about modulus and masking, in both integers and pointers.
   Originally introduced as a test case because the pointer variant was causing
   issues for the SMT solver for unclear reasons.
*/

/* Sep 2026: in-progress. RB tried porting from integers to bitvectors but incomplete */

#include <stddef.h>

typedef unsigned long long u64;

#ifndef CN_UTILS
void *cn_aligned_alloc(size_t alignment, size_t size);
void cn_free_sized(void *ptr, size_t size);
#endif

enum { SHIFT_AMOUNT = 5 };

/*@
function [rec] (boolean) isPow2(integer n) {
    n != 0 && (n == 1 || (mod(n, 2) == 0 && isPow2(n / 2)))
}

spec cn_aligned_alloc(integer alignment, integer size);
requires alignment > 0;
         isPow2(alignment);
ensures  (size == 0) ? is_null(return) : !is_null(return);
         mod((integer) return, alignment) == 0;
         (integer) return <= (integer) return + size;
         take O = each (integer i; i < size) {
             RW<char>(array_shift<char>(return, i))
         };

spec cn_free_sized(pointer ptr, integer size);
requires take O = each (integer i; i < size) {
             RW<char>(array_shift<char>(ptr, i))
         };
@*/

u64 foo_integer(u64 y)
/*@ requires mod(y, shift_left(1, SHIFT_AMOUNT)) == 0; @*/
/* y = 42 */
/* shift_left(1u64, 5u64) = 0...100000*/
{
  u64 x = y;
  x &= ~((1UL << SHIFT_AMOUNT) - 1);
  /*@ assert (x == y); @*/
  return x;
}

int *foo(int *p)
/*@ requires mod((integer) p, shift_left(1, SHIFT_AMOUNT)) == 0; @*/
{
  u64 x = ((u64)p);
  int *p2;

  x &= ~((1UL << SHIFT_AMOUNT) - 1);

  p2 = ((int *)x);
  /*@ assert ((integer) p2 == (integer) p); @*/
  return p2;
}

int main(void)
/*@ trusted; @*/
{
  u64 r1 = foo_integer(128);
  int *p = cn_aligned_alloc(32, sizeof(int));
  int *r2 = foo(p);
  cn_free_sized(p, sizeof(int));
}
