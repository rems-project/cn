
/* A test case about modulus and masking, in both integers and pointers.
   Originally introduced as a test case because the pointer variant was causing
   issues for the SMT solver for unclear reasons.
*/

#include <stddef.h>

typedef unsigned long long u64;

#ifndef CN_UTILS
void *cn_aligned_alloc(size_t alignment, size_t size);
void cn_free_sized(void *ptr, size_t size);
#endif

enum { SHIFT_AMOUNT = 5 };

/*@
function (boolean) isPow2u64(u64 n) {
    n != 0u64 && (n & (n - 1u64)) == 0u64
}

spec cn_aligned_alloc(u64 alignment, u64 size);
requires alignment > 0u64;
         isPow2u64(alignment);
ensures  (size == 0u64) ? is_null(return) : !is_null(return);
         (u64)return % alignment == 0u64;
         (u64)return <= (u64)return + size;
         take O = each (u64 i; i < size) {
             RW<char>(array_shift<char>(return, i))
         };

spec cn_free_sized(pointer ptr, u64 size);
requires take O = each (u64 i; i < size) {
             RW<char>(array_shift<char>(ptr, i))
         };
@*/

u64 foo_integer(u64 y)
/*@ requires mod(y, shift_left(1u64, ((u64) SHIFT_AMOUNT))) == 0u64; @*/
/* y = 42 */
/* shift_left(1u64, 5u64) = 0...100000*/
{
  u64 x = y;
  x &= ~((1UL << SHIFT_AMOUNT) - 1);
  /*@ assert (x == y); @*/
  return x;
}

int *foo(int *p)
/*@ requires let p_u64 = (u64) p;
             mod(p_u64, shift_left(1u64, ((u64) SHIFT_AMOUNT))) == 0u64; @*/
{
  u64 x = ((u64)p);
  int *p2;

  x &= ~((1UL << SHIFT_AMOUNT) - 1);

  p2 = ((int *)x);
  /*@ assert (((u64) p2) == ((u64) p)); @*/
  return p2;
}

int main(void) {
  u64 r1 = foo_integer(128);
  int *p = cn_aligned_alloc(32, sizeof(int));
  int *r2 = foo(p);
  cn_free_sized(p, sizeof(int));
}
