/* Preserve the old signed-int to u64 conversion, including negative sizes.
   Integer indices explicitly retain the nonnegative domain implicit in u64;
   unsigned_size is always below 2^64, so the old upper bound is preserved too. */
/*@
function (integer) unsigned_size(integer size) {
  mod(size, 18446744073709551616)
}
@*/

void bounds1(int size, int* p)
/*@
requires
  take a1 = each(integer i; 0 <= i && i < unsigned_size(size)) { Owned<int>(array_shift<int>(p,i)) };
ensures
  take a2 = each(integer i; 0 <= i && i < unsigned_size(size)) { Owned<int>(array_shift<int>(p,i)) };
@*/
{}

void bounds2(int size, int* p)
/*@
requires
  take a1 = each(integer i; 0 <= i && i < unsigned_size(size)) { Owned<int>(array_shift<int>(p,i)) };
ensures
  take a2 = each(integer i; 0 <= i && i < unsigned_size(size)) { Owned<int>(array_shift<int>(p,i)) };
@*/
{}

void bounds3(int size, int* p)
/*@
requires
  take a1 = each (integer i; -1 < i && i < size) { Owned<int>(array_shift<int>(p,i)) };
ensures
  take a2 = each (integer i; -1 < i && i < size) { Owned<int>(array_shift<int>(p,i)) };
@*/
{}

void bounds4(int* p)
/*@
requires
  take a1 = each (integer i; i == 1 || i == 2 || i == 5) { Owned<int>(array_shift<int>(p,i)) };
ensures
  take a2 = each (integer i; i == 1 || i == 2 || i == 5) { Owned<int>(array_shift<int>(p,i)) };
@*/
{}
