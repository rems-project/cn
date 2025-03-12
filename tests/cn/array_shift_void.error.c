#include <stdlib.h>
#include <string.h>

void *_malloc(size_t size)
/*@ trusted;
ensures
  take A = Alloc(return);
  A.base == (u64) return;
  A.size == size;
  take B = each(u64 i; i < size) {W<unsigned char>(array_shift(return, i))};
@*/
{
    return malloc(size);
}
