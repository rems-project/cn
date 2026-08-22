
/*@
predicate (integer) Owned_char_wrapper(pointer p) {
    take c = RW<char>(p);
    return c;
}

predicate void Owned_char_wrapper_void(pointer p) {
    take c = RW<char>(p);
    return;
}
@*/

/* 
Fulminate accepts this splitting up of ownership but CN proof rejects this file.
Spec contains a mix of contiguous `RW` ranges, unused maps and predicates with `void` return type,
triggering Fulminate's optimisations for conjunctions and checking they work in tandem.
*/
void
naive_memcpy (char *dst, char *src, int n)
/*@ requires take dstStart_half_1 = each (integer j; 0 <= j && j < n && j % 2 == 0)
                                  {RW(array_shift(dst, j))};
             take dstStart_half_2 = each (integer j; 0 <= j && j < n && j % 2 == 1)
                                  {Owned_char_wrapper_void(array_shift(dst, j))};
             take srcStart = each (integer j; 0 <= j && j < n)
                                  {Owned_char_wrapper(array_shift(src, j))};
    ensures take dstEnd = each (integer j; 0 <= j && j < n)
                               {RW(array_shift(dst, j))};
            take srcEnd = each (integer j; 0 <= j && j < n)
                               {RW(array_shift(src, j))};
            srcEnd == srcStart;
            each (integer k; 0 <= k && k < n) {dstEnd[k] == srcStart[k]};
@*/
{
  int i;
  for (i = 0; i < n; i = i + 1)
  /*@ inv take dstInv = each (integer j; 0 <= j && j < n)
                             {RW(array_shift(dst, j))};
          take srcInv = each (integer j; 0 <= j && j < n)
                             {RW(array_shift(src, j))};
          // srcInv == srcStart;
          each (integer j; 0 <= j && j < i) {dstInv[j] == srcStart[j]};
          0 <= i;
          {dst} unchanged;
          {src} unchanged;
          {n} unchanged; @*/
  {
    /*@ focus RW<char>, i; @*/
    /*@ instantiate good<char>, i; @*/
    dst[i] = src[i];
  }
}

int main(void)
/*@ trusted; @*/
{
  char src[5] = "hello";
  char dst[5];
  naive_memcpy(dst, src, 5);
}
