void swap_pair(unsigned long int *pair)
/*@
requires
   take pairStart = each (integer j; 0 <= j && j < 2) {RW(array_shift(pair, j))};
ensures
    take pairEnd = each (integer j; 0 <= j && j < 2) {RW(array_shift(pair, j))};
    pairEnd[0] == pairStart[1];
    pairEnd[1] == pairStart[0];
@*/
{
    /*@ focus RW<unsigned long int>, 0; @*/
    /*@ focus RW<unsigned long int>, 1; @*/
    /*@ instantiate 0; @*/
    /*@ instantiate 1; @*/
    unsigned long int tmp = pair[0];
    pair[0] = pair[1];
    pair[1] = tmp;
}

int main(void)
/*@ trusted; @*/
{
  unsigned long int pair[2] = {4, 5};
  swap_pair(pair);
}
