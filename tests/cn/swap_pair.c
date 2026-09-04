void swap_pair(unsigned long int *pair_p)
/*@
requires
    take pairStart = each (integer j; 0 <= j && j < 2) {RW(array_shift(pair_p, j))};
ensures
    take pairEnd = each (integer j; 0 <= j && j < 2) {RW(array_shift(pair_p, j))};
    pairEnd[0] == pairStart[1];
    pairEnd[1] == pairStart[0];
@*/
{
    /*@ focus RW<unsigned long int>, 0; @*/
    unsigned long int tmp = pair_p[0];
    /*@ focus RW<unsigned long int>, 1; @*/
    /*@ instantiate 0; @*/
    /// originally: instantiate good<unsigned long int>, 0;
    pair_p[0] = pair_p[1];
    /*@ instantiate 1; @*/
    /// originally: instantiate good<unsigned long int>, 1;
    pair_p[1] = tmp;
}

int main(void)
/*@ trusted; @*/
{
  unsigned long int pair_p[2] = {1, 5};
  swap_pair(pair_p);
}
