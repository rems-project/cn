int live_RW_footprint(char *p, char *q)
/*@
 requires
    take P = RW<int[11]>(array_shift<char>(p, -2));
    ptr_eq(q, array_shift<char>(p, 12));
ensures
    take P2 = RW<int[11]>(array_shift<char>(p, -2));
    P == P2;
    return == 12;
@*/
{
  // will fail without -- /*@ extract RW<int>, 7; @*/
  return q - p;
}

int main(void)
{
    int arr[11] = { 0 };
    char *p = (char*) arr;
    live_RW_footprint(p + 2, p + 14);
}
