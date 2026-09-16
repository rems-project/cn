int live_owned_footprint(char *p, char *q)
/*@
 requires
    take P = RW<int[11]>(array_shift<char>(p, -2));
    ptr_eq(q, array_shift<char>(p, 12));
ensures
    take P2 = RW<int[11]>(array_shift<char>(p, -2));
    P == P2;
    return == 0;
@*/
{
  /*@ focus RW<int>, 7; @*/
  // NOTE: neither argument needs to be in the footprint of the RW
  // The bounds check for the allocation are done separately to the resource
  // lookup
  return q < p;
}

// Here, only one ownership is required to establish the that the allocation is
// live, but both are required to ensure that the bounds check succeeds
int live_owned_both(int *p, int *q)
/*@
 requires
    take P = RW(p);
    take Q = RW(q);
    (integer) p < (integer) q;
    ptr_eq(q, array_shift(p, 10));
ensures
    take P2 = RW(p);
    P == P2;
    take Q2 = RW(q);
    Q == Q2;
    return == 0;
@*/
{
  return p > q;
}

int live_owned_one(int *p, int *q)
/*@
 requires
    take P = RW(p);
    ptr_eq(q, array_shift(p, 10));
    let A = allocs[(alloc_id)p];
    (integer) p <= (integer) q;
    (integer) q <= A.base + A.size;
ensures
    take P2 = RW(p);
    P == P2;
    return == 1;
@*/
{
  return p <= q;
}

int live_alloc(int *p, int *q)
/*@
 requires
    !is_null(p);
    ptr_eq(q, array_shift(p, 10));
    take A = Alloc(p);
    A.base <= (integer) p;
    (integer) p <= (integer) q;
    (integer) q <= A.base + A.size;
ensures
    return == 0;
    take A2 = Alloc(p);
    A == A2;
@*/
{
  /*@ assert(allocs[(alloc_id)p] == A); @*/
  return p >= q;
}

int main(void)
{
    int arr[11] = { 0 };
    live_alloc(&arr[0], &arr[10]);
    /*@ focus RW<int>, 0; @*/
    /*@ focus RW<int>, 10; @*/
    live_owned_one(&arr[0], &arr[10]);
    live_owned_both(&arr[0], &arr[10]);
    char *p = (char*) arr;
    live_owned_footprint(p + 2, p + 14);
}
