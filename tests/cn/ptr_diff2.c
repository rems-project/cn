int* f(int *p)
/*@
requires
    has_alloc_id(p);
    let A = allocs[(alloc_id)p];
    A.base <= (integer) p - 4;
    0 <= (integer) p - 4;
    (integer) p <= A.base + A.size;
ensures
    ptr_eq(return, array_shift(p, -1));
@*/
{
  return p - 1;
}

int main(void)
{
    int arr[2] = { 0 };
    f(arr + 1);
}
