void f1 (int *p)
/*@
requires
    take B = W(p);
ensures
    take B2 = W(p);
@*/
{
  unsigned long long p_int = (unsigned long long) p;
  int* q = __cerbvar_copy_alloc_id(p_int + 0ULL, p);
  /*@ assert (ptr_eq(p, q)); @*/
}

void f2 (int *p)
/*@
requires
    take A = Alloc(p);
    A.base <= (integer) p;
    (integer) p <= (integer) p + sizeof<int>;
    (integer)p + sizeof<int> <= A.base + A.size;
    has_alloc_id(p);
ensures
    take A2 = Alloc(p);
    A == A2;
@*/
{
  unsigned long long p_int = (unsigned long long) p;
  int* q = __cerbvar_copy_alloc_id(p_int + 0ULL, p);
  /*@ assert (ptr_eq(p, q)); @*/
}

int main(void)
{
  int p[1] = {1};
  /*@ focus RW<int>, 0; @*/
  f1(p);
  f2(p);
}
