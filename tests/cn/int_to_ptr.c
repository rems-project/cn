void* cast(unsigned long long addr)
/*@
ensures
    addr == 0 && is_null(return) || addr != 0 && has_alloc_id(return) && (integer) return == addr;
@*/
{
    return (void*)addr;
}

int main()
{
    int x = 0;
    void* p = cast((unsigned long long)&x);
    /*@ assert ((integer) &x == 0 || has_alloc_id(p) && (integer) p == (integer) &x); @*/
}
