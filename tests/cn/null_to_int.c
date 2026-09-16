unsigned long long f(int *p)
/*@
requires
    ptr_eq(p, NULL);
ensures
    return == 0;
@*/
{
    return (unsigned long long)p;
}

int main()
{
    return f((int*)0);
}
