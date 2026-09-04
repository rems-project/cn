void half(int *q)
/*@
requires
    take X = each (integer i; 5 <= i && i < 10 ) { RW(array_shift(q, i)) };
ensures
    take X2 = each (integer i; 5 <= i && i < 10 ) { RW(array_shift(q, i)) };
@*/
{
}

void whole(int *q)
/*@
requires
    take X = each (integer i; 0 <= i && i < 10 ) { RW(array_shift(q, i)) };
ensures
    take X2 = each (integer i; 0 <= i && i < 10 ) { RW(array_shift(q, i)) };
@*/
{
}

int main()
{
    int a[10] = {0};
    half(a);
    whole(a);
}
