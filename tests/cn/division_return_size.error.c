/* Division can be done with Integers of different signs and sizes
   but think about the return type carefully. */

/* The test now behaves different from before since CN treats the
   division as uninterpreted. */

// fails because it should return a long
int different_size(int x, long y)
/*@ requires y != 0;
@*/
{
    return x / y;
}
