/* Modulo can be done with Integers of different signs and sizes
   but think about the return type carefully. */

// fails because it should return a long
int different_size(int x, long y)
/*@ requires y != 0;
    ensures return == rem (x, y); @*/
{
    return x % y;
}
