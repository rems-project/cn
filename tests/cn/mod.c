/* Modulo by zero is an undefined behavior.
   Must specify that the second operand is not equal to zero */

int mod (int x, int y)
/*@ requires y != 0;
    ensures return == x % y; @*/
{
    return x % y;
}
