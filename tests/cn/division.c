/* Division by zero is an undefined behavior.
   Must specify that the second operand is not equal to zero */

int division (int x, int y)
/*@ requires y != 0;
             (y == (-1)) implies (x > MINi32());
             let q = abs(x)/y;
    ensures let r = (x < 0) ? (-q) : q;
            return == r; @*/
{
    return x / y;
}
