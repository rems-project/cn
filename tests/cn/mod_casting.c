/* Integer promotions in modulo are subtle. The result of a modulo will be the larger type,
according to the following hierarchy `iN <: uN` and `uN <: i(2N)`  (and of course `iN <: i(2N)` and `uN <: u2N`).

Important: (1) signed integers must be non-negative to convert to unsigned (2) if one of the operands
is unsigned, the result will be unsigned, so any signed values must be non-negative. */

unsigned int mod (unsigned int x, int y)
/*@ requires y > 0;
     ensures let r = mod (abs(x), y);
             let result = (x < 0) ? (-r) : r;
             return == result; @*/
{
    return x % y;
}
