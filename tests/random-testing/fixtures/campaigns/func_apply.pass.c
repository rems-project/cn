#include <stdint.h>

/*@
    function [rec] (integer) count(integer n) {
        if (n == 0) {
            0
        } else {
            1 + count(n - 1)
        }
    }
@*/

uint8_t f(uint8_t x)
/*@
requires
    0 <= x; x <= MAXu8();
    x == count(x);
ensures
    return == count(x);
@*/
{
  return x;
}
