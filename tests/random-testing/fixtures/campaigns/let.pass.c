#include <stdint.h>

/*@
    function [rec] (integer) count(integer n) {
        let res = (n == 0) ? 0 : 1 + count(n - 1);
        res
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
