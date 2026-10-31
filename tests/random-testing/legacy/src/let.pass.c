#include <stdint.h>

/*@
    function [rec] (u8) count(u8 n) {
        let res = (n == 0u8) ? 0u8 : 1u8 + count(n - 1u8);
        res
    }
@*/

uint8_t f(uint8_t x)
/*@
requires
    x == count(x);
ensures
    return == count(x);
@*/
{
  return x;
}
