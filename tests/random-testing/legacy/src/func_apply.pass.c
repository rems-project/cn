#include <stdint.h>

/*@
    function [rec] (u8) count(u8 n) {
        if (n == 0u8) {
            0u8
        } else {
            1u8 + count(n - 1u8)
        }
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