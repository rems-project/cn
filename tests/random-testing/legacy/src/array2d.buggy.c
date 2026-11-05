#include <stdint.h>

/*@
predicate map<u32, u8> Array_u8(pointer p){
    take q = RW<uint8_t*>(p); // First dereference
    take V = each(u32 j; j < 10u32) {
        RW(array_shift<uint8_t>(q, j)) // Second dereference
    };
    return V;
}
@*/

void foo(uint8_t **arr)
/*@
  requires
    take ain = each(u32 i; i < 10u32) {
        Array_u8(array_shift(arr, i))
    };
  ensures
    take aout = each(u32 i; i < 10u32) {
        Array_u8(array_shift(arr, i))
    };
@*/
{}
