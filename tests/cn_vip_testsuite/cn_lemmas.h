[[cerb::byte]] typedef unsigned char byte;

/*@
function [rec] (boolean) array_bits_eq(map<u64, byte> arr1, map<u64, byte> arr2, u64 end) {
    let end1 = end - 1u64;
    let b1 = arr1[end1];
    let b2 = arr2[end1];
    end == 0u64 ||
        is_some(b1) && is_some(b1) &&
        ((u8) get_opt(b1)) == ((u8) get_opt(b2)) && array_bits_eq(arr1, arr2, end1)
}
@*/

/*@
lemma byte_arrays_equal(pointer x, pointer y, u64 n)

requires
    take X = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift<byte>(x, i)) };
    take Y = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift<byte>(y, i)) };
    each (u64 i; 0u64 <= i && i < n) { X[i] == Y[i] };

ensures
    take XR = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift<byte>(x, i)) };
    take YR = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift<byte>(y, i)) };
    X == XR; Y == YR;
    XR == YR;
@*/

#include <stddef.h>

int _memcmp(byte *dest, byte *src, size_t n);
/*@ spec _memcmp(pointer dest, pointer src, u64 n);

requires
    (u64) src + n <= (u64) dest || (u64) dest + n <= (u64) src;
    (u64) src <= (u64) src + n;
    (u64) dest <= (u64) dest + n;
    take Src = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift(src, i)) };
    take Dest = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift(dest, i)) };

ensures
    take SrcR = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift(src, i)) };
    take DestR = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift(dest, i)) };
    Src == SrcR; Dest == DestR;
    let bits_eq = array_bits_eq(Src, Dest, n);
    (return == 0i32 implies bits_eq) && (return != 0i32 implies !bits_eq);
@*/

/*@
lemma assert_equal(u64 x, u64 y)
requires
    true;
ensures
    x == y;
@*/

/*@
lemma array_bits_eq_8(pointer dest, pointer src, u64 n)
requires
    n == sizeof<int*>;
    take Src = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift<byte>(src, i)) };
    take Dest = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift<byte>(dest, i)) };
ensures
    take SrcR = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift<byte>(src, i)) };
    take DestR = each (u64 i; 0u64 <= i && i < n ) { RW(array_shift<byte>(dest, i)) };
    Src == SrcR; Dest == DestR;
    let arr_eq = array_bits_eq(Src, Dest, n);
    let each_eq = each (u64 i: 0,7;
        is_some(Src[i]) && is_some(Dest[i]) &&
        (u8) get_opt(Src[i]) == (u8) get_opt(Dest[i]) );
    (arr_eq implies each_eq) && (each_eq implies arr_eq);
@*/
