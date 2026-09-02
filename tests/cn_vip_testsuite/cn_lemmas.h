[[cerb::byte]] typedef unsigned char byte;

/*@
function [rec] (boolean) byte_array_init(map<integer, byte> arr1, map<integer, byte> arr2, integer end) {
    let end1 = end - 1;
    let b1 = arr1[end1];
    let b2 = arr2[end1];
    end == 0 || is_some(b1) && is_some(b1) && byte_array_bits_eq(arr1, arr2, end1)
}

function [rec] (boolean) byte_array_bits_eq(map<integer, byte> arr1, map<integer, byte> arr2, integer end) {
    let end1 = end - 1;
    let b1 = arr1[end1];
    let b2 = arr2[end1];
    end == 0 ||
        ((integer) get_opt(b1)) == ((integer) get_opt(b2)) && byte_array_bits_eq(arr1, arr2, end1)
}
@*/

/*@
lemma byte_arrays_equal(pointer x, pointer y, integer n)

requires
    n >= 0;
    take X = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(x, i)) };
    take Y = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(y, i)) };
    each (integer i; 0 <= i && i < n) { X[i] == Y[i] };

ensures
    take XR = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(x, i)) };
    take YR = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(y, i)) };
    X == XR; Y == YR;
    XR == YR;
@*/

#include <stddef.h>

int _memcmp(byte *dest, byte *src, size_t n);
/*@ spec _memcmp(pointer dest, pointer src, integer n);

requires
    n >= 0;
    (integer) src + n <= (integer) dest || (integer) dest + n <= (integer) src;
    take Src = each (integer i; 0 <= i && i < n ) { RW(array_shift(src, i)) };
    take Dest = each (integer i; 0 <= i && i < n ) { RW(array_shift(dest, i)) };
    byte_array_init(Src, Dest, n);

ensures
    take SrcR = each (integer i; 0 <= i && i < n ) { RW(array_shift(src, i)) };
    take DestR = each (integer i; 0 <= i && i < n ) { RW(array_shift(dest, i)) };
    Src == SrcR; Dest == DestR;
    let bits_eq = byte_array_bits_eq(Src, Dest, n);
    (return == 0 implies bits_eq) && (return != 0 implies !bits_eq);
@*/

/*@
lemma assert_equal(integer x, integer y)
requires
    true;
ensures
    x == y;
@*/

/*@
lemma byte_array_init_8(pointer dest, pointer src, integer n)
requires
    n == sizeof<int*>;
    take Src = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(src, i)) };
    take Dest = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(dest, i)) };
ensures
    take SrcR = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(src, i)) };
    take DestR = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(dest, i)) };
    Src == SrcR; Dest == DestR;
    let all_init = byte_array_init(Src, Dest, n);
    let each_init = each (integer i: 0,7; is_some(Src[i]) && is_some(Dest[i]));
    (all_init implies each_init) && (each_init implies all_init);

lemma byte_array_bits_eq_8(pointer dest, pointer src, integer n)
requires
    n == sizeof<int*>;
    take Src = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(src, i)) };
    take Dest = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(dest, i)) };
ensures
    take SrcR = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(src, i)) };
    take DestR = each (integer i; 0 <= i && i < n ) { RW(array_shift<byte>(dest, i)) };
    Src == SrcR; Dest == DestR;
    let arr_eq = byte_array_bits_eq(Src, Dest, n);
    let each_eq = each (integer i: 0,7; (integer) get_opt(Src[i]) == (integer) get_opt(Dest[i]) );
    (arr_eq implies each_eq) && (each_eq implies arr_eq);
@*/
