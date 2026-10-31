/* Preserve the old u64 length and signed i32 index casts, including wrap.
   The C argument and owned elements retain their original int range. */
/*@
function (integer) unsigned_length(integer len) {
  mod(len, 18446744073709551616)
}

function (integer) signed_index(integer i) {
  let bits = mod(i, 4294967296);
  (bits < 2147483648) ? bits : bits - 4294967296
}
@*/

int range(int* arr, int len)
/*@ trusted;
    requires
        take A = each (integer i; 0 <= i && i < unsigned_length(len)) {
            Owned(array_shift<int>(arr, i))
        };
        each (integer i; 0 <= i && i < unsigned_length(len)) {
            A[i] == signed_index(i)
        };
    ensures
        take A2 = each (integer i; 0 <= i && i < unsigned_length(len)) {
            Owned(array_shift<int>(arr, i))
        };
        A == A2;
        return == 1;
@*/
{
    int i = 0;
    while (i < len) {
        if (arr[i] != i) {
            return 0;
        }

        i++;
    }

    return 1;
}
