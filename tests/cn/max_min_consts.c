void check_cn_max_min_consts()
{
    /*@ assert(255 == MAXu8()); @*/
    /*@ assert(127 == MAXi8()); @*/

    /*@ assert(0 == MINu8()); @*/
    /*@ assert(-128 == MINi8()); @*/

    /*@ assert(65535 == MAXu16()); @*/
    /*@ assert(32767 == MAXi16()); @*/

    /*@ assert(0 == MINu16()); @*/
    /*@ assert(-32768 == MINi16()); @*/

    /*@ assert(4294967295 == MAXu32()); @*/
    /*@ assert(4294967290 == MAXu32() - 5); @*/
    /*@ assert(2147483647 == MAXi32()); @*/

    /*@ assert(0 == MINu32()); @*/
    /*@ assert(-2147483648 == MINi32()); @*/

    /*@ assert(18446744073709551615 == MAXu64()); @*/
    /*@ assert(18446744073709551610 == MAXu64() - 5); @*/
    /*@ assert(9223372036854775807 == MAXi64()); @*/
    /*@ assert(9223372036854775800 == MAXi64() - 7); @*/

    /*@ assert(0 == MINu64()); @*/
    /*@ assert(-9223372036854775808 == MINi64()); @*/
    /*@ assert(-9223372036854775800 == MINi64() + 8); @*/
}

int main(void) {
    check_cn_max_min_consts();
    return 0;
}
