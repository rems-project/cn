void check_cn_max_min_consts()
{
    /*@ assert(255u8 == MAXu8()); @*/
    /*@ assert(127i8 == MAXi8()); @*/

    /*@ assert(0u8 == MINu8()); @*/
    /*@ assert(-128i8 == MINi8()); @*/

    /*@ assert(65535u16 == MAXu16()); @*/
    /*@ assert(32767i16 == MAXi16()); @*/

    /*@ assert(0u16 == MINu16()); @*/
    /*@ assert(-32768i16 == MINi16()); @*/

    /*@ assert(4294967295u32 == MAXu32()); @*/
    /*@ assert(4294967290u32 == MAXu32() - 5u32); @*/
    /*@ assert(2147483647i32 == MAXi32()); @*/

    /*@ assert(0u32 == MINu32()); @*/
    /*@ assert(-2147483648i32 == MINi32()); @*/

    /*@ assert(18446744073709551615u64 == MAXu64()); @*/
    /*@ assert(18446744073709551610u64 == MAXu64() - 5u64); @*/
    /*@ assert(9223372036854775807i64 == MAXi64()); @*/
    /*@ assert(9223372036854775800i64 == MAXi64() - 7i64); @*/

    /*@ assert(0u64 == MINu64()); @*/
    /*@ assert(-9223372036854775808i64 == MINi64()); @*/
    /*@ assert(-9223372036854775800i64 == MINi64() + 8i64); @*/
}

int main(void) {
    check_cn_max_min_consts();
    return 0;
}