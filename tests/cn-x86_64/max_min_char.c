void check_cn_max_min_char()
{
    /*@ assert(-128 == MINchar()); @*/
    /*@ assert(127 == MAXchar()); @*/
}

int main(void) {
    check_cn_max_min_char();
    return 0;
}
