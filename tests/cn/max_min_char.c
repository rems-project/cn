// Under the default profile (hafnium), plain char is unsigned.
void check_cn_max_min_char()
{
    /*@ assert(0 == MINchar()); @*/
    /*@ assert(255 == MAXchar()); @*/
}

int main(void) {
    check_cn_max_min_char();
    return 0;
}
