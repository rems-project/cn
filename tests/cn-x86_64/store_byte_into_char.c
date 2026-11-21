/* Storing a byte value 128..255 into a char is an implementation-defined
   conversion (C11 6.3.1.3p3); under --impl=x86_64 it reduces modulo 256,
   as in GCC. */

/*@ function (integer) char_of_byte (integer v) { v <= MAXchar() ? v : v - 256 } @*/

void store(char *p, int x)
/*@ requires take v = RW<char>(p); 0 <= x; x <= 255;
    ensures take w = RW<char>(p); w == char_of_byte(x); @*/
{
    *p = x;
}

int main(void) {
    char c = 0;
    store(&c, 200);
    /*@ assert(c == -56); @*/
    return 0;
}
