/* Under the default profile plain char is unsigned, so a byte value
   128..255 stored into a char reads back unchanged. Fulminate must compile
   the generated C with the same char (-funsigned-char): with the signed
   char of an x86-64 host, the postcondition would fail at run time. */
void store(char *p, int x)
/*@ requires take v = RW<char>(p); 0 <= x; x <= 255;
    ensures take w = RW<char>(p); w == x; @*/
{
    *p = x;
}

int main(void) {
    char c = 0;
    store(&c, 200);
    /*@ assert(c == 200); @*/
    return 0;
}
