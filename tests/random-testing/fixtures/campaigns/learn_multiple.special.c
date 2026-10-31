/* The old signed-bitvector condition x < x + 1000 excluded wrapped sums
   for positive x. Preserve that finite domain explicitly in integer mode. */
void p(int x, int y)
/*@
    requires
        0 < x;
        x <= MAXi32() - 1000;
        x + 1000 < y;
        x < x + 1000;
        y < MAXi32() - 5;
@*/
{}
