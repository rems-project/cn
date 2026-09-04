// Previously this was binding
// (x == 0 && y == 0 || x != 0) && y != 0;
// which made this example pass. Yikes!
void g1(int x, int y)
/*@
requires
    x == 0 && y == 0 || x != 0 && y != 0;
ensures
    true;
@*/
{
    if (y != 0) {
        /*@ assert (x != 0); @*/
    } else {
        /*@ assert (false); @*/
    }
}
