int main()
{
    int x = 0;
    int y = ~x;
    /*@ assert(y == -1); @*/
    return 0;
}
