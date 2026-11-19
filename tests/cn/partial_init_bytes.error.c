int main()
{
    int x;
    int *p = &x;
    /*@ to_bytes W(p); @*/
    char *p_char = (char *)p;
    /*@ focus W<char>, 2; @*/
    p_char[2] = 0xff;
    /*@ from_bytes RW<int>(p); @*/
}
