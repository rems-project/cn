struct s
{
    int x;
    int y;
};

int main()
{
    // (C standard defines offsetof to have a size_t type)
    /*@ assert (offsetof(s, y) == 4); @*/
}
