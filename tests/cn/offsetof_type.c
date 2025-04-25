struct s
{
    int x;
    int y;
};

int main()
{
    // C standard defines offsetof to have a size_t type, which we map to u64
    /*@ assert (offsetof(s, y) == 4u64); @*/
}