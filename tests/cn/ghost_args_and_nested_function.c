int f(int x)  /*@ requires cn_ghost u64 y; true; @*/
{
    return x;
}

int main() {
    f(f(1 /*@ 2u64 @*/) /*@ 3u64 @*/);
    return 0;
}
