enum {
  ARRAY_SIZE = 63
};

int f(void *p)
/*@ requires take P = RW<char[ARRAY_SIZE]>(p);
    ensures take P2 = RW<char[ARRAY_SIZE]>(p); @*/
{
    return 0;
}

int main(void)
{
  char p[ARRAY_SIZE] = {0};
  int r = f(&p);
}
