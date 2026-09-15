int f(int x)
/*@ ensures return == ((0 <= x && x <= 30) ? 1 : 0); @*/
{
  switch (x) {
    case 0 ... 30:
      return 1;
    default:
      return 0;
  }
}

int main(void)
{
  int r = f(29);
}
