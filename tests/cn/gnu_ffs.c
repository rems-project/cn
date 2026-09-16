int ffs(int x)
/*@ ensures (x == 0) ? (return == 0) : true;
            (x == 1) ? (return == 1) : true;
            (x == 2) ? (return == 2) : true;
            (x == 3) ? (return == 1) : true;
            (x == 8) ? (return == 4) : true; @*/
{
  return __builtin_ffs(x);
}

int main(void) {
  int r = ffs(1);
  return 0;
}
