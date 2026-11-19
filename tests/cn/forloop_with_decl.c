int for_with_decl()
{
  int acc = 0;
  for(int i = 0; i < 10; i++)
  /*@ inv 0 <= i; i <= 10;
          acc <= 10; @*/
  {
    acc = i;
  };
  return acc;
}

int main(void)
/*@ trusted; @*/
{
  int r = for_with_decl();
}
