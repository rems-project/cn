
unsigned int
add_self (unsigned int x)
/*@ requires x + x < MAXu32(); 
    ensures return == x + x; @*/
{
  return x + x;
}

unsigned int
add_self_twice (unsigned int x)
/*@ requires x * 4 < MAXu32(); 
    ensures return == x * 4; @*/
{
  unsigned int y = add_self(x);
  return y + y;
}

int main(void)
/*@ trusted; @*/
{
  unsigned int r = add_self_twice(5);
}
