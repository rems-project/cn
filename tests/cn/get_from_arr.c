
/* originally made by minimising a problematic case from memcpy.c */


char
get_from_arr (char *in_arr)
/*@ requires take IA = each (integer j; 0 <= j && j < 10)
  {RW<char>(in_arr + j)};
    ensures take IA2 = each (integer j; 0 <= j && j < 10)
  {RW<char>(in_arr + j)}; @*/
{
  char c;

  /*@ focus RW<char>, 4; @*/
  /*@ instantiate 4; @*/
  c = in_arr[4];

  return c;
}

int main(void)
/*@ trusted; @*/
{
  char *str = "hello";
  char c = get_from_arr(str);
}
