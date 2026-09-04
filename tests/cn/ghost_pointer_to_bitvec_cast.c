
int
test_cast_loc_to_various (int *p)
/*@
requires
    let p_u64 = (integer)p;
    p_u64 <= MAXu64() - 3;
@*/
{
  return 1;
}

int main(void)
{
  int p[1] = {0};
  test_cast_loc_to_various(p);
}
