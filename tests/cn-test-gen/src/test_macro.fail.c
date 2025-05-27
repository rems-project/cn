#ifdef __CN_TEST
static int x;

int always_fail()
/*@ require true; @*/
{
  return x;
}
#endif
