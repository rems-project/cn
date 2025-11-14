#ifdef __CN_TEST
static int x;

int always_fail()
/*@ requires true; @*/
{
  return x;
}
#endif
