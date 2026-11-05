#if defined(__CN_TEST) || defined(__AUSTEN_TEST)
static int x;

int always_fail()
/*@ requires true; @*/
{
  return x;
}
#endif
