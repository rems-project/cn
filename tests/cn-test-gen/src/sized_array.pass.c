struct foo {
  // Type `unsigned char` is a regression test, do not change
  unsigned char bar[16];
};

void test_gen_const_array(struct foo *c)
/*@
requires take Client_in  = Owned<struct foo>(c);
ensures  take Client_out = Owned<struct foo>(c);
@*/
{
  return;
}
