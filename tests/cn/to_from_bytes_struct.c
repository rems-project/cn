/*CN_VIP*/ [[cerb::byte]] typedef unsigned char byte;
typedef struct s {
    byte a;
} s;
/*@
predicate (map<integer,byte>) Array_byte (pointer p, integer l)
{
  take pv = each(integer i; i >= 0 && i < l) {RW<byte>(array_shift<byte>(p,i))};
  return pv;
}
@*/

#if 0
static void transmute_to_blob(void *p)
/*@
  requires take xsi = Array_byte(p, sizeof<s>);
  ensures take xso = RW<s>p);
  @*/
{
  /*@ focus RW<byte>, 0; @*/
  /*@ from_bytes RW<s>(p); @*/
}
#else

static void untransmute_to_blob(void *p)
/*@
  requires take xsi = RW<s>(p);
  ensures take xso = Array_byte(p, sizeof<s>);
  @*/
{
  /*@ focus RW<byte>, 0; @*/
  /*@ to_bytes RW<s>(p); @*/
}
#endif
