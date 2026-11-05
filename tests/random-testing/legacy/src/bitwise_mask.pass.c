// Test for tristate (tnum) domain - bitwise operations with bit masking
// preconditions This test exercises bitwise AND, OR, XOR operations which are
// optimally tracked by the tristate number abstract domain.

int extract_low_nibble(int x)
/*@ requires (x & (-256i32)) == 0i32;
    ensures return == (x & 15i32);
@*/
{
  return x & 0x0F;
}

int set_low_bit(int x)
/*@ requires (x & (-128i32)) == 0i32;
    ensures return == (x | 1i32);
@*/
{
  return x | 1;
}

int toggle_bit(int x, int mask)
/*@ requires (x & (-256i32)) == 0i32;
             (mask & (-256i32)) == 0i32;
    ensures return == (x ^ mask);
@*/
{
  return x ^ mask;
}

int clear_high_bit(int x)
/*@ requires (x & (-256i32)) == 0i32;
    ensures return == (x & 127i32);
@*/
{
  return x & 0x7F;
}

int double_val(int x)
/*@ requires (x & (-128i32)) == 0i32;
    ensures return == x + x;
@*/
{
  return x + x;
}
