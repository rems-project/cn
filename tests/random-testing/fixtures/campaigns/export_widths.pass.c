/* C storage stays finite while specification arithmetic remains mathematical. */
signed char signed_echo(signed char x)
/*@ ensures return == x; @*/
{
  return x;
}

unsigned long long unsigned_echo(unsigned long long x)
/*@ ensures return == x; x + 1 > x; @*/
{
  return x;
}

_Bool boolean_echo(_Bool x)
/*@ ensures return == x; x == 0 || x == 1; @*/
{
  return x;
}

signed char signed_edges(signed char x)
/*@ requires x == -128 || x == -1;
    ensures return == x; return < 0; @*/
{
  return x;
}

unsigned long long unsigned_max(unsigned long long x)
/*@ requires x == MAXu64();
    ensures return == 18446744073709551615;
            x + 1 == 18446744073709551616; @*/
{
  return x;
}

_Bool owned_boolean(_Bool *p)
/*@ requires take before = RW<_Bool>(p);
    ensures take after = RW<_Bool>(p);
            after == before;
            return == before;
            after == 0 || after == 1; @*/
{
  return *p;
}

/*@
predicate (integer) WidthCell(pointer p) {
  take value = RW<int>(p);
  return value;
}
@*/

int owned_increment(int *p)
/*@ requires take before = WidthCell(p);
             before < MAXi32();
    ensures take after = RW<int>(p);
            after == before + 1;
            return == after; @*/
{
  return ++*p;
}

struct width_point {
  int x;
  unsigned char tag;
};

struct width_block {
  struct width_point point;
  unsigned char grid[2][2];
};

struct width_nested {
  struct width_point first;
  struct width_point second;
};

struct width_unused {
  unsigned char bytes[16];
  _Bool flags[2];
};

void unused_aggregate(struct width_unused *p)
/*@ requires take before = RW<struct width_unused>(p);
    ensures take after = RW<struct width_unused>(p); @*/
{
  (void)p;
}

/*@
function (integer) logical_update(struct width_point p) {
  let wide = {x: p.x + 2147483648, ..p};
  wide.x - p.x
}

predicate (struct width_block) WidthBlock(pointer p) {
  take block = RW<struct width_block>(p);
  return block;
}
@*/

struct width_nested nested_echo(struct width_nested value)
/*@ ensures return == value;
            logical_update(value.first) == 2147483648;
            return.second.x == value.second.x; @*/
{
  return value;
}

void owned_nested(struct width_nested *p)
/*@ requires take before = RW<struct width_nested>(p);
    ensures take after = RW<struct width_nested>(p);
            after == before;
            logical_update(after.first) == 2147483648; @*/
{
  (void)p;
}

struct width_block aggregate_echo(struct width_block value)
/*@ ensures return == value;
            logical_update(value.point) == 2147483648;
            return.grid[1][1] == value.grid[1][1]; @*/
{
  return value;
}

void owned_aggregate(struct width_block *p)
/*@ requires take before = WidthBlock(p);
    ensures take after = WidthBlock(p);
            after == before;
            logical_update(after.point) == 2147483648;
            after.grid[1][0] == before.grid[1][0]; @*/
{
  (void)p;
}

int fixed_array(int (*p)[2])
/*@ requires take before = RW<int[2]>(p);
    ensures take after = RW<int[2]>(p);
            after == before;
            return == before[1]; @*/
{
  return (*p)[1];
}

void quantified_array(int *p, unsigned char n)
/*@ requires n <= 2;
             take before = each (integer i; 0 <= i && i < n) { RW<int>(p + i) };
    ensures take after = each (integer i; 0 <= i && i < n) { RW<int>(p + i) };
            after == before; @*/
{
  (void)p;
  (void)n;
}

void empty_array(int *p)
/*@ requires take before = each (integer i; 0 <= i && i < 0) { RW<int>(p + i) };
    ensures take after = each (integer i; 0 <= i && i < 0) { RW<int>(p + i) };
            after == before; @*/
{
  (void)p;
}

void uninitialized_output(int *p)
/*@ requires take before = RW<int>(p);
    ensures take unused = W<int>(p); @*/
{
  (void)p;
}
