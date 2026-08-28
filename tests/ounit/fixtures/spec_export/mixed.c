enum color {
  Red,
  Green,
  Blue
};

static int helper(void);
int helper(void) { return 0; }

int specified(int x)
/*@ requires x == 1i32; @*/
{
  return x;
}

enum color identity(enum color x) { return x; }

int main(void) { return 0; }
