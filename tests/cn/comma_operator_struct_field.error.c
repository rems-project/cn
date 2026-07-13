struct {
  int a;
} b;
void f() /*@ accesses b; @*/ { (0, b).a; }
int g(void) /*@ accesses b; ensures return==b.a; @*/ { return (0, b).a; }
