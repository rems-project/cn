void f() /*@ requires cn_ghost integer x; x < 10; @*/ {}

int main() {
    int a = 14;
    f(/*@ a + 55 @*/);
}
