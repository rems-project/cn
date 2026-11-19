void f() /*@ requires cn_ghost integer x; x < 10; @*/ {}

int main() {
    int h = 20;
    goto t;

t:
    f(/*@ h + 4 @*/);
    return 0;
}
