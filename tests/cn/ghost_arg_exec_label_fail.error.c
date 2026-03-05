void f() /*@ requires cn_ghost i32 size; size < 10i32; @*/ {}

int main() {
    int h = 20;
    goto t;

t:
    f(/*@ h + 4i32 @*/);
    return 0;
}
