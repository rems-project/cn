void f() /*@ requires cn_ghost i32 size; size < 10i32; @*/ {}

int main() {
    int a = 14;
    f(/*@ a + 55i32 @*/);
}
