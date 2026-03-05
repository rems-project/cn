void f() /*@ requires cn_ghost i32 size; ensures size < 10i32; @*/ {}

int main() {
    int i = 0;
    int g = 19;
    switch (i) {
        default:
            f(/*@ g + i @*/);
    }
}
