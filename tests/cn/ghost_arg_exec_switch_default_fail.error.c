void f() /*@ requires cn_ghost i32 x; ensures x < 10i32; @*/ {}

int main() {
    int i = 0;
    int g = 19;
    switch (i) {
        default:
            f(/*@ g + i @*/);
    }
}
