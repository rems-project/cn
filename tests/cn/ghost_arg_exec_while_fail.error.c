void f() /*@ requires cn_ghost i32 size; size < 10i32; @*/ {}

int main() {
    int i = 0;
    while (i < 5) {
        int b = 15;
        f(/*@ b + i @*/);
        i++;
    }
}
