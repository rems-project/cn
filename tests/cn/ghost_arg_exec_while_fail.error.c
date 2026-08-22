void f() /*@ requires cn_ghost integer x; x < 10; @*/ {}

int main() {
    int i = 0;
    while (i < 5) {
        int b = 15;
        f(/*@ b + i @*/);
        i++;
    }
}
