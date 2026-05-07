void f() /*@ requires cn_ghost i32 x; x < 10i32; @*/ {}

int main() {
    int i = 0;
    int d = 17;
    switch (i) {
        case 0:
            f(/*@ d + i @*/);
            break;
    }
}
