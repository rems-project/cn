void f() /*@ requires cn_ghost integer x; x < 10; @*/ {}

int main() {
    int i = 0;
    int d = 17;
    switch (i) {
        case 0:
            f(/*@ d + i @*/);
            break;
    }
}
