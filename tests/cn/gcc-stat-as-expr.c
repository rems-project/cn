int f(void) {
    int x = 
        ({int y = 5;
        y = y * 10;
        y;});
    return x;
}

int main(void) {
    int r = f();
    return 0;
}