native void printnum64(int64 a);

public main() {
    int64 a = 20;
    int64 b = ~a;
    int64 c = -a;
    printnum64(b);
    printnum64(c);
}
