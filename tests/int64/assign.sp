native void printnum64(int64 a);

void set_byref(int64& out, int64 other) {
    printnum64(out);
    printnum64(other);
    out = other;
}

public main() {
    int64 a = 10;
    int64 b = a;
    printnum64(a);
    printnum64(b);

    int64 c = 55;
    set_byref(a, c);
    printnum64(c);
}
