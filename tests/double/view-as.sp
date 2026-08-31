native void printdouble(double n);
native void printnum64(int64 n);

public main() {
    int64 bits = 0x4048f5c28f5c28f6;  // 3.14 as IEEE 754 bits
    double d = view_as<double>(bits);
    printdouble(d);
    int64 back = view_as<int64>(d);
    printnum64(back);
}
