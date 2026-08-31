native void printdouble(double n);

public main() {
    double a = 3.14d;
    double b = 0.0d;
    if (a)
        printdouble(a);
    if (!a)
        printdouble(0.0d);
    if (b)
        printdouble(1.0d);
    if (!b)
        printdouble(0.0d);
}
