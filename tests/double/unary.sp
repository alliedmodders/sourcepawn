native void printdouble(double n);

public main() {
    double a = 2.5d;
    double b = -a;
    printdouble(b);
    printdouble(-(-a));
}
