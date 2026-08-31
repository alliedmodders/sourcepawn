native void printdouble(double n);

public main() {
    double a = 10.0d;
    double b = a;
    printdouble(a);
    printdouble(b);

    double c = 5.5d;
    b = c;
    printdouble(b);
    printdouble(c);
}
