native void printdouble(double n);

public main() {
    printdouble(5.0d % 2.0d);
    printdouble(7.5d % 2.5d);
    printdouble(10.0d % 3.0d);
    printdouble(-5.0d % 2.0d);
    printdouble(5.0d % -2.0d);
}
