native double add_double(double a, double b);
native void printdouble(double n);

double getsmallnumber() {
    return 1.5d;
}

double getbignumber() {
    return 1234567.89012345d;
}

public main() {
    printdouble(getsmallnumber());
    printdouble(getbignumber());
    printdouble(add_double(getbignumber(), getbignumber()));
}
