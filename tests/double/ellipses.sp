native void printdouble(double n);

double g[4] = {1.5d, ...};

public main() {
    double a[4] = {1.5d, ...};
    printdouble(a[0]);
    printdouble(a[1]);
    printdouble(a[2]);
    printdouble(a[3]);

    double b[3] = {2, ...};
    printdouble(b[0]);
    printdouble(b[1]);
    printdouble(b[2]);

    printdouble(g[0]);
    printdouble(g[3]);
}
