native void printdouble(double n);

double g[2] = {1.5, 2.5};
double g2[2] = {1, 2};

public main() {
    double a[2] = {1.5, 2.5};
    printdouble(a[0]);
    printdouble(a[1]);

    double b[2] = {1, 2};
    printdouble(b[0]);
    printdouble(b[1]);

    double c[3] = {1, 2.5, 3};
    printdouble(c[0]);
    printdouble(c[1]);
    printdouble(c[2]);

    printdouble(g[0]);
    printdouble(g[1]);

    printdouble(g2[0]);
    printdouble(g2[1]);

    double[] d = new double[2];
    d = {1.5, 2.5};
    printdouble(d[0]);
    printdouble(d[1]);

    d = {3, 4};
    printdouble(d[0]);
    printdouble(d[1]);
}
