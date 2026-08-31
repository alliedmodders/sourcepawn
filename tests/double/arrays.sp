native void printdouble(double n);

double blah[5] = {
    1.1d,
    2.2d,
    3.3d,
    4.4d,
    5.5d
};

public void main() {
    printdouble(blah[0]);
    printdouble(blah[1]);
    printdouble(blah[2]);
    blah[2] = 9.9d;
    printdouble(blah[2]);
    printdouble(blah[3]);
    printdouble(blah[4]);
}
