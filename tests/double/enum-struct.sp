native void printdouble(double n);

enum struct Clams {
    double a;
    double b;
}

public void main() {
    Clams clam;
    clam.a = 1.5d;
    clam.b = 2.5d;

    printdouble(clam.a);
    printdouble(clam.b);
}
