native void printdouble(double n);

void test_deref(double a) {
    printdouble(a);
    a = 50.0d;
}

void test_ref(double& a) {
    test_deref(a);
    printdouble(a);
}

public void main() {
    double a = 10.0d;
    test_ref(a);
    printdouble(a);
}
