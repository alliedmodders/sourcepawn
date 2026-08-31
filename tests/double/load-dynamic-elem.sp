native void printdouble(double n);

double blah[2] = {
    1.1d,
    2.2d
};

public void main() {
    load_dynamic_d(blah, 1);
    double val = 9.9d;
    store_dynamic_d(blah, 0, val);
    printdouble(blah[0]);
    printdouble(blah[1]);
}

void load_dynamic_d(double[] arr, int index) {
    printdouble(arr[index]);
}

void store_dynamic_d(double[] arr, int index, double value) {
    arr[index] = value;
}
