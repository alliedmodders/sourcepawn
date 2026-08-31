native void printdouble(double n);

double field_;

double take_double(double x) {
    return x;
}

double from_float_local(float f) {
    double d = f;
    return d;
}

public main() {
    float f = 1.5;

    // Local: float -> double.
    double local = f;
    printdouble(local);

    // Field: float -> double field.
    field_ = f;
    printdouble(field_);

    // Parameter: float -> double parameter.
    printdouble(take_double(f));

    // Return value: float -> double.
    printdouble(from_float_local(f));
}
