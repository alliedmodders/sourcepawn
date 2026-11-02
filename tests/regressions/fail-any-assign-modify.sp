// type: compiler-output

enum struct Holder {
    any v;
}

stock void ScaleRef(any& x) {
    x *= 2.0;
}

public void main() {
    any a = 1.5;
    a *= 2.0;
    a += 1.0;
    a -= 0.5;
    a /= 0.5;

    float f = 2.0;
    a *= f;

    // The right-hand side is `any`; the operation still promotes it to float.
    float w = 2.0;
    w *= a;

    int64 big = 1;
    a += big;

    any arr[2];
    arr[1] *= 2.0;

    Holder h;
    h.v *= 2.0;

    ScaleRef(a);
}
