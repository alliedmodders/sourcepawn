native void printnumptr(intptr a);

void set_byref(intptr& out, intptr other) {
    printnumptr(out);
    printnumptr(other);
    out = other;
}

public main() {
    intptr a = 10;
    intptr b = a;
    printnumptr(a);
    printnumptr(b);

    intptr c = 55;
    set_byref(a, c);
    printnumptr(c);
    printnumptr(a);
}
