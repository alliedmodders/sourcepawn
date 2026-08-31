// type: compiler-output
native void printnumptr(intptr a);

public void main() {
    int64 big = 123;
    intptr p = big;

    // Range check: out-of-range int64 constants used to initialize intptr.
    intptr q = 5000000000;
}
