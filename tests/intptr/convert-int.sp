native void printnumptr(intptr a);

public void main() {
    intptr a = 50000000;
    int b = 10;
    printnumptr(a + b);
}
