native void printnumptr(intptr a);
native void printnum64(int64 n);

public void main() {
    int x = 10;
    intptr p = 20;

    // Operator promotion ranking. These resolve via the C#-style "closest"
    // cross-check in FindBinaryOperator rather than an explicit ranking table:
    // the intptr<->int64 relationship is asymmetric (intptr->int64 is implicit,
    // int64->intptr is not), which makes the tie-break decisive.
    //
    // int OP intptr -> intptr (int is closer to intptr than to int64).
    printnumptr(x + p);
    printnumptr(p - x);
    printnumptr(x * p);

    // intptr OP int64 -> int64 (intptr promotes to int64).
    int64 big = 1000000000000;
    printnum64(p + big);
    printnum64(big - p);
}
