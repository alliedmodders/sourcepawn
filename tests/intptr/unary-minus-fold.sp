native void printnumptr(intptr a);

public main() {
    intptr v0 = -8;

    // Folded ternary yields an intptr-typed constant.
    printnumptr(-(1 <= 94 ? 8 : v0));

    // Other ways to mint an intptr-typed constant.
    printnumptr(-(view_as<intptr>(21)));
    printnumptr(-(intptr:33));

    // Runtime path still works.
    printnumptr(-(94 <= 1 ? 8 : v0));
}
