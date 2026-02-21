native int64 add_int64(int64 a, int64 b);
native void printnum64(int64 n);

int64 getsmallnumber() {
    return 12345;
}

int64 getbignumber() {
    return 1234567812345678;
}

public main() {
    printnum64(getsmallnumber());
    printnum64(getbignumber());
    printnum64(add_int64(getbignumber(), getbignumber()));
}
