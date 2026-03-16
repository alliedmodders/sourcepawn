native void printnums64(...);

int64 crab() {
    return 0xeeeeeeeeee;
}

public void main() {
    int64 a = 100;
    int64 b = 0x55555555555;
    printnums64(a, b, crab());
}
