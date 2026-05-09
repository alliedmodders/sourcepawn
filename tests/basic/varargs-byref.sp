#include <shell>

void pass_int64_byref(int64& y) {
    printnums64(y);
}

public void main() {
    int64 a = 0x55555555555;
    pass_int64_byref(a);
}
