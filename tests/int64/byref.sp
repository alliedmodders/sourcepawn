#include <shell>

void test_deref(int64 a) {
    printnum64(a);
    a = 5000;
}

void test_ref(int64& a) {
    test_deref(a);
    printnum64(a);
}

public void main() {
    int64 a = 1000;
    test_ref(a);
    printnum64(a);
}
