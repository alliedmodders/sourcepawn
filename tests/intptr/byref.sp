#include <shell>

void test_deref(intptr a) {
    printnumptr(a);
    a = 5000;
}

void test_ref(intptr& a) {
    test_deref(a);
    printnumptr(a);
}

public void main() {
    intptr a = 1000;
    test_ref(a);
    printnumptr(a);
}
