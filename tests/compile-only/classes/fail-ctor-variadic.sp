#include <shell>

class MyClass {
    int val;

    MyClass(int a, ...) {
        this.val = a;
    }
}

public void main() {
    MyClass a = new MyClass(5, 6, 7);
}
