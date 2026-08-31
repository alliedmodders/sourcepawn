#include <shell>

class Counter {
    int value;

    Counter() {
        this.value = 42;
    }

    int GetValue() {
        return this.value;
    }
}

public void main() {
    Counter c = new Counter();
    printnum(c.GetValue());
}
