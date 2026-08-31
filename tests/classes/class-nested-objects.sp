#include <shell>

class Inner {
    int value;

    void SetValue(int v) {
        this.value = v;
    }

    void PrintValue() {
        printnum(this.value);
    }
}

class Outer {
    Inner inner;

    Inner GetInner() {
        return this.inner;
    }

    void SetInner(Inner i) {
        this.inner = i;
    }
}

public void main() {
    Outer outer = new Outer();
    Inner shared = new Inner();
    shared.SetValue(42);

    outer.SetInner(shared);
    outer.GetInner().PrintValue();
}
