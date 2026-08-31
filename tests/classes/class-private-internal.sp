#include <shell>

class MyClass {
    int val;

    private void PrivateMethod() {
        this.val = 10;
        printnum(this.val);
    }

    property int PrivProp {
        public get() {
            return this.val;
        }
        private set(int v) {
            this.val = v;
        }
    }

    public void Test() {
        this.PrivateMethod();
        this.PrivProp = 20;
        printnum(this.PrivProp);
    }
}

public void main() {
    MyClass a = new MyClass();
    a.Test();
}
