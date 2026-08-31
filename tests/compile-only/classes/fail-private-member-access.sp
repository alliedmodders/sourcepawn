#include <shell>

class MyClass {
    int val;

    private void PrivateMethod() {
        this.val = 1;
    }

    property int Prop {
        public get() {
            return this.val;
        }
        private set(int v) {
            this.val = v;
        }
    }
}

public void main() {
    MyClass a = new MyClass();

    a.PrivateMethod();
    a.Prop = 10;
}
