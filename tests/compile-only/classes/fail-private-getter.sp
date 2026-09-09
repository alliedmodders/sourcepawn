#include <shell>

class MyClass {
    int val;

    property int Prop {
        private get() {
            return this.val;
        }
        public set(int v) {
            this.val = v;
        }
    }
}

public void main() {
    MyClass a = new MyClass();
    printnum(a.Prop);
}
