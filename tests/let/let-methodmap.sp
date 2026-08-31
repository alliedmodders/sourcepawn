#include <shell>

methodmap MyObj __nullable__ {
    public MyObj(int val) {
        return view_as<MyObj>(val);
    }
    public void print() {
        printnum(view_as<int>(this));
    }
};

public void main() {
    let h = new MyObj(1337);
    h.print();
}
