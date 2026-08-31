#include <shell>

methodmap Foo {
    property int Bar {
    }
}

public void main() {
    Foo f = view_as<Foo>(0);
    int x = f.Bar;
}
