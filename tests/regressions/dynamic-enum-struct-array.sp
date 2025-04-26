#include <shell>

enum struct MyStruct {
    int a;
    int b;
}

public void main() {
    MyStruct msStatic[12];
    MyStruct[] msDynamic = new MyStruct[12];

    for (int i = 0; i < 12; i++) {
        printnums(i, msStatic[i].a, msDynamic[i].a);
        printnums(i, msStatic[i].b, msDynamic[i].b);
    }
}
