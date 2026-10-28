#include <shell>

enum MyEnum {
    E_Zero = 0
};

stock any AnyFn(any x) {
    return x;
}

stock MyEnum EnumFn(int x) {
    return view_as<MyEnum>(x);
}

public void main() {
    any a = 2.5;

    printfloat(a * 2.0);
    printfloat(2.0 * a);
    printfloat(AnyFn(2.5) + 1.0);

    float w = 2.0;
    w *= a;
    printfloat(w);

    any neg = -2.5;
    printnum(neg < -3.0);
    if (neg < -3.0)
        printnum(1);
    else
        printnum(0);

    printnum(view_as<int16>(EnumFn(0x18000)));
    printnum(view_as<int8>(EnumFn(0x180)));
}
