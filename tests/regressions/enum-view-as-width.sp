#include <shell>

enum MyEnum {
    E_Zero = 0
};

stock MyEnum EnumFn(int x) {
    return view_as<MyEnum>(x);
}

public void main() {
    printnum(view_as<int16>(EnumFn(0x18000)));
    printnum(view_as<int8>(EnumFn(0x180)));
}
