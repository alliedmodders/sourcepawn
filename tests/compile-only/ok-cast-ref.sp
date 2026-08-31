enum MyEnum {
    MyValue = 0
};

public void test(int& refVal) {
    MyEnum val = view_as<MyEnum>(refVal);
}
