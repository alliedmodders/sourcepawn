methodmap TestA {
    public void FuncA() {}
}

enum struct TestB {
    void TestB() {
        TestA.UndeclaredStaticFunc();
    }
}

int main() {}