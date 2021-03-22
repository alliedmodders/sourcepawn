methodmap TestA {
    public void FuncA() {}
}

enum struct TestB {
	int Var;
    void TestB() {
        TestA.UndeclaredStaticFunc();
    }
}

int main() {}