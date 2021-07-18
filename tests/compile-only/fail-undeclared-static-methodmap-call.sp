methodmap TestA {
    public void FuncA() {}
}

enum struct TestB {
	int Var;
    void TestB() {
        TestA.UndeclaredStaticFunc();
    }
}

public int main() {}
