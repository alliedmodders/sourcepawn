enum struct MyStruct
{
    int i;

    void FuncA()
    {
        FuncB(this); // error 035: argument type mismatch (argument 1)
    }
}

void FuncB(any[] arr) { }

public main() {
}
