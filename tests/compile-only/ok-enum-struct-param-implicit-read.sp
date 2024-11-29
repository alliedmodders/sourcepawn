// warnings_are_errors: true

enum struct Foo
{
    int value;
}

void FunctionWithStructParam(Foo param)    // warning 204: symbol is assigned a value that is never used: "param"
{
    Foo bar;
    bar.value = 1;
    param = bar;
}

public main() {
    Foo foo;
    FunctionWithStructParam(foo);
}
