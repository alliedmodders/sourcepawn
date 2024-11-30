#include <shell>

enum struct MyStruct
{
    int v;
    float f;
    char sz[8];
}

MyStruct CreateStruct()
{
    MyStruct st;
    st.v = 1;
    st.f = 1.0;
    st.sz = "Hello";
    return st;
}

void PrintStruct(MyStruct st)
{
    printnums(st.v);
    printfloat(st.f);
    print(st.sz);
    print("\n");
}

public void main() 
{
    print("First Struct\n");
    MyStruct st;
    st = CreateStruct();
    PrintStruct(st);
    st = CreateStruct();
    PrintStruct(st);
    st = CreateStruct();
    PrintStruct(st);
    print("New Struct Below\n");
    MyStruct st2;
    st2 = CreateStruct();
    PrintStruct(st2);
    st2 = CreateStruct();
    PrintStruct(st2);
    st2 = CreateStruct();
    PrintStruct(st2);
}
