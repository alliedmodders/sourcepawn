#include <shell>

enum struct Test {
        int i;
        float f;
        bool b;
        char str[32];
        float vec[3];
}

Test g_test[] = {
        {1, 1.0, false, "value1", {1.0, 2.0, 3.0}},
        {2, 2.0, true, "value2", {4.0, 5.0, 6.0}},
};

public main()
{
  for (int i = 0; i < sizeof(g_test); i++) {
    printnum(g_test[i].i);
    printfloat(g_test[i].f);
    printnum(g_test[i].b);
    print(g_test[i].str);
    print("\n");
    printfloat(g_test[i].vec[0]);
    printfloat(g_test[i].vec[1]);
    printfloat(g_test[i].vec[2]);
  }
}
