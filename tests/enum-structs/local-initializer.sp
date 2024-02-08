#include <shell>

enum struct Test {
        int i;
        float f;
        bool b;
        char str[32];
        float vec[3];
}

public main()
{
  Test test = {1, 1.0, false, "value1", {1.0, 2.0, 3.0}};

  printnum(test.i);
  printfloat(test.f);
  printnum(test.b);
  print(test.str);
  print("\n");
  printfloat(test.vec[0]);
  printfloat(test.vec[1]);
  printfloat(test.vec[2]);
}
