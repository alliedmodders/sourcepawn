#include <shell>

void f(int a[2][2] = {{1, 2}, {3, 4}})
{
  printnum(a[0][1]);
}

public main()
{
  f();
}
