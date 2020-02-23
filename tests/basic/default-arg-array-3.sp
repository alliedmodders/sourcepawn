#include <shell>

void f(int a[3] = {1, 2, 3})
{
  printnum(a[1]);
}

public main()
{
  f();
}
