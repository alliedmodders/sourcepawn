#include <shell>

void f(int[] a = {1, 2, 3})
{
  printnum(a[1]);
}

public main()
{
  f();
}
