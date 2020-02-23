// warnings_are_errors: true
#include <shell>

void f(int a[1] = {1.0})
{
  printnum(a[0]);
}

public main()
{
  f();
}
