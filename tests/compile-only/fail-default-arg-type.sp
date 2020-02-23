// warnings_are_errors: true
#include <shell>

void f(int a = 1.0)
{
  printnum(a);
}

public main()
{
  f();
}
