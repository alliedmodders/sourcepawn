#include <shell>

public main()
{
  int a = 10;
  int b = 20;
  bool c = true;
  printnum(c ? a : b);
  printnum(!c ? a : b);
  printnum(c ? 10 : 20);
  printnum(!c ? 10 : 20);
}
