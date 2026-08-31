#include <shell>

public main()
{
  intptr a = 100;

  print(">=\n");
  printnum(a >= 90);
  printnum(a >= 100);
  printnum(a >= 110);

  print(">\n");
  printnum(a > 90);
  printnum(a > 100);
  printnum(a > 110);

  print("<\n");
  printnum(a < 90);
  printnum(a < 100);
  printnum(a < 110);

  print("<=\n");
  printnum(a <= 90);
  printnum(a <= 100);
  printnum(a <= 110);
}
