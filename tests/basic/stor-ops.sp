#include <shell>

int sGlobal = 0;

public main()
{
  int local = donothing();
  local += donothing();
  printnum(local - donothing());
  printnum(donothing() - local);
  local = donothing();
  printnum(local);

  sGlobal = donothing();
  printnum(sGlobal);

  sGlobal = 99;
  printnums(sGlobal, sGlobal);
}
