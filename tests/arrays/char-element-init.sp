#include <shell>

char gFull[5] = {'a', 'b', 'c', 'd', 'e'};
char gPartial[5] = {'x', 'y'};

public main()
{
  printnum(gFull[0]);
  printnum(gFull[1]);
  printnum(gFull[2]);
  printnum(gFull[3]);
  printnum(gFull[4]);

  printnum(gPartial[0]);
  printnum(gPartial[1]);
  printnum(gPartial[2]);

  char local[4] = {'a', 'b', 'c', 'd'};
  printnum(local[0]);
  printnum(local[3]);
  local[1] = 'z';
  printnum(local[1]);
}
