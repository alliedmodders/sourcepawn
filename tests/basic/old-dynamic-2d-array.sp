#include <shell>

public main()
{
  new x = 2;
  new y = 3;
  new arr[x][y];
  arr[0][0] = 10;
  arr[0][1] = 20;
  arr[1][0] = 30;
  printnum(arr[0][0]);
  printnum(arr[0][1]);
  printnum(arr[1][0]);
}
