#include <shell>

void lots_of_stuff(int a = 5, int& b = 10, int c[3] = {1, 2, 3}, int d = 2)
{
  printnum(a);
  printnum(b);
  printnum(c[0]);
  printnum(c[1]);
  printnum(c[2]);
  printnum(d);
}

public main()
{
  lots_of_stuff();
  lots_of_stuff(2, _, _);
  lots_of_stuff(2, _, _, 9);
}
