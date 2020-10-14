#include <shell>

public main()
{
  printnum(dynamic_native(100));

  int[] dynarray = new int[13];
  dynarray[12] = 200;
  printnum(dynamic_native(dynarray[12]));
}
