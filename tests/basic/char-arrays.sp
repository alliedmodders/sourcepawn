#include <shell>

public main()
{
  char test[5];
  test[0] = 'a';
  test[1] = 'b';
  test[2] = 'c';
  test[3] = 'd';
  print(test);
  print("\n");
  printnum(asRef(test));
  print(test);
  print("\n");
}

int asRef(char[] test)
{
  test[1] = 'q';
  return test[0];
}
