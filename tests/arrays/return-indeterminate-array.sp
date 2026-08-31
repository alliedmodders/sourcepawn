#include <shell>

char[] Workaround(val)
{
  char sNames[][] = {"Humans\n", "Zombies\n", "Egg\n"};
  return sNames[val];
}

public main()
{
  print(Workaround(1));
}
