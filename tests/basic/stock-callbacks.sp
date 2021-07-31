#include <shell>

public main()
{
  invoke(callback, 1);
}

stock do_print(const char[] text)
{
  print(text);
}

stock callback()
{
  do_print("hello!\n");
}
