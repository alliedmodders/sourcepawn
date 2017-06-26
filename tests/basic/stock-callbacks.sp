#include <shell>

public main()
{
  invoke(1, callback);
}

stock do_print(const char[] text)
{
  print(text);
}

stock callback()
{
  do_print("hello!\n");
}
