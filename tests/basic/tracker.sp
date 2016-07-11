#include <shell>

String:blah()
{
  char str[] = "hello\n";
  return str;
}

String:other()
{
  char str[] = "what is this\n";
  return str;
}

public main()
{
  int a = 5;

  print(a ? blah() : other());
}
