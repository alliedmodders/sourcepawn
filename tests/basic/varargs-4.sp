#include <shell>

public main()
{
  int a = 1, b = 2, c = 3, d = 4, e = 5;
  do_1(a);
  do_2(a, b);
  do_3(a, b, c);
  do_4(a, b, c, d);
  do_5(a, b, c, d, e);
}

void do_1(int& a)
{
  printnums(a);
}

void do_2(int& a, int& b)
{
  printnums(a, b);
}

void do_3(int& a, int& b, int& c)
{
  printnums(a, b, c);
}

void do_4(int& a, int& b, int& c, int& d)
{
  printnums(a, b, c, d);
}

void do_5(int& a, int& b, int& c, int& d, int& e)
{
  printnums(a, b, c, d, e);
}

