#include <shell>

void CheckInts()
{
  int a = 5;
  int b = 7;
  int c = 9;

  printnum(a < b < c);
  printnum(a < b <= b);
  printnum(a < b < b);
  printnum(b < b < c);
  printnum(b <= b < c);
  printnum(c < a < b);

  printnum(c > b > a);
  printnum(c > b >= b);
  printnum(c > b > b);
  printnum(b > b > a);
  printnum(b >= b > a);
  printnum(a > c > b);
}

void CheckFloats()
{
  float a = 5.0;
  float b = 7.0;
  float c = 9.0;

  printnum(a < b < c);
  printnum(a < b <= b);
  printnum(a < b < b);
  printnum(b < b < c);
  printnum(b <= b < c);
  printnum(c < a < b);

  printnum(c > b > a);
  printnum(c > b >= b);
  printnum(c > b > b);
  printnum(b > b > a);
  printnum(b >= b > a);
  printnum(a > c > b);
}

void CheckConsts()
{
#define a 5
#define b 7
#define c 9

  printnum(a < b < c);
  printnum(a < b <= b);
  printnum(a < b < b);
  printnum(b < b < c);
  printnum(b <= b < c);
  printnum(c < a < b);

  printnum(c > b > a);
  printnum(c > b >= b);
  printnum(c > b > b);
  printnum(b > b > a);
  printnum(b >= b > a);
  printnum(a > c > b);

#undef c
#undef b
#undef a
}

public main()
{
  CheckInts();
  CheckFloats();
  CheckConsts();
}
