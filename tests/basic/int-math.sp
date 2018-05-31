#include <shell>

public main()
{
  int a = 10;
  int b = 20;
  int c = 30;
  int d = -45;

  printnum(a | b);
  printnum(a ^ c);
  printnum(a & c);
  printnum(a - c);
  printnum(a * c);
  printnum(c / b);
  printnum(c / a);
  printnum(a << b);
  printnum(0x5e5e5e5e >> a);
  printnum(-1 >>> a);
  printnum(d >> 2);
  printnum(d >>> 2);
  printnum(c << 2);
  printnum(!d);
  printnum(!!d);
  printnum(-d);
  printnum(~d);
  printnum(-3 - d);
  printnum(a == 10);
  printnum(a == 11);
  printnum(a >= 11);
  printnum(a > 11);
  printnum(b > 90);
  printnum(b < 90);
  printnum(d <= -45);
  printnum(d != -45);
  printnum(d != -5);
}
