#include <shell>

public main()
{
  intptr a = 100;
  intptr b = 200;
  intptr c = 300;
  intptr d = 1 << 30;
  intptr divd = 450;
  intptr mod = 7;

  printnumptr(a | b);
  printnumptr(a ^ b);
  printnumptr(a & b);
  printnumptr(a - c);
  printnumptr(a * c);
  printnumptr(c / b);
  printnumptr(c / a);
  printnumptr(a << 4);
  printnumptr(d >> 10);
  printnumptr(c << 2);
  printnumptr(-3 - divd);
  printnumptr(divd / mod);
  printnumptr(divd % mod);
  printnumptr(-divd % mod);
  printnum(divd == 450);
  printnum(divd == 451);
  printnum(divd != d);
  printnum(divd != mod);
}
