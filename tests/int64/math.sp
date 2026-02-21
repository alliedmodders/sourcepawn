#include <shell>

public main()
{
  int64 a = 100000000000;
  int64 b = 200000000000;
  int64 c = 300000000000;
  int64 d = -450000000000;
  int64 e = 0x5e5e5e5e5e5e5e5e;
  int64 f = -1;

  printnum64(a | b);
  printnum64(a ^ c);
  printnum64(a & c);
  printnum64(a - c);
  printnum64(a * c);
  printnum64(c / b);
  printnum64(c / a);
  printnum64(a << b);
  printnum64(e >> 10);
  printnum64(f >>> 10);
  printnum64(d >> 2);
  printnum64(d >>> 2);
  printnum64(c << 2);
  printnum64(-3 - d);
  printnum(a == 10);
  printnum(a == 11);
  printnum(d != -45);
  printnum(d != -5);
}
