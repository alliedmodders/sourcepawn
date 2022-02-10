#include <shell>

public main()
{
  float a = 10.3;
  float b = 20.5;
  float c = 30.2;
  float d = -45.7;

  printfloat(a - c);
  printfloat(a + c);
  printfloat(a * c);
  printfloat(c / a);
  printnum(a < b);
  printnum(a <= b);
  printnum(a < a);
  printnum(a <= a);
  printnum(b > a);
  printnum(b >= a);
  printnum(a > a);
  printnum(a >= a);
  printnum(b != d);
  printnum(b == d);
  printnum(!b);
  printfloat(FloatAbs(d));
  printfloat(FloatAbs(a));
  printfloat(-d);
  printfloat(c % a);
}
