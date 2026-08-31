native void printdouble(double n);

public main()
{
  double a = 1.5d;
  double b = 2.25d;
  double c = 3.75d;
  double d = -1.5d;

  printdouble(a + b);
  printdouble(a - b);
  printdouble(a * b);
  printdouble(a / b);
  printdouble(a + c);
  printdouble(d * 2.0d);
}
