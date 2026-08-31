native void print(const char[] text);
native void printnum(int a);
native void printdouble(double n);

public void main() {
  double a = 3.14d;

  print(">=\n");
  printnum(a >= 3.0d);
  printnum(a >= 3.14d);
  printnum(a >= 4.0d);

  print(">\n");
  printnum(a > 3.0d);
  printnum(a > 3.14d);
  printnum(a > 4.0d);

  print("<\n");
  printnum(a < 3.0d);
  printnum(a < 3.14d);
  printnum(a < 4.0d);

  print("<=\n");
  printnum(a <= 3.0d);
  printnum(a <= 3.14d);
  printnum(a <= 4.0d);

  print("==\n");
  printnum(a == 3.14d);
  printnum(a == 3.0d);

  print("!=\n");
  printnum(a != 3.14d);
  printnum(a != 3.0d);
}
