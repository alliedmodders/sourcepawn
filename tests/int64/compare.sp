native void print(const char[] text);
native void printnum(int a);

public void main() {
  int64 a = 10000000000;

  print(">=\n");
  printnum(a >= 9000000000);
  printnum(a >= 10000000000);
  printnum(a >= 11000000000);

  print(">\n");
  printnum(a > 9000000000);
  printnum(a > 10000000000);
  printnum(a > 11000000000);

  print("<\n");
  printnum(a < 9000000000);
  printnum(a < 10000000000);
  printnum(a < 11000000000);

  print("<=\n");
  printnum(a <= 9000000000);
  printnum(a <= 10000000000);
  printnum(a <= 11000000000);
}
