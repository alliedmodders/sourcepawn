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

  print("branch\n");
  if (a > 3.0d) printnum(1); else printnum(2);
  if (a > 3.14d) printnum(3); else printnum(4);
  if (a >= 3.14d) printnum(5); else printnum(6);
  if (a < 4.0d) printnum(7); else printnum(8);
  if (a <= 3.0d) printnum(9); else printnum(10);
  if (a == 3.14d) printnum(11); else printnum(12);
  if (a != 3.14d) printnum(13); else printnum(14);
  if (a != 3.0d) printnum(15); else printnum(16);
  if (!(a == 3.0d)) printnum(17); else printnum(18);
  if (!(a != 3.14d)) printnum(19); else printnum(20);
  double b = 3.14d;
  double c = 3.0d;
  if (a == b) printnum(21); else printnum(22);
  if (a == c) printnum(23); else printnum(24);
  if (a != c) printnum(25); else printnum(26);
  if (c < a) printnum(27); else printnum(28);
  int i = 0;
  while (a < 3.2d) { printnum(29); if (++i == 2) break; }
  do { printnum(30); } while (a > 3.2d);
  if (a < 3.0d || a > 3.1d) printnum(31); else printnum(32);
  if (a > 3.0d && a < 3.2d) printnum(33); else printnum(34);
}
