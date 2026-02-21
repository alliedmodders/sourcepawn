#include <shell>

public void main() {
  int64 a = 100;
  int64 b = 1;
  int64 c = 3;

  a += 1;
  printnum64(a);
  a -= b;
  printnum64(a);
  a |= b;
  printnum64(a);
  a ^= b;
  printnum64(a);
  a &= 0x55555555;
  printnum64(a);
  a *= c;
  printnum64(a);
  a /= c;
  printnum64(a);
  a %= c;
  printnum64(a);

  a <<= 3;
  printnum64(a);

  a = -1;
  a >>= 2;
  printnum64(a);
  a >>>= 2;
  printnum64(a);
}
