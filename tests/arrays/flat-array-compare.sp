#include <shell>

int g_a[3] = { 1, 2, 3 };
int g_b[3] = { 1, 2, 3 };

public void main() {
  int a[3];
  int b[3];

  // Flat arrays compare by address.
  printnum(a == b); // 0
  printnum(a == a); // 1
  printnum(a != b); // 1
  printnum(g_a == g_b); // 0
  printnum(g_a == g_a); // 1

  a[0] = 7;
  b[0] = 7;
  printnum(a == b); // 0
}
