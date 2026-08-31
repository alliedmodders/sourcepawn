// returnCode: 1
#include <shell>

int[] g;

void f(int[] x) {
  g = x;
}

public main() {
  int buf[10];
  f(buf);
}
