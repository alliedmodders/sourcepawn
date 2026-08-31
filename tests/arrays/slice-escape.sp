// returnCode: 1
#include <shell>

int[] g = null;

void f(int[] x) {
  g = x;
}

public main() {
  int buf[10];
  f(buf);
}
