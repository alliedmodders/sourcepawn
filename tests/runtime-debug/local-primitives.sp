// debug_break_line: 10
#include <shell>

void f(int a, int b, int c) {
  bool somebool = false;
  if (somebool) {
  	float foo = 12.34;
  	printfloat(foo);
  }
  printnums(a, b, c);
}

public main() {
  f(12, 13, 14);
}
