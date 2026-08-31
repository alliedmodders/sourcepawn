#include <shell>

enum struct Inner {
  int a;
}

enum struct Outer {
  Inner x;
}

public main() {
  printnum(sizeof(Outer::x));
}
