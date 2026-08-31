#include <shell>

enum struct Inner {
  int a;
}

enum struct Middle {
  Inner inner;
}

enum struct Outer {
  Middle middle;
}

public main() {
  Outer o;
  printnum(sizeof(o.middle.inner));
}
