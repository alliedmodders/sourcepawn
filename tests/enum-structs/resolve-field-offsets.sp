#include <shell>

enum struct X {
  int a[5];
  char message[20];
  float x;
  float y;
}

public main() {
  printnum(X::a);
  printnum(X::message);
  printnum(X::x);
  printnum(X::y);
}
