#include <shell>

enum struct X {
  int a[5];
  char message[20];
  float x;
  float y;
}

public main() {
  printnum(sizeof(X::a));
  printnum(sizeof(X::message));
}
