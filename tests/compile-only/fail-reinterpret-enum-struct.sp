#include <shell>

enum struct X {
  int a[5];
  char message[20];
  float x;
  float y;
};

public main() {
  int a;
  printnum(view_as<X>(a));
  printnum(X:a);
}
