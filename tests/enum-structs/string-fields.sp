// warnings_are_errors: true
#include <shell>

enum struct X {
  char message[20];
  int a;
}

public main() {
  X x;
  x.a = 1;
  for (int i = 0; i < sizeof(x.message) - 1; i++) {
    x.message[i] = 'a' + i;
  }
  printnum(X::a);
  printnum(x.a);
  print(x.message);
  print("\n");
}
