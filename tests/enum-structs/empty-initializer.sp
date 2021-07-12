#include <shell>

enum struct Foo {
  int i;
  float f;
}

public main() {
  TestFunc();
  printnums(0xffffffff, 0xfffffffe, 0xfffffffd);
  TestFunc();
}

void TestFunc(Foo foo = {}) {
  printnum(foo.i);
  printfloat(foo.f);
}
