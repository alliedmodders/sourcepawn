#include <shell>

enum struct MyStruct {
  int a;
  char message[20];
  float x;
}

void TestFunc(int size = sizeof(MyStruct)) {
  printnum(size);
}

public main() {
  TestFunc();
}
