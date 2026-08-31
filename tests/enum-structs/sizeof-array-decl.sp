#include <shell>

enum struct MyStruct {
  int a;
}

public main() {
  any aFrameData[sizeof(MyStruct)];
  any aWriteData[sizeof(MyStruct) * 10];

  aFrameData[0] = 42;
  aWriteData[5] = 1337;

  printnum(aFrameData[0]);
  printnum(aWriteData[5]);
}
