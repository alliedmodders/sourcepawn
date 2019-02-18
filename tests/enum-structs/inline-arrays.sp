#include <shell>

enum struct Sample {
  int x;
  int y;
  int data[5];
}

public main() {
  Sample s;
  for (int i = 0; i < 5; i++)
    s.data[i] = i * 10;
  for (int i = 0; i < 5; i++)
    printnum(s.data[i]);
}
