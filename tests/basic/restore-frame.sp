#include <shell>

void callback() {
  print("hello\n");
  report_error();
  print("world\n");
}

public int main() {
  int val = 10;

  execute(callback, 1);

  printnum(val);
}
