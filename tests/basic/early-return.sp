#include <shell>

void inner() {
  print("inner.1\n");
  return;
  print("inner.2\n");
  return;
  print("inner.3\n");
}

public void main() {
  print("main.1\n");
  inner();
  print("main.2\n");
  return;
  print("main.3\n");
  inner();
  print("main.4\n");
  return;
  print("main.5\n");
  inner();
  print("main.6\n");
}
