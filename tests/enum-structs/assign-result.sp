#include <shell>

enum struct Sample {
  int x;
  int y;
}

public main() {
  Sample a;
  Sample b;
  b.x = 11;
  b.y = 22;

  // The result of an enum struct assignment can be read.
  printnum((a = b).x);

  // The result of an enum struct assignment can be written to.
  (a = b).y = 99;
  printnum(a.y);

  a = b;
  printnum(a.x + a.y);

  int arr[2];
  int arr2[2];
  arr2[0] = 43;
  arr2[1] = 44;

  // The result of an array assignment can be read.
  printnum((arr = arr2)[1]);
  printnum(arr[0] + arr[1]);
}
