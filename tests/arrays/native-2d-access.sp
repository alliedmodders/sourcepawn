#include <shell>

public main() {
  int array[10][10];
  array[3][5] = 100;
  int out;
  printnum(access_2d_array(array, 3, 5, out));
  printnum(out);
}
