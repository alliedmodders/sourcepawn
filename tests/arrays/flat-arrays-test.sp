#include <shell>

// Global flat array
int g_arr[5] = { 10, 20, 30, 40, 50 };

public main() {
  // Local flat array
  int l_arr[5];

  // Testing global element load
  printnum(g_arr[0]); // 10
  printnum(g_arr[2]); // 30
  printnum(g_arr[4]); // 50

  // Testing global flat array element store
  g_arr[1] = 25;
  g_arr[3] = 45;
  printnum(g_arr[1]); // 25
  printnum(g_arr[3]); // 45

  // Testing local flat array element store and load
  l_arr[0] = 100;
  l_arr[1] = 200;
  l_arr[2] = 300;
  l_arr[3] = 400;
  l_arr[4] = 500;
  printnum(l_arr[0]); // 100
  printnum(l_arr[2]); // 300
  printnum(l_arr[4]); // 500

  // Testing flat array copy (OP_COPYARRAY / LL_COPYARRAY_FLAT)
  int l_arr2[5];
  l_arr2 = l_arr;
  printnum(l_arr2[1]); // 200
  printnum(l_arr2[3]); // 400

  // Testing flat array fill (OP_FILLARRAY / LL_FILLARRAY_FLAT)
  int l_arr3[5] = { 1, 2, ... };
  printnum(l_arr3[0]); // 1
  printnum(l_arr3[1]); // 2
  printnum(l_arr3[2]); // 3
  printnum(l_arr3[4]); // 5

  // Testing assigning a flat array to/from a nested fixed array element (x[5] = y)
  int nested_arr[10][3];
  int flat_src[3] = { 77, 88, 99 };
  nested_arr[5] = flat_src;
  printnum(nested_arr[5][0]); // 77
  printnum(nested_arr[5][1]); // 88
  printnum(nested_arr[5][2]); // 99

  int flat_dest[3];
  flat_dest = nested_arr[5];
  printnum(flat_dest[0]); // 77
  printnum(flat_dest[1]); // 88
  printnum(flat_dest[2]); // 99
}
