#include <shell>

public main() {
  // 1. Code that does NOT generate ARRAY_TO_NATIVE: flat array
  char flat[] = "hello";

  // Test offset 0 (aligned address)
  // Should return 'h' (104).
  printnum(test_local_to_array_ptr(flat, 0));

  // Test offset 1 (unaligned address)
  // Should return 'e' (101).
  printnum(test_local_to_array_ptr(flat[1], 0));

  // 2. Code that DOES generate ARRAY_TO_NATIVE: 2D array slice
  char multi[2][10];
  multi[0] = "abc";
  multi[1] = "def";

  // Test offset 0 on slice (which is a direct array SpArray)
  // Should return 'd' (100).
  printnum(test_local_to_array_ptr(multi[1], 0));
}
