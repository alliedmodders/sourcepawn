#include <shell>

enum struct Wide {
    int64 x;
    int64 y;
}

public main()
{
  // Local initializer with int64 fields (was ICE'ing before the fix).
  Wide w = {100, 200};
  printnum64(w.x);
  printnum64(w.y);

  // Array initializer with int64 fields (exercises ArrayValidator path).
  Wide arr[2] = {{1, 2}, {3, 4}};
  for (int i = 0; i < sizeof(arr); i++) {
    printnum64(arr[i].x);
    printnum64(arr[i].y);
  }
}
