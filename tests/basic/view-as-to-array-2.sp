#include <shell>

enum Handle: {};
enum Crab: {};

stock void WriteFile(Handle hndl, const int[] items, int num_items, int size)
{
  for (int i = 0; i < num_items; i++) {
    printnum(view_as<int>(items[i]));
  }
  print("--\n");
}

public void main() {
  Crab test[3] = { Crab:1, Crab:2, Crab:3 };
  float vec[3] = { 1.0, 2.0, 3.0 };
  Handle hFile;
  WriteFile(hFile, test, 3, 4); // warning: tag mismatch but works
  WriteFile(hFile, _:test, 3, 4); // All fine
  WriteFile(hFile, view_as<int>(vec), 3, 4);
  WriteFile(hFile, view_as<int>(test), 3, 4); // Writes random memory
}
