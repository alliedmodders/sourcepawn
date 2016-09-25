#include <shell>

char Array1[][] = { "hi\n", "bye\n", "A big fish jumped\n", "" };
char Array2[][25] = { "hi\n", "bye\n", "A big fish jumped\n", "" };
char Array3[5][] = { "a", "b", "c", "d", "e" };

public main()
{
  printnums(sizeof(Array1));
  print(Array1[2]);
  printnums(sizeof(Array2), sizeof(Array2[]));
  print(Array2[2]);
  printnums(sizeof(Array3), sizeof(Array3[]));
  print(Array3[0]);
  print(Array3[4]);
  print("\n");
}
