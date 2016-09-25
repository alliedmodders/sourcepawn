#include <shell>

char Array1[][][] = {
  { "hello", "goodbye", "yams" },
  { "why", "sigh", "egg" },
  { "egg", "Egg", "EGG" },
  { "FOUUL DEMON", "Lemonade", "whaaaaaat" },
};

char Array2[4][3][] = {
  { "hello", "goodbye", "yams" },
  { "why", "sigh", "egg" },
  { "egg", "Egg", "EGG" },
  { "FOUUL DEMON", "Lemonade", "whaaaaaat" },
};

char Array3[][][25] = {
  { "hello", "goodbye", "yams" },
  { "why", "sigh", "egg" },
  { "egg", "Egg", "EGG" },
  { "FOUUL DEMON", "Lemonade", "whaaaaaat" },
};

public main()
{
  printnums(sizeof(Array1), sizeof(Array1[]), sizeof(Array1[][][]));
  print(Array1[0][2]);
  print("\n");
  print(Array1[3][2]);
  print("\n");
  printnum(Array1[3][2][3]);

  // Same as above.
  printnums(sizeof(Array2), sizeof(Array2[]), sizeof(Array2[][][]));
  print(Array2[0][2]);
  print("\n");
  print(Array2[3][2]);
  print("\n");
  printnum(Array2[3][2][3]);

  // Last dimension should be 25, but it is not. However we do get the
  // indirection vectors correct.
  printnums(sizeof(Array3), sizeof(Array3[]), sizeof(Array3[][][]));
  print(Array3[0][2]);
  print("\n");
  print(Array3[3][2]);
  print("\n");
  printnum(Array3[3][2][3]);
}
