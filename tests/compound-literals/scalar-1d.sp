#include <shell>

int Array1[] = { 2, 3, 4, 5 };
int Array2[7] = { 3, 4, 5, 6, ... };
int Array3[10] = { 0, 0, 0, 1, ... };
int Array4[10] = { 0, 0, 0, ... };
int Array5[10] = { 0, ... };
int Array6[10] = { 20, 35, ... };

public main()
{
  printnums(sizeof(Array1), Array1[0], Array1[1], Array1[2], Array1[3]);
  printnums(sizeof(Array2), Array2[5]);
  printnums(sizeof(Array3), Array3[6]);
  printnums(sizeof(Array4), Array4[6]);
  printnums(sizeof(Array5), Array5[6]);
  printnums(sizeof(Array6), Array6[6]);
}
