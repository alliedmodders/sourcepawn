#include <shell>

int Array1[][] = { {1}, {2, 3}, {4, 5, 6} };
int Array2[][] = { {1, 2,}, {3, 4}, {5, 6} };
int Array3[3][] = { {7, 8}, {9, 1}, {2, 3} };
int Array4[3][] = { {1}, {2, 3}, {4, 5, 6} };
int Array5[][3] = { {1, 2, 3}, {2, 3, 4}, {4, 5, 6} };

public main()
{
  printnums(sizeof(Array1), Array1[0][0], Array1[1][1], Array1[2][2]);
  printnums(sizeof(Array2), sizeof(Array2[]), Array2[0][1], Array2[1][1]);
  printnums(sizeof(Array3), sizeof(Array3[]), Array3[1][0], Array3[2][1]);
  printnums(sizeof(Array4), Array4[0][0], Array4[1][1], Array4[2][2]);
  printnums(sizeof(Array5), sizeof(Array5[]), Array5[0][0], Array5[1][1], Array5[2][2]);
}
