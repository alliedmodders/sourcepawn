#include <shell>

// :TODO: this asserts in the compiler 
// int Array1[][][] = { {1}, {2, 3}, {4, 5, 6} };
// :TODO: this should not compile
// int Array1[][][] = { { {1}, {2, 3}, {4, 5, 6} }, {2, 3} };
int Array1[][][] = {
 {
  {1, 2, 3},
  {4, 5, 6},
 }, {
  {8, 9, 1},
  {2, 3, 4},
 }
};

int Array2[3][2][] = {
 {
  {1, 2, 3},
  {4, 5, 6},
 }, {
  {8, 9, 1},
  {2, 3, 4},
 }, {
  {1, 6, 9},
  {2, 7, 3},
 }
};

public main()
{
  printnums(sizeof(Array1), sizeof(Array1[]), sizeof(Array1[][]),
            Array1[0][1][2], Array1[0][1][1], Array1[0][0][0]);
  printnums(sizeof(Array2), sizeof(Array2[]), sizeof(Array2[][]),
            Array2[0][1][2], Array2[0][1][1], Array2[0][0][0],
            Array2[2][1][2], Array2[1][1][1], Array2[1][0][0]);
}
