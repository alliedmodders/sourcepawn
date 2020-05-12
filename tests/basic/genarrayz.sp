#include <shell>

public main()
{
  test();
  test();
}

public test() {
  int[] arr = new int[2];
  int[][] arr2 = new int[2][2];

  for (int i = 0; i < 2; i++) {
    printnum(arr[i]);
    arr[i] = i + 1;
    for (int j = 0; j < 2; j++) {
      printnum(arr2[i][j]);
      arr2[i][j] = i * 2 + j + 1;
    }
  }
}
