#include <shell>

int TestData[] = {
  5, 6, 7, 8, 9, 0, 1,
  1, 2, 3, 4, 5, 2, 3,
  2, 2, 2, 2, 2, 1, 4,
  7, 8, 1, 2, 3, 4, 5,
  9, 9, 8, 8, 1, 2, 2,
}

int TestData2[] = {
  5, 6,
  1, 2,
  2, 2,
  7, 8,
  9, 9,
}

public void main() {
  copy_2d_array_to_callback(TestData, 5, 7, callback);
  copy_2d_array_to_callback(TestData2, 5, 2, callback);
}

void callback(const int[][] array, int length, int stride) {
  for (int i = 0; i < length; i++) {
    for (int j = 0; j < stride; j++) {
      printnums(i, j, array[i][j]);
    }
  }
}
