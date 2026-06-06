// returnCode: 0
#include <shell>

#pragma dynamic 16384

void callback_array(const int[] array, int length) {
  assert_eq(length, 5000);
  assert_eq(array[0], 42);
  assert_eq(array[4999], 99);
}

void callback_string(char[] buffer, int length) {
  assert_eq(length, 5000);
  assert_eq(buffer[0], 'A');
  assert_eq(buffer[4998], 'B');
}

public void main() {
  int array[5000];
  array[0] = 42;
  array[4999] = 99;
  call_with_array(callback_array, array, 5000);

  char buffer[5000];
  buffer[0] = 'A';
  for (int i = 1; i < 4998; i++) {
    buffer[i] = ' ';
  }
  buffer[4998] = 'B';
  buffer[4999] = '\0';
  call_with_string(callback_string, buffer, 5000, SM_PARAM_STRING_UTF8 | SM_PARAM_STRING_COPY, SM_PARAM_COPYBACK);
  print("OK\n");
}
