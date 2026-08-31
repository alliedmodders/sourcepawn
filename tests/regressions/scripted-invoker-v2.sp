// returnCode: 0
#include <shell>

#pragma dynamic 16384

typedef RefCallback = function void (int& val);
native void call_with_ref(RefCallback callback, int& val);

typedef Int64Callback = function void (int64 val);
native void call_with_int64(Int64Callback callback, int64 val);

typedef FlatArrayCallback = function void (const int array[4], int length);
native void call_with_flat_array(FlatArrayCallback callback, const int array[4], int length);

typedef FlatStringCallback = function void (const char str[32]);
native void call_with_flat_string(FlatStringCallback callback, const char str[32]);

void callback_ref(int& val) {
  assert_eq(val, 1337);
  val = 7331;
}

void callback_int64(int64 val) {
  printnum64(val);
}

void callback_flat_array(const int array[4], int length) {
  assert_eq(length, 4);
  assert_eq(array[0], 11);
  assert_eq(array[1], 22);
  assert_eq(array[2], 33);
  assert_eq(array[3], 44);
}

void callback_flat_string(const char str[32]) {
  print(str);
  print("\n");
}

public void main() {
  int val = 1337;
  call_with_ref(callback_ref, val);
  assert_eq(val, 7331);

  int64 big_val = 0x1122334455667788;
  call_with_int64(callback_int64, big_val);

  int array[4] = {11, 22, 33, 44};
  call_with_flat_array(callback_flat_array, array, 4);

  char str[32] = "Hello from flat string";
  call_with_flat_string(callback_flat_string, str);

  print("OK\n");
}
