// returnCode: 0
#include <shell>

#pragma dynamic 16384

// Callbacks attached to a forward may omit trailing parameters. The native
// pushes 11 arguments (including trailing arrays); the callback declares 6.
typedef ExtraArgsCallback = function int (int command, int buttons, const int angles[3],
                                          int impulse, const char name[16], int& result);
native int call_with_extra_args(ExtraArgsCallback cb, const int angles[3],
                                const char name[16], int& result);

int gCalls = 0;

int on_extra_args(int command, int buttons, const int angles[3], int impulse,
                  const char name[16], int& result) {
  gCalls++;
  assert_eq(command, 42);
  assert_eq(buttons, 7);
  assert_eq(angles[0], 11);
  assert_eq(angles[1], 22);
  assert_eq(angles[2], 33);
  assert_eq(impulse, 3);
  print(name);
  print("\n");
  result = 777;
  return 1234;
}

public void main() {
  int angles[3] = {11, 22, 33};
  char name[16] = "trailing";
  int result = 0;

  int rval = call_with_extra_args(on_extra_args, angles, name, result);
  assert_eq(rval, 1234);
  assert_eq(result, 777);
  assert_eq(gCalls, 1);

  print("OK\n");
}
