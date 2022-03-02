#include <shell>

void callback(char[] buffer, int length) {
}

public main() {
  char buffer[] = "hello";
  call_with_string(callback, buffer, sizeof(buffer), SM_PARAM_STRING_UTF8, SM_PARAM_COPYBACK);
  printnum(buffer[0]);
}
