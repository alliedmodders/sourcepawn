#include <shell>

bool do_not_call() {
  print("SHOULD NOT GET HERE!\n");
  return false;
}

public main() {
  bool a = true;
  bool b = false;
  bool c = false;
  bool d = true;
  printnum(a || b);
  printnum(a && b);
  printnum(b && do_not_call());
  printnum(a && b && do_not_call());
  printnum(a || do_not_call());
  printnum(c || a || do_not_call());
  printnum(a && d && b);
  printnum(c || b || d);
  printnum(c || d && a);
  printnum(d && b || c);
  printnum(true && false);
  printnum(true && true);
  printnum(false && false);
  printnum(true || false);
  printnum(true || true);
  printnum(false || false);
}
