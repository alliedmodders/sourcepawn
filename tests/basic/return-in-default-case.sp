// warnings_are_errors: true
#include <shell>

public int main() {
  printnum(funct(2));
  printnum(funcu(1));
}

int funct(int input) {
  switch (input) {
    case 0:
      return 1;
    case 1:
      return 2;
    default:
      return 3;
  }
}

int funcu(int input) {
  switch (input) {
    default:
      return 2;
  }
}
