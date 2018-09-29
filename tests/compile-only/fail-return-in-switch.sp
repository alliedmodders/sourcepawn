// warnings_are_errors: true
int main() {
  funct(2);
  funcg(3);
}

native void donothing(int arg);

int funct(int input) {
  int egg = 2;
  switch (input) {
    case 0:
      return 1;
    case 1:
      return 2;
    case 5:
      egg = 3;
    default:
      return 3;
  }
  donothing(egg);
}

int funcg(int input) {
  if (input > 5) {
    return 2;
  }
  switch (input) {
  }
}
