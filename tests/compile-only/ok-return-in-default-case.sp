// warnings_are_errors: true
int main() {
  funct(2);
  funcu(1);
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
