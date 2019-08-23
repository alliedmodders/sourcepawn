// warnings_are_errors: true
enum struct My_Struct {
  int size;

  void File() {
    this.size = fun(this.size);
  }
}

int fun(int value) {
  return value;
}

public main() {
}
