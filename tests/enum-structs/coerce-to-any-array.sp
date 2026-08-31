#include <shell>

enum struct Sample {
  int a;
  float b;
  int c;
}

public main() {
  Sample s;
  s.a = 123;
  s.b = 456.0;
  s.c = 789;
  
  print_cells(s);
}

void print_cells(const any[] array) {
  printnum(array[Sample::a]);
  printfloat(view_as<float>(array[Sample::b]));
  printnum(array[Sample::c]);
}
