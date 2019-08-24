#include <shell>

enum struct VecContainer {
  float vec[3];
}

DoesNotExist(const float vec[3]) {
  printfloat(vec[0]);
  printfloat(vec[1]);
  printfloat(vec[2]);
}

public main() {
  VecContainer vc;
  vc.vec[0] = 10.0;
  vc.vec[1] = 20.0;
  vc.vec[2] = 30.0;
  DoesNotExist(vc.vec);
}
