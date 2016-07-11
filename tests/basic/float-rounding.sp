#include <shell>

public main()
{
  float sequence[] = {
    -2147400000.0,
    -2147400000.5,
    -3.6, -3.5, -3.0, -2.5, -2.0, -1.5, -1.0, -0.5, 0.0,
    0.3, 0.5, 0.7, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 3.6,
    2147400000.0,
    2147400000.5,
  };

  print("RoundToZero:\n");
  for (int i = 0; i < sizeof(sequence); i++) {
    writefloat(sequence[i]);
    print(" => ");
    printnum(RoundToZero(sequence[i]));
  }

  print("RoundToCeil:\n");
  for (int i = 0; i < sizeof(sequence); i++) {
    writefloat(sequence[i]);
    print(" => ");
    printnum(RoundToCeil(sequence[i]));
  }

  print("RoundToFloor:\n");
  for (int i = 0; i < sizeof(sequence); i++) {
    writefloat(sequence[i]);
    print(" => ");
    printnum(RoundToFloor(sequence[i]));
  }

  print("RoundToNearest:\n");
  for (int i = 0; i < sizeof(sequence); i++) {
    writefloat(sequence[i]);
    print(" => ");
    printnum(RoundToNearest(sequence[i]));
  }
}
