#include <shell>

public main()
{
  // view_as<intptr> from int16/int8 (bug: no cvt.intptr was emitted, producing
  // an invalid binary on 64-bit targets).
  int16 a = 5;
  intptr b = view_as<intptr>(a);
  printnumptr(b);                                        // 5

  int8 c = -3;
  printnumptr(view_as<intptr>(c));                       // -3

  // Implicit int16 -> intptr numeric coercion.
  int16 d = -1000;
  intptr e = d;
  printnumptr(e);                                        // -1000

  // Constant folding paths.
  printnumptr(view_as<intptr>(view_as<int16>(70000)));   // 4464
  printnumptr(view_as<intptr>(view_as<int8>(-1)));       // -1

  // Round-trip back to int16.
  printnum(view_as<int16>(b));                           // 5
}
