#include <shell>

public main()
{
  int iv = 2000099;
  int64 lv = 1000000000007;
  intptr pv = 300;

  // int -> int64, int -> intptr
  printnum64(view_as<int64>(iv));
  printnumptr(view_as<intptr>(iv));

  // int64 -> int (truncate to 32-bit)
  printnum(view_as<int>(lv));

  // intptr -> int, intptr -> int64
  printnum(view_as<int>(pv));
  printnum64(view_as<int64>(pv));

  // int64 -> intptr via intermediate int (arch-independent low-32)
  printnum(view_as<int>(view_as<intptr>(lv)));

  // round-trips
  printnum64(view_as<int64>(view_as<int>(lv)));
  printnumptr(view_as<intptr>(view_as<int64>(iv)));
}
